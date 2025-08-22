#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include <llvm-c/Core.h>
#include <llvm-c/Analysis.h>
#include <llvm-c/Target.h>
#include <llvm-c/TargetMachine.h>

// ----- Lexer -----
typedef enum {
    TOK_LPAREN,
    TOK_RPAREN,
    TOK_NUMBER,
    TOK_SYMBOL,
    TOK_STRING,
    TOK_EOF,
    TOK_ERROR
} TokenKind;

typedef struct {
    TokenKind kind;
    const char *start; // pointer into input
    size_t len;
    long long number;  // valid if kind == TOK_NUMBER
    size_t pos;        // absolute offset in input
} Token;

typedef struct {
    const char *src;
    size_t len;
    size_t i;
} Lexer;

static void lex_init(Lexer *L, const char *src) {
    L->src = src;
    L->len = strlen(src);
    L->i = 0;
}

static int is_sym_char(int c) {
    return isalnum((unsigned char)c) || strchr("+-*/<>=!?_:.", c) != NULL;
}

static void skip_ws(Lexer *L) {
    while (L->i < L->len) {
        char c = L->src[L->i];
        if (isspace((unsigned char)c)) { L->i++; continue; }
        // Simple line comment ; ... end
        if (c == ';') { while (L->i < L->len && L->src[L->i] != '\n') L->i++; continue; }
        break;
    }
}

static Token lex_next(Lexer *L) {
    skip_ws(L);
    Token t = {0};
    t.pos = L->i;
    if (L->i >= L->len) { t.kind = TOK_EOF; return t; }
    char c = L->src[L->i];
    if (c == '(') { L->i++; t.kind = TOK_LPAREN; t.start = &L->src[t.pos]; t.len = 1; return t; }
    if (c == ')') { L->i++; t.kind = TOK_RPAREN; t.start = &L->src[t.pos]; t.len = 1; return t; }

    // string literal: "..." with simple escapes
    if (c == '"') {
        size_t j = ++L->i; // skip opening quote
        int esc = 0;
        while (j < L->len) {
            char d = L->src[j++];
            if (esc) { esc = 0; continue; }
            if (d == '\\') { esc = 1; continue; }
            if (d == '"') { break; }
        }
        if (j > L->len || L->src[j-1] != '"') {
            t.kind = TOK_ERROR;
            t.start = &L->src[t.pos];
            t.len = 1;
            return t;
        }
        t.kind = TOK_STRING;
        t.start = &L->src[L->i];
        t.len = (j - 1) - L->i; // exclude closing quote
        L->i = j; // past closing quote
        return t;
    }

    // number: optional leading '-', then digits
    if (c == '-' || isdigit((unsigned char)c)) {
        size_t j = L->i;
        if (c == '-') j++;
        if (j < L->len && isdigit((unsigned char)L->src[j])) {
            j++;
            while (j < L->len && isdigit((unsigned char)L->src[j])) j++;
            t.kind = TOK_NUMBER;
            t.start = &L->src[L->i];
            t.len = j - L->i;
            char buf[64];
            size_t n = t.len < sizeof(buf) - 1 ? t.len : sizeof(buf) - 1;
            memcpy(buf, t.start, n);
            buf[n] = '\0';
            t.number = atoll(buf);
            L->i = j;
            return t;
        }
    }

    // symbol
    if (is_sym_char((unsigned char)c)) {
        size_t j = L->i + 1;
        while (j < L->len && is_sym_char((unsigned char)L->src[j])) j++;
        t.kind = TOK_SYMBOL;
        t.start = &L->src[L->i];
        t.len = j - L->i;
        L->i = j;
        return t;
    }

    t.kind = TOK_ERROR;
    t.start = &L->src[L->i];
    t.len = 1;
    L->i++;
    return t;
}

// ----- Parser AST -----
typedef enum {
    AST_NUMBER,
    AST_STRING,
    AST_CALL
} AstKind;

typedef struct Ast Ast;

typedef struct {
    char *name;   // function/operator name (e.g., "+", "printf")
    size_t arity; // number of arguments
    Ast **args;   // dynamic array
} AstCall;

struct Ast {
    AstKind kind;
    union {
        long long number; // AST_NUMBER
        char *string;     // AST_STRING
        AstCall call;     // AST_CALL
    } v;
};

typedef struct {
    Lexer L;
    Token cur;
    int had_error;
    char err_msg[256];
    size_t err_pos;
    // extern prototypes collected at parse time
    struct Proto *protos;
    size_t n_protos, cap_protos;
} Parser;

// ---- Types and prototypes ----
typedef enum {
    TY_I64,
    TY_I32,
    TY_I8PTR,
    TY_VOID
} TyKind;

typedef struct Proto {
    char *name;
    TyKind ret;
    TyKind *params;
    size_t n_params;
    int is_vararg;
} Proto;

static void parser_init(Parser *P, const char *src) {
    memset(P, 0, sizeof(*P));
    lex_init(&P->L, src);
    P->cur = lex_next(&P->L);
}

static void parser_error(Parser *P, const char *msg, size_t pos) {
    P->had_error = 1;
    snprintf(P->err_msg, sizeof(P->err_msg), "%s", msg);
    P->err_pos = pos;
}

static void advance(Parser *P) { P->cur = lex_next(&P->L); }

static int tok_is(Parser *P, TokenKind k) { return P->cur.kind == k; }

static int tok_is_sym(Parser *P, const char *s) {
    if (P->cur.kind != TOK_SYMBOL) return 0;
    size_t n = strlen(s);
    return P->cur.len == n && strncmp(P->cur.start, s, n) == 0;
}

static char *copy_lexeme(const char *start, size_t len) {
    char *s = (char *)malloc(len + 1);
    memcpy(s, start, len);
    s[len] = '\0';
    return s;
}

static Ast *ast_number(long long v) {
    Ast *a = (Ast *)calloc(1, sizeof(Ast));
    a->kind = AST_NUMBER;
    a->v.number = v;
    return a;
}

static Ast *ast_string(const char *start, size_t len) {
    Ast *a = (Ast *)calloc(1, sizeof(Ast));
    a->kind = AST_STRING;
    // unescape into newly allocated C string
    char *buf = (char *)malloc(len + 1);
    size_t bi = 0;
    for (size_t i = 0; i < len; i++) {
        char c = start[i];
        if (c == '\\' && i + 1 < len) {
            char n = start[++i];
            switch (n) {
                case 'n': buf[bi++] = '\n'; break;
                case 't': buf[bi++] = '\t'; break;
                case 'r': buf[bi++] = '\r'; break;
                case '\\': buf[bi++] = '\\'; break;
                case '"': buf[bi++] = '"'; break;
                case '0': buf[bi++] = '\0'; break;
                default: buf[bi++] = n; break;
            }
        } else {
            buf[bi++] = c;
        }
    }
    buf[bi] = '\0';
    a->v.string = buf;
    return a;
}

static Ast *ast_call(const char *name_start, size_t name_len, size_t arity, Ast **args) {
    Ast *a = (Ast *)calloc(1, sizeof(Ast));
    a->kind = AST_CALL;
    a->v.call.name = (char *)malloc(name_len + 1);
    memcpy(a->v.call.name, name_start, name_len);
    a->v.call.name[name_len] = '\0';
    a->v.call.arity = arity;
    a->v.call.args = args;
    return a;
}

static void ast_free_rec(Ast *a) {
    if (!a) return;
    if (a->kind == AST_CALL) {
        for (size_t i = 0; i < a->v.call.arity; i++) ast_free_rec(a->v.call.args[i]);
        free(a->v.call.args);
        free(a->v.call.name);
    } else if (a->kind == AST_STRING) {
        free(a->v.string);
    }
    free(a);
}

static int is_op_token(Token t) {
    return t.kind == TOK_SYMBOL && t.len == 1 && strchr("+-*/", t.start[0]) != NULL;
}

static Ast *parse_expr(Parser *P);

static int parse_type_sym(Parser *P, TyKind *out) {
    if (P->cur.kind != TOK_SYMBOL) {
        parser_error(P, "expected type symbol", P->cur.pos);
        return 0;
    }
    const char *s = P->cur.start; size_t n = P->cur.len;
    if (n == 3 && strncmp(s, "i64", 3) == 0) { *out = TY_I64; advance(P); return 1; }
    if (n == 3 && strncmp(s, "i32", 3) == 0) { *out = TY_I32; advance(P); return 1; }
    if (n == 3 && strncmp(s, "i8*", 3) == 0) { *out = TY_I8PTR; advance(P); return 1; }
    if (n == 4 && strncmp(s, "void", 4) == 0) { *out = TY_VOID; advance(P); return 1; }
    parser_error(P, "unknown type", P->cur.pos);
    return 0;
}

static void parser_add_proto(Parser *P, Proto *pr) {
    if (P->n_protos == P->cap_protos) {
        P->cap_protos = P->cap_protos ? P->cap_protos * 2 : 4;
        P->protos = (Proto *)realloc(P->protos, P->cap_protos * sizeof(Proto));
    }
    P->protos[P->n_protos++] = *pr; // struct copy
}

static int parse_extern(Parser *P) {
    // Current token is 'extern' symbol; lparen was consumed.
    advance(P); // consume 'extern'
    if (P->cur.kind != TOK_SYMBOL) { parser_error(P, "expected function name after extern", P->cur.pos); return 0; }
    char *name = copy_lexeme(P->cur.start, P->cur.len);
    advance(P);
    if (P->cur.kind != TOK_LPAREN) { parser_error(P, "expected ( to start parameter list", P->cur.pos); free(name); return 0; }
    advance(P); // consume '('
    // Parse 0+ type symbols, optional '...' at end
    TyKind *params = NULL; size_t n = 0, cap = 0; int vararg = 0;
    while (P->cur.kind != TOK_RPAREN && P->cur.kind != TOK_EOF && !P->had_error) {
        if (P->cur.kind == TOK_SYMBOL && P->cur.len == 3 && strncmp(P->cur.start, "...", 3) == 0) {
            vararg = 1;
            advance(P);
            break;
        }
        TyKind tk;
        if (!parse_type_sym(P, &tk)) { free(name); free(params); return 0; }
        if (n == cap) { cap = cap ? cap * 2 : 4; params = (TyKind *)realloc(params, cap * sizeof(TyKind)); }
        params[n++] = tk;
    }
    if (P->cur.kind != TOK_RPAREN) { parser_error(P, "expected ) after parameter list", P->cur.pos); free(name); free(params); return 0; }
    advance(P); // consume ')'
    // Return type
    TyKind ret;
    if (!parse_type_sym(P, &ret)) { free(name); free(params); return 0; }
    if (P->cur.kind != TOK_RPAREN) { parser_error(P, "expected ) to close extern", P->cur.pos); free(name); free(params); return 0; }
    advance(P); // consume ')'
    Proto pr = {0}; pr.name = name; pr.ret = ret; pr.params = NULL; pr.n_params = n; pr.is_vararg = vararg;
    if (n) { pr.params = (TyKind *)malloc(n * sizeof(TyKind)); memcpy(pr.params, params, n * sizeof(TyKind)); }
    free(params);
    parser_add_proto(P, &pr);
    return 1;
}

static Ast *parse_list_after_lparen(Parser *P) {
    // Current token is the operator/callee symbol
    if (P->cur.kind != TOK_SYMBOL) {
        parser_error(P, "expected symbol after (", P->cur.pos);
        return NULL;
    }
    const char *name_start = P->cur.start;
    size_t name_len = P->cur.len;
    advance(P); // consume symbol
    // Parse 1+ expressions until ')'
    size_t cap = 4, n = 0;
    Ast **args = (Ast **)malloc(cap * sizeof(Ast *));
    while (P->cur.kind != TOK_RPAREN && P->cur.kind != TOK_EOF && !P->had_error) {
        if (n == cap) { cap *= 2; args = (Ast **)realloc(args, cap * sizeof(Ast *)); }
        Ast *e = parse_expr(P);
        if (!e) {
            for (size_t i = 0; i < n; i++) ast_free_rec(args[i]);
            free(args);
            return NULL;
        }
        args[n++] = e;
    }
    if (P->cur.kind != TOK_RPAREN) {
        parser_error(P, "expected ) to close list", P->cur.pos);
        for (size_t i = 0; i < n; i++) ast_free_rec(args[i]);
        free(args);
        return NULL;
    }
    advance(P); // consume ')'
    if (n == 0) {
        parser_error(P, "operator requires at least 1 operand", P->cur.pos);
        return NULL;
    }
    return ast_call(name_start, name_len, n, args);
}

static Ast *parse_list(Parser *P) {
    // current token is '('
    size_t lparen_pos = P->cur.pos;
    advance(P); // consume '('
    // Special form: extern
    if (P->cur.kind == TOK_SYMBOL && P->cur.len == 6 && strncmp(P->cur.start, "extern", 6) == 0) {
        if (!parse_extern(P)) return NULL;
        // Return a dummy number 0 expression; caller may discard
        return ast_number(0);
    }
    // Generic list
    // Reset position for error messages if needed
    (void)lparen_pos;
    return parse_list_after_lparen(P);
}

static Ast *parse_expr(Parser *P) {
    if (P->cur.kind == TOK_NUMBER) {
        long long v = P->cur.number;
        advance(P);
        return ast_number(v);
    } else if (P->cur.kind == TOK_STRING) {
        const char *s = P->cur.start;
        size_t sl = P->cur.len;
        advance(P);
        return ast_string(s, sl);
    } else if (P->cur.kind == TOK_LPAREN) {
        return parse_list(P);
    } else {
        parser_error(P, "expected number or (", P->cur.pos);
        return NULL;
    }
}

// ----- Codegen (LLVM) -----
typedef struct {
    LLVMContextRef ctx;
    LLVMModuleRef module;
    LLVMBuilderRef builder;
    LLVMTypeRef i64;
    LLVMTypeRef i8ptr;
    Proto *protos;
    size_t n_protos;
} CG;

static LLVMTypeRef cg_type_for(CG *cg, TyKind tk) {
    switch (tk) {
        case TY_I64: return cg->i64;
        case TY_I32: return LLVMInt32TypeInContext(cg->ctx);
        case TY_I8PTR: return cg->i8ptr;
        case TY_VOID: return LLVMVoidTypeInContext(cg->ctx);
    }
    return LLVMInt32TypeInContext(cg->ctx);
}

static void cg_init(CG *cg, const char *module_name) {
    memset(cg, 0, sizeof(*cg));
    cg->ctx = LLVMContextCreate();
    cg->module = LLVMModuleCreateWithNameInContext(module_name, cg->ctx);
    cg->builder = LLVMCreateBuilderInContext(cg->ctx);
    cg->i64 = LLVMInt64TypeInContext(cg->ctx);
    cg->i8ptr = LLVMPointerType(LLVMInt8TypeInContext(cg->ctx), 0);
}

static void cg_dispose(CG *cg) {
    LLVMDisposeBuilder(cg->builder);
    LLVMDisposeModule(cg->module);
    LLVMContextDispose(cg->ctx);
}

static void cg_set_protos(CG *cg, Proto *ps, size_t n) {
    // shallow copy of array entries, deep copy strings/params
    cg->protos = (Proto *)calloc(n, sizeof(Proto));
    cg->n_protos = n;
    for (size_t i = 0; i < n; i++) {
        cg->protos[i].name = strdup(ps[i].name);
        cg->protos[i].ret = ps[i].ret;
        cg->protos[i].n_params = ps[i].n_params;
        cg->protos[i].is_vararg = ps[i].is_vararg;
        if (ps[i].n_params) {
            cg->protos[i].params = (TyKind *)malloc(ps[i].n_params * sizeof(TyKind));
            memcpy(cg->protos[i].params, ps[i].params, ps[i].n_params * sizeof(TyKind));
        }
    }
}

static void cg_free_protos(CG *cg) {
    if (!cg->protos) return;
    for (size_t i = 0; i < cg->n_protos; i++) {
        free(cg->protos[i].name);
        free(cg->protos[i].params);
    }
    free(cg->protos);
    cg->protos = NULL;
    cg->n_protos = 0;
}

static LLVMValueRef codegen_expr(CG *cg, Ast *a) {
    if (a->kind == AST_NUMBER) {
        return LLVMConstInt(cg->i64, (unsigned long long)a->v.number, 1);
    } else if (a->kind == AST_STRING) {
        return LLVMBuildGlobalStringPtr(cg->builder, a->v.string, "str");
    }
    // AST_CALL
    const char *name = a->v.call.name;
    // Built-in arithmetic: + - * /
    if (a->v.call.arity == 0) return LLVMConstInt(cg->i64, 0, 0);
    if (strlen(name) == 1 && strchr("+-*/", name[0])) {
        char op = name[0];
        LLVMValueRef acc = NULL;
        for (size_t i = 0; i < a->v.call.arity; i++) {
            LLVMValueRef val = codegen_expr(cg, a->v.call.args[i]);
            if (!acc) {
                if (a->v.call.arity == 1) {
                    switch (op) {
                        case '+': return val;
                        case '-': return LLVMBuildNeg(cg->builder, val, "neg");
                        case '*': return val;
                        case '/': return val;
                    }
                }
                acc = val;
                continue;
            }
            switch (op) {
                case '+': acc = LLVMBuildAdd(cg->builder, acc, val, "add"); break;
                case '-': acc = LLVMBuildSub(cg->builder, acc, val, "sub"); break;
                case '*': acc = LLVMBuildMul(cg->builder, acc, val, "mul"); break;
                case '/': acc = LLVMBuildSDiv(cg->builder, acc, val, "div"); break;
                default: acc = val; break;
            }
        }
        return acc ? acc : LLVMConstInt(cg->i64, 0, 0);
    }

    // Generic external C call, using declared prototypes if available.
    size_t n = a->v.call.arity;
    LLVMValueRef *args = (LLVMValueRef *)alloca(sizeof(LLVMValueRef) * n);
    LLVMTypeRef *param_tys = (LLVMTypeRef *)alloca(sizeof(LLVMTypeRef) * (n > 0 ? n : 1));
    for (size_t i = 0; i < n; i++) {
        args[i] = codegen_expr(cg, a->v.call.args[i]);
        param_tys[i] = LLVMTypeOf(args[i]);
    }

    // Lookup declared prototype
    Proto *decl = NULL;
    for (size_t i = 0; i < cg->n_protos; i++) {
        if (strcmp(cg->protos[i].name, name) == 0) { decl = &cg->protos[i]; break; }
    }

    LLVMTypeRef ret_i32 = LLVMInt32TypeInContext(cg->ctx);

    if (decl) {
        // Cast fixed arguments to expected types
        size_t m = decl->n_params < n ? decl->n_params : n;
        for (size_t i = 0; i < m; i++) {
            LLVMTypeRef want = cg_type_for(cg, decl->params[i]);
            LLVMTypeRef have = LLVMTypeOf(args[i]);
            if (want != have) {
                if (want == cg->i64 && have == ret_i32) {
                    args[i] = LLVMBuildSExtOrBitCast(cg->builder, args[i], cg->i64, "sext");
                } else if (want == ret_i32 && have == cg->i64) {
                    args[i] = LLVMBuildTruncOrBitCast(cg->builder, args[i], ret_i32, "trunc");
                } else if (want == cg->i8ptr && LLVMGetTypeKind(have) == LLVMIntegerTypeKind) {
                    args[i] = LLVMBuildIntToPtr(cg->builder, args[i], cg->i8ptr, "inttoptr");
                } else if (LLVMGetTypeKind(want) == LLVMIntegerTypeKind && LLVMGetTypeKind(have) == LLVMPointerTypeKind) {
                    LLVMTypeRef want_int = want;
                    LLVMValueRef v = LLVMBuildPtrToInt(cg->builder, args[i], cg->i64, "ptrtoint");
                    if (want_int == ret_i32) v = LLVMBuildTruncOrBitCast(cg->builder, v, ret_i32, "trunc");
                    args[i] = v;
                } else {
                    args[i] = LLVMBuildBitCast(cg->builder, args[i], want, "cast");
                }
            }
        }
        // Build function type
        LLVMTypeRef *fixed = NULL;
        if (decl->n_params) {
            fixed = (LLVMTypeRef *)alloca(sizeof(LLVMTypeRef) * decl->n_params);
            for (size_t i = 0; i < decl->n_params; i++) fixed[i] = cg_type_for(cg, decl->params[i]);
        }
        LLVMTypeRef fn_ret = cg_type_for(cg, decl->ret);
        LLVMTypeRef fn_ty = LLVMFunctionType(fn_ret, fixed, (unsigned)decl->n_params, decl->is_vararg);
        LLVMValueRef callee = LLVMGetNamedFunction(cg->module, name);
        if (!callee) {
            callee = LLVMAddFunction(cg->module, name, fn_ty);
            LLVMSetLinkage(callee, LLVMExternalLinkage);
        }
        LLVMValueRef callv = LLVMBuildCall2(cg->builder, fn_ty, callee, args, (unsigned)n, "call");
        // Normalize return to i64
        if (decl->ret == TY_I64) return callv;
        if (decl->ret == TY_I32) return LLVMBuildSExtOrBitCast(cg->builder, callv, cg->i64, "ret64");
        if (decl->ret == TY_I8PTR) {
            LLVMValueRef v = LLVMBuildPtrToInt(cg->builder, callv, cg->i64, "ptrtoint");
            return v;
        }
        // void
        return LLVMConstInt(cg->i64, 0, 0);
    }

    // Fallback heuristic
    int isVarArg = 0;
    unsigned fixed_params = n;
    if (n >= 2 && param_tys[0] == cg->i8ptr) { isVarArg = 1; fixed_params = 1; }
    LLVMTypeRef fn_ty = LLVMFunctionType(ret_i32, param_tys, fixed_params, isVarArg);
    LLVMValueRef callee = LLVMGetNamedFunction(cg->module, name);
    if (!callee) { callee = LLVMAddFunction(cg->module, name, fn_ty); LLVMSetLinkage(callee, LLVMExternalLinkage); }
    LLVMValueRef callv = LLVMBuildCall2(cg->builder, fn_ty, callee, args, (unsigned)n, "call");
    return LLVMBuildSExtOrBitCast(cg->builder, callv, cg->i64, "ret64");
}

static int compile_to_ir(const char *src, const char *out_path) {
    // Parse
    Parser P; parser_init(&P, src);
    Ast *root = NULL;
    // Parse multiple top-level forms: extern decls and one expression
    while (!P.had_error && P.cur.kind != TOK_EOF) {
        if (P.cur.kind == TOK_LPAREN) {
            // Peek: if extern, parse_extern via parse_list
            Ast *a = parse_list(&P);
            if (!a) break;
            // If it was extern, parse_list returns number 0; ignore those.
            if (a->kind != AST_NUMBER || a->v.number != 0) {
                if (root) ast_free_rec(root);
                root = a;
            } else {
                ast_free_rec(a);
            }
        } else if (P.cur.kind == TOK_NUMBER || P.cur.kind == TOK_STRING) {
            Ast *a = parse_expr(&P);
            if (!a) break;
            if (root) ast_free_rec(root);
            root = a;
        } else {
            parser_error(&P, "unexpected token at top-level", P.cur.pos);
            break;
        }
    }
    if (P.had_error || !root) {
        fprintf(stderr, "Parse error at offset %zu: %s\n", P.err_pos, P.err_msg[0] ? P.err_msg : "unknown error");
        if (root) ast_free_rec(root);
        return 1;
    }

    // Codegen
    CG cg; cg_init(&cg, "lispc_module");
    // Predeclare extern prototypes in module
    if (P.n_protos) {
        cg_set_protos(&cg, P.protos, P.n_protos);
        LLVMTypeRef i32 = LLVMInt32TypeInContext(cg.ctx);
        for (size_t i = 0; i < P.n_protos; i++) {
            Proto *pr = &P.protos[i];
            // Build function type
            LLVMTypeRef ret_ty = (pr->ret == TY_I64) ? cg.i64 : (pr->ret == TY_I32) ? i32 : (pr->ret == TY_I8PTR) ? cg.i8ptr : LLVMVoidTypeInContext(cg.ctx);
            LLVMTypeRef *fixed = NULL;
            if (pr->n_params) {
                fixed = (LLVMTypeRef *)alloca(sizeof(LLVMTypeRef) * pr->n_params);
                for (size_t j = 0; j < pr->n_params; j++) {
                    TyKind tk = pr->params[j];
                    fixed[j] = (tk == TY_I64) ? cg.i64 : (tk == TY_I32) ? i32 : (tk == TY_I8PTR) ? cg.i8ptr : LLVMVoidTypeInContext(cg.ctx);
                }
            }
            LLVMTypeRef fnty = LLVMFunctionType(ret_ty, fixed, (unsigned)pr->n_params, pr->is_vararg);
            if (!LLVMGetNamedFunction(cg.module, pr->name)) {
                LLVMValueRef fn = LLVMAddFunction(cg.module, pr->name, fnty);
                LLVMSetLinkage(fn, LLVMExternalLinkage);
            }
        }
    }
    // Create lisp_main(): i64
    LLVMTypeRef fn_type = LLVMFunctionType(cg.i64, NULL, 0, 0);
    LLVMValueRef lisp_main = LLVMAddFunction(cg.module, "lisp_main", fn_type);
    LLVMBasicBlockRef entry = LLVMAppendBasicBlockInContext(cg.ctx, lisp_main, "entry");
    LLVMPositionBuilderAtEnd(cg.builder, entry);
    LLVMValueRef result = codegen_expr(&cg, root);
    LLVMBuildRet(cg.builder, result);

    // Optionally, also provide an i32 main() that calls lisp_main and truncates to i32
    LLVMTypeRef i32 = LLVMInt32TypeInContext(cg.ctx);
    LLVMTypeRef main_ty = LLVMFunctionType(i32, NULL, 0, 0);
    LLVMValueRef c_main = LLVMAddFunction(cg.module, "main", main_ty);
    LLVMBasicBlockRef mentry = LLVMAppendBasicBlockInContext(cg.ctx, c_main, "entry");
    LLVMPositionBuilderAtEnd(cg.builder, mentry);
    LLVMValueRef call_res = LLVMBuildCall2(cg.builder, fn_type, lisp_main, NULL, 0, "res");
    LLVMValueRef trunc = LLVMBuildTruncOrBitCast(cg.builder, call_res, i32, "ret32");
    LLVMBuildRet(cg.builder, trunc);

    // Validate
    char *err = NULL;
    if (LLVMVerifyModule(cg.module, LLVMAbortProcessAction, &err)) {
        fprintf(stderr, "LLVM module verification failed: %s\n", err ? err : "");
        LLVMDisposeMessage(err);
        ast_free_rec(root);
        cg_dispose(&cg);
        return 1;
    }
    LLVMDisposeMessage(err);

    // Write .ll IR
    if (out_path == NULL) out_path = "out.ll";
    char *err_msg = NULL;
    if (LLVMPrintModuleToFile(cg.module, out_path, &err_msg) != 0) {
        fprintf(stderr, "Failed to write IR to %s: %s\n", out_path, err_msg ? err_msg : "");
        LLVMDisposeMessage(err_msg);
        ast_free_rec(root);
        cg_dispose(&cg);
        return 1;
    }
    LLVMDisposeMessage(err_msg);

    ast_free_rec(root);
    cg_free_protos(&cg);
    cg_dispose(&cg);
    return 0;
}

typedef struct {
    char *exe_path;
    char *obj_path;
} BuildOut;

static int emit_object_and_link(LLVMModuleRef module, BuildOut *out) {
    // Initialize native target for codegen and asm printer
    if (LLVMInitializeNativeTarget() != 0) {
        fprintf(stderr, "Failed to initialize native target\n");
        return 1;
    }
    if (LLVMInitializeNativeAsmPrinter() != 0) {
        fprintf(stderr, "Failed to initialize native asm printer\n");
        return 1;
    }

    char *triple = LLVMGetDefaultTargetTriple();
    LLVMSetTarget(module, triple);

    LLVMTargetRef target;
    char *err = NULL;
    if (LLVMGetTargetFromTriple(triple, &target, &err) != 0) {
        fprintf(stderr, "LLVMGetTargetFromTriple failed: %s\n", err ? err : "");
        LLVMDisposeMessage(err);
        LLVMDisposeMessage(triple);
        return 1;
    }

    char *cpu = LLVMGetHostCPUName();
    char *features = LLVMGetHostCPUFeatures();
    LLVMTargetMachineRef tm = LLVMCreateTargetMachine(
        target,
        triple,
        cpu,
        features,
        LLVMCodeGenLevelDefault,
        LLVMRelocDefault,
        LLVMCodeModelDefault
    );
    LLVMDisposeMessage(cpu);
    LLVMDisposeMessage(features);
    if (!tm) {
        fprintf(stderr, "Failed to create target machine\n");
        LLVMDisposeMessage(triple);
        return 1;
    }

    // Set data layout
    LLVMTargetDataRef dl = LLVMCreateTargetDataLayout(tm);
    char *dl_str = LLVMCopyStringRepOfTargetData(dl);
    LLVMSetDataLayout(module, dl_str);
    LLVMDisposeMessage(dl_str);

    // Determine object and executable paths
    const char *exe = out && out->exe_path ? out->exe_path : "a.out";
    char objbuf[1024];
    if (out && out->obj_path) {
        snprintf(objbuf, sizeof(objbuf), "%s", out->obj_path);
    } else {
        // obj next to exe: exe + ".o"
        size_t n = strlen(exe);
        if (n + 3 >= sizeof(objbuf)) n = sizeof(objbuf) - 4;
        memcpy(objbuf, exe, n);
        objbuf[n] = '\0';
        strncat(objbuf, ".o", sizeof(objbuf) - strlen(objbuf) - 1);
    }

    // Emit object file
    char *emit_err = NULL;
    if (LLVMTargetMachineEmitToFile(tm, module, objbuf, LLVMObjectFile, &emit_err) != 0) {
        fprintf(stderr, "Failed to emit object: %s\n", emit_err ? emit_err : "");
        LLVMDisposeMessage(emit_err);
        LLVMDisposeMessage(triple);
        LLVMDisposeTargetMachine(tm);
        return 1;
    }

    // Link using system compiler
    char cmd[2048];
    snprintf(cmd, sizeof(cmd), "cc %s -o %s", objbuf, exe);
    int rc = system(cmd);
    if (rc != 0) {
        fprintf(stderr, "Linker command failed (%d): %s\n", rc, cmd);
        LLVMDisposeMessage(triple);
        LLVMDisposeTargetMachine(tm);
        return 1;
    }

    LLVMDisposeMessage(triple);
    LLVMDisposeTargetMachine(tm);
    printf("Linked executable: %s\n", exe);
    return 0;
}

static char *read_file(const char *path) {
    FILE *f = fopen(path, "rb");
    if (!f) return NULL;
    fseek(f, 0, SEEK_END);
    long n = ftell(f);
    fseek(f, 0, SEEK_SET);
    char *buf = (char *)malloc((size_t)n + 1);
    if (!buf) { fclose(f); return NULL; }
    size_t rd = fread(buf, 1, (size_t)n, f);
    fclose(f);
    buf[rd] = '\0';
    return buf;
}

static int has_ext(const char *path, const char *ext) {
    size_t lp = strlen(path), le = strlen(ext);
    return lp >= le && strcmp(path + lp - le, ext) == 0;
}

int main(int argc, char **argv) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <file.lisp> [-o out.ll] [--exe [out]]\n", argv[0]);
        return 2;
    }
    const char *in_path = argv[1];
    const char *out_path = NULL;
    int want_exe = 0;
    char exe_path_buf[1024] = {0};
    BuildOut bout = {0};

    for (int i = 2; i < argc; i++) {
        if (strcmp(argv[i], "-o") == 0 && i + 1 < argc) {
            out_path = argv[++i];
        } else if (strcmp(argv[i], "--exe") == 0) {
            want_exe = 1;
            if (i + 1 < argc && argv[i+1][0] != '-') {
                ++i;
                snprintf(exe_path_buf, sizeof(exe_path_buf), "%s", argv[i]);
                bout.exe_path = exe_path_buf;
            }
        }
    }
    if (!has_ext(in_path, ".lisp") && !has_ext(in_path, ".lsp")) {
        fprintf(stderr, "Warning: input does not have .lisp extension\n");
    }
    char *src = read_file(in_path);
    if (!src) {
        fprintf(stderr, "Failed to read %s\n", in_path);
        return 1;
    }
    int rc = compile_to_ir(src, out_path);
    free(src);
    if (rc == 0) {
        const char *out = out_path ? out_path : "out.ll";
        printf("Wrote LLVM IR to %s\n", out);
        if (want_exe) {
            if (!bout.exe_path || bout.exe_path[0] == '\0') {
                // Derive exe name from input filename
                const char *p = strrchr(in_path, '/');
                const char *base = p ? p + 1 : in_path;
                size_t blen = strlen(base);
                // Strip .lisp or .lsp
                const char *dot = strrchr(base, '.');
                size_t name_len = dot ? (size_t)(dot - base) : blen;
                if (name_len >= sizeof(exe_path_buf)) name_len = sizeof(exe_path_buf) - 1;
                memcpy(exe_path_buf, base, name_len);
                exe_path_buf[name_len] = '\0';
                if (exe_path_buf[0] == '\0') snprintf(exe_path_buf, sizeof(exe_path_buf), "a.out");
                bout.exe_path = exe_path_buf;
            }
            // Re-parse IR? We still have the in-memory module only during compile_to_ir.
            // Simpler: regenerate by parsing again, but compile_to_ir currently disposes module.
            // Instead, re-run parse+codegen here quickly (duplicating a bit), to own the module.

            // Parse again
            char *src2 = read_file(in_path);
            if (!src2) {
                fprintf(stderr, "Failed to re-read %s for codegen\n", in_path);
                return 1;
            }
            Parser P; parser_init(&P, src2);
            Ast *root = NULL;
            while (!P.had_error && P.cur.kind != TOK_EOF) {
                if (P.cur.kind == TOK_LPAREN) {
                    Ast *a = parse_list(&P);
                    if (!a) break;
                    if (a->kind != AST_NUMBER || a->v.number != 0) {
                        if (root) ast_free_rec(root);
                        root = a;
                    } else {
                        ast_free_rec(a);
                    }
                } else if (P.cur.kind == TOK_NUMBER || P.cur.kind == TOK_STRING) {
                    Ast *a = parse_expr(&P);
                    if (!a) break;
                    if (root) ast_free_rec(root);
                    root = a;
                } else {
                    parser_error(&P, "unexpected token at top-level", P.cur.pos);
                    break;
                }
            }
            if (P.had_error || !root) {
                fprintf(stderr, "Parse error at offset %zu: %s\n", P.err_pos, P.err_msg[0] ? P.err_msg : "unknown error");
                free(src2);
                return 1;
            }
            // Codegen fresh module
            CG cg; cg_init(&cg, "lispc_module");
            // Predeclare externs
            if (P.n_protos) {
                cg_set_protos(&cg, P.protos, P.n_protos);
                LLVMTypeRef i32b = LLVMInt32TypeInContext(cg.ctx);
                for (size_t i = 0; i < P.n_protos; i++) {
                    Proto *pr = &P.protos[i];
                    LLVMTypeRef ret_ty = (pr->ret == TY_I64) ? cg.i64 : (pr->ret == TY_I32) ? i32b : (pr->ret == TY_I8PTR) ? cg.i8ptr : LLVMVoidTypeInContext(cg.ctx);
                    LLVMTypeRef *fixed = NULL;
                    if (pr->n_params) {
                        fixed = (LLVMTypeRef *)alloca(sizeof(LLVMTypeRef) * pr->n_params);
                        for (size_t j = 0; j < pr->n_params; j++) {
                            TyKind tk = pr->params[j];
                            fixed[j] = (tk == TY_I64) ? cg.i64 : (tk == TY_I32) ? i32b : (tk == TY_I8PTR) ? cg.i8ptr : LLVMVoidTypeInContext(cg.ctx);
                        }
                    }
                    LLVMTypeRef fnty = LLVMFunctionType(ret_ty, fixed, (unsigned)pr->n_params, pr->is_vararg);
                    if (!LLVMGetNamedFunction(cg.module, pr->name)) {
                        LLVMValueRef fn = LLVMAddFunction(cg.module, pr->name, fnty);
                        LLVMSetLinkage(fn, LLVMExternalLinkage);
                    }
                }
            }
            LLVMTypeRef fn_type = LLVMFunctionType(cg.i64, NULL, 0, 0);
            LLVMValueRef lisp_main = LLVMAddFunction(cg.module, "lisp_main", fn_type);
            LLVMBasicBlockRef entry = LLVMAppendBasicBlockInContext(cg.ctx, lisp_main, "entry");
            LLVMPositionBuilderAtEnd(cg.builder, entry);
            LLVMValueRef result = codegen_expr(&cg, root);
            LLVMBuildRet(cg.builder, result);
            LLVMTypeRef i32 = LLVMInt32TypeInContext(cg.ctx);
            LLVMTypeRef main_ty = LLVMFunctionType(i32, NULL, 0, 0);
            LLVMValueRef c_main = LLVMAddFunction(cg.module, "main", main_ty);
            LLVMBasicBlockRef mentry = LLVMAppendBasicBlockInContext(cg.ctx, c_main, "entry");
            LLVMPositionBuilderAtEnd(cg.builder, mentry);
            LLVMValueRef call_res = LLVMBuildCall2(cg.builder, fn_type, lisp_main, NULL, 0, "res");
            LLVMValueRef trunc = LLVMBuildTruncOrBitCast(cg.builder, call_res, i32, "ret32");
            LLVMBuildRet(cg.builder, trunc);

            char *verr = NULL;
            LLVMVerifyModule(cg.module, LLVMAbortProcessAction, &verr);
            LLVMDisposeMessage(verr);

            int link_rc = emit_object_and_link(cg.module, &bout);
            ast_free_rec(root);
            cg_free_protos(&cg);
            cg_dispose(&cg);
            free(src2);
            if (link_rc != 0) return link_rc;
        }
    }
    return rc;
}
