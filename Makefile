CC ?= cc
CFLAGS ?= -Wall -Wextra -O2

LLVM_CONFIG ?= llvm-config
LLVM_CFLAGS := $(shell $(LLVM_CONFIG) --cflags)
LLVM_LIBS := $(shell $(LLVM_CONFIG) --libs core native)
LLVM_LDFLAGS := $(shell $(LLVM_CONFIG) --ldflags --system-libs) $(LLVM_LIBS)

all: main

main: main.c
	$(CC) $(CFLAGS) $(LLVM_CFLAGS) -o $@ $< $(LLVM_LDFLAGS)

.PHONY: clean
clean:
	rm -f main
