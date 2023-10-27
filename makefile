CLANG_PROGRAM_NAME := clang++
undefine EXTRA_CLANG_FLAGS

.PHONY: all build rebuild clean

all: build

build: a.out

a.out: moken.h test.cpp
	$(CLANG_PROGRAM_NAME) --std=c++20 -Wall $(EXTRA_CLANG_FLAGS) test.cpp -o a.out

rebuild:
	$(MAKE) clean
	$(MAKE) build

clean:
	git clean -fdx
