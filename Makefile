CC ?= gcc
OUT := libfbx
SUFFIX := so
CFLAGS := -O3 -ftree-vectorize -fPIC -g -D UFBX_REAL_IS_FLOAT
CFLAGS_SSE := -msse -mfpmath=sse
CFLAGS_M1 := -march=armv8.5-a -mfpu=neon
LDFLAGS := -l m

ifeq ($(OS),Windows_NT)
    CFLAGS += $(CFLAGS_SSE)
    OUT := $(OUT)-win
    SUFFIX := dll
    ifeq ($(PROCESSOR_ARCHITEW6432),AMD64)
        OUT := $(OUT)-amd64
    else
        ifeq ($(PROCESSOR_ARCHITECTURE),AMD64)
            OUT := $(OUT)-amd64
        endif
        ifeq ($(PROCESSOR_ARCHITECTURE),x86)
            OUT := $(OUT)-i686
        endif
    endif
else
    UNAME_S := $(shell uname -s)
    ifeq ($(UNAME_S),Linux)
        OUT := $(OUT)-lin
	CFLAGS += -static-libgcc -include glibc-2.13.h
    endif
    ifeq ($(UNAME_S),Darwin)
        OUT := $(OUT)-mac
	SUFFIX = dylib
    endif
    PROC_P := $(shell $(CC) -dumpmachine)
    ifneq ($(filter %x86_64,$(PROC_P)),)
        OUT := $(OUT)-amd64
        CFLAGS += $(CFLAGS_SSE)
    endif
    ifneq ($(filter %86,$(PROC_P)),)
        OUT := $(OUT)-i686
        CFLAGS += $(CFLAGS_SSE)
    endif
    ifneq ($(filter arm%,$(PROC_P)),)
        OUT := $(OUT)-arm
        CFLAGS += $(CFLAGS_M1)
    endif
endif

all:
	$(CC) -shared -o static/$(OUT).$(SUFFIX) $(CFLAGS) ufbx.c $(LDFLAGS)
