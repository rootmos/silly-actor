#pragma once

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

typedef uint64_t word_t;

enum value_type {
    NIL = 0,
    NUMBER,
    ATOM,
    CL,
    SYS
};

enum sys_atom {
    OUTPUT=1
};

struct value {
    enum value_type t;
    word_t v;
};

word_t mk_cons(struct value v0, struct value v1);
bool is_cons(struct value v);

bool eq(struct value v0, struct value v1);
