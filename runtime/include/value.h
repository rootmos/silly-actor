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
    OUTPUT=1,
    INIT
};

struct value {
    enum value_type t;
    word_t v;
};

struct value mk_cons(struct value v0, struct value v1);
#define mk_atom(a) ((struct value){.t=ATOM,.v=a})
#define mk_number(n) ((struct value){.t=NUMBER,.v=n})
#define mk_sys(s) ((struct value){.t=SYS,.v=s})
#define mk_nil() ((struct value){.t=NIL,.v=0})

bool is_cons(struct value v);

bool eq(struct value v0, struct value v1);

struct value car(struct value c);
struct value cdr(struct value c);
