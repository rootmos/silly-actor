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
    SYS,
    ACTOR_ID,
    CONS
};

enum sys_atom {
    SYS_ROOT=0,
    SYS_OUTPUT,
    SYS_INIT,
    SYS_DIED,
    SYS_MATCH_ERROR,
    SYS_ATOMS_END
};

#define sys_actor(s) (s == SYS_OUTPUT)

struct value {
    enum value_type t;
    word_t v;
};

struct value mk_cons(struct value v0, struct value v1);
#define mk_atom(a) ((struct value){.t=ATOM,.v=a})
#define mk_number(n) ((struct value){.t=NUMBER,.v=n})
#define mk_sys(s) ((struct value){.t=SYS,.v=SYS_##s})
#define mk_nil() ((struct value){.t=NIL,.v=0})
#define mk_aid(aid) ((struct value){.t=ACTOR_ID,.v=aid})

bool is_cons(struct value v);

bool eq(struct value v0, struct value v1);

struct value car(struct value c);
struct value cdr(struct value c);

const char* pretty_print(struct value);
