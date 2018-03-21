#pragma once

#include <value.h>
#include <stack.h>

enum trampoline_action {
    CONTINUE = 0,
    YIELD,
    MATCH_ERROR,
    VALUE_ERROR
};

struct trampoline {
    enum trampoline_action a;
    struct value cl;
    struct value v;
};

struct trampoline yield();
struct trampoline cont(struct value cl, struct value v);
struct trampoline match_error();
struct trampoline value_error();

struct value fromM();
struct value parentM();
struct value msgM();
struct value sendM(struct value to, struct value msg);
struct value spawnM(struct value cl, struct value state);
struct value stateM();
struct value selfM();
struct value set_stateM(struct value state);
struct value set_clM(struct value cl);

typedef struct trampoline (*cl_t)(struct stack*, struct value);


typedef uint64_t actor_id;

struct msg {
    actor_id to;
    actor_id from;
    struct value v;
    struct msg* next;
};

struct closure {
    cl_t f;
    struct stack* st;
};

struct value make_closure(cl_t, struct stack*);
struct closure* cast_cl(struct value);

struct actor {
    actor_id aid;
    struct closure* cl;
    struct value state;
    actor_id parent;
    struct actor* next;
};
