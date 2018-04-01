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
struct trampoline match_error(struct value v);
struct trampoline value_error();

struct value fromR();
struct value parentR();
struct value msgR();
struct value sendR(struct value to, struct value msg);
struct value spawnR(struct value cl, struct value state);
struct value stateR();
struct value selfR();
struct value set_stateR(struct value state);
struct value set_clR(struct value cl);

struct value equalR(struct value a, struct value b);
struct value addR(struct value a, struct value b);

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
