#pragma once

#include <value.h>
#include <stack.h>

enum trampoline_action {
    CONTINUE = 0,
    YIELD
};

struct trampoline {
    enum trampoline_action a;
    struct value cl;
    struct value v;
};

struct trampoline yield();
struct trampoline cont(struct value cl, struct value v);
struct trampoline match_error();

struct value fromM();
struct value msgM();
struct value sendM(struct value to, struct value msg);
struct value becomeM(struct value cl);
struct value spawnM(struct value cl, struct value state);
struct value stateM();
struct value selfM();

typedef struct trampoline (*cl_t)(struct stack*, struct value);

struct value mk_cl(cl_t, struct stack*);
