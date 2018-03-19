#pragma once

#include <value.h>

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
#define yieldM() return yield()
struct trampoline cont(struct value cl, struct value v);

struct value fromM();
struct value msgM();
struct value sendM();
