#pragma once

#include <value.h>

struct stack_data {
    word_t d[2];
};

struct stack;
struct stack* stack_fresh();
void stack_push(struct stack* st, struct stack_data d);
void stack_pop(struct stack* st);
struct stack_data stack_nth(const struct stack* st, size_t n);
struct stack* stack_fork(const struct stack* st);
