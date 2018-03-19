#pragma once

struct stack;
struct stack* stack_fresh();
void stack_push(struct stack* st, void* p);
void stack_pop(struct stack* st);
void* stack_nth(const struct stack* st, size_t n);
struct stack* stack_fork(const struct stack* st);
