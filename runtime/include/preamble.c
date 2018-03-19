#include <stack.h>
#include <value.h>
#include <stddef.h>
#include <stdbool.h>
#include <runtime.h>

inline struct value stov(struct stack_data sd)
{
    struct value v = {.t=sd.d[0], .v=sd.d[1] };
    return v;
}

inline struct stack_data vtos(struct value v)
{
    struct stack_data sd = {.d = {v.t, v.v}};
    return sd;
}
