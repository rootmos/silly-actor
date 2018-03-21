#include <stubs.h>
#include <stack.h>
#include <value.h>
#include <stddef.h>
#include <stdbool.h>
#include <runtime.h>
#include <common.h>

struct value stov(struct stack_data sd)
{
    struct value v = {.t=sd.d[0], .v=sd.d[1] };
    return v;
}

struct stack_data vtos(struct value v)
{
    struct stack_data sd = {.d = {v.t, v.v}};
    return sd;
}

#define atoms_begin() \
    void atoms_lookup(word_t w, const char** p, size_t* n) \
    { switch(w) {
#define atoms_entry(w,a) case w: *p = a; *n = sizeof(a) - 1; break;
#define atoms_end()   default: failwith("unknown atom: %lu", w); } }
