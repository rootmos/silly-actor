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

void print_stack(const struct stack* st)
{
    size_t N = stack_height(st);
    if (N == -1) {
        debug("stack empty");
    }
    else {
        debug("stack:");
        for (size_t n = 0; n <= stack_height(st); n++) {
            debug(" %u: %s", n, pretty_print(stov(stack_nth(st, n))));
        }
    }
}

#define atoms_begin() \
    void atoms_lookup(word_t w, const char** p, size_t* n) \
    { switch(w) {
#define atoms_entry(w,a) case w: *p = a; *n = sizeof(a) - 1; break;
#define atoms_end()   default: failwith("unknown atom: %lu", w); } }
