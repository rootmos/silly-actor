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
        log(STDERR, "stack empty");
    } else {
        log(STDERR, "stack:");
        for (size_t n = 0; n <= stack_height(st); n++) {
            log(STDERR, " %u: %s", n, pretty_print(stov(stack_nth(st, n))));
        }
    }
}

#define atoms_begin() \
    void atoms_lookup(word_t w, const char** p, size_t* n) \
    { switch(w) {
#define atoms_entry(w,a) case w: *p = a; *n = sizeof(a) - 1; break;
#define atoms_end()   default: failwith("unknown atom: %lu", w); } }

#define str(s) #s

#define define_unary_closure(cl,v) \
    struct trampoline cl(struct stack* st, struct value v) { \
        debug("in "str(cl)"(%s)", pretty_print(v)); \
        if(log_level >= TRACE) { print_stack(st); }

#define define_nullary_closure(cl) \
    struct trampoline cl(struct stack* st, struct value _) { \
        debug("in "str(cl)"()"); \
        if(log_level >= TRACE) { print_stack(st); }

#define end_closure() return yield(); }

#define define_system() void setup_system(struct stack* st) {
#define end_system() }

#define push(v) stack_push(st,vtos(v))
#define nth(n) stov(stack_nth(st,n))

#define mk_cl(cl) make_closure(cl, stack_fork(st))

#define predefined_atom(a,v) struct value ATOM_##a = mk_atom(v)
