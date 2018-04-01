#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include <value.h>
#include <common.h>
#include <stubs.h>
#include <mem.h>

struct cons {
    struct value car;
    struct value cdr;
};

struct value mk_cons(struct value v0, struct value v1)
{
    struct cons* c = (struct cons*)my_malloc(sizeof(*c)); assert(c);
    c->car = v0;
    c->cdr = v1;
    return (struct value){.t=CONS,.v=(word_t)c};
}

bool is_cons(struct value v)
{
    return v.t == CONS;
}

bool eq(struct value v0, struct value v1)
{
    debug("%s =?= %s", pretty_print(v0), pretty_print(v1));

    if(v0.t != v1.t) {
        return false;
    } else {
        switch (v0.t) {
        case CL: return false;
        case NIL: return true;
        case ATOM:
        case SYS:
        case ACTOR_ID:
        case NUMBER: return v0.v == v1.v;
        case CONS: {
            struct cons* c0 = (struct cons*)v0.v;
            struct cons* c1 = (struct cons*)v1.v;
            return eq(c0->car, c1->car) && eq(c0->cdr, c1->cdr);
        }
        }
    }
}

struct value car(struct value c)
{
    assert(c.t == CONS);
    return ((struct cons*)c.v)->car;
}

struct value cdr(struct value c)
{
    assert(c.t == CONS);
    return ((struct cons*)c.v)->cdr;
}

void do_put_str(char**buf, int* rem, const char* s, size_t n)
{
    if(n > *rem) failwith("pretty_print buffer too small");
    memcpy(*buf, s, n);
    *rem -= n;
    *buf += n;
}

void do_put_word(char**buf, int* rem, word_t w)
{
    int n = snprintf(*buf, *rem, "%lu", w);
    if(n < 0) failwith("snprintf failed");
    if(n > *rem) failwith("pretty_print buffer too small");
    *rem -= n;
    *buf += n;
}

#define do_put_lit(buf,rem,lit) do_put_str(buf,rem,lit,sizeof(lit)-1)

void do_pretty_print(struct value v, char** buf, int* rem)
{
    switch (v.t) {
    case NIL: do_put_lit(buf, rem, "()"); break;
    case NUMBER:
        do_put_lit(buf, rem, "(number . ");
        do_put_word(buf, rem, v.v);
        do_put_lit(buf, rem, ")");
        break;
    case ATOM: {
        do_put_lit(buf, rem, "(atom . ");
        size_t n; const char* p;
        atoms_lookup(v.v, &p, &n);
        do_put_str(buf, rem, p, n);
        do_put_lit(buf, rem, ")");
        break;
    }
    case CL: do_put_lit(buf, rem, "<closure>"); break;
    case SYS: {
        switch (v.v) {
        case SYS_ROOT: do_put_lit(buf, rem, "(sys . Root)"); break;
        case SYS_OUTPUT: do_put_lit(buf, rem, "(sys . Output)"); break;
        case SYS_INIT: do_put_lit(buf, rem, "(sys . Init)"); break;
        case SYS_DIED: do_put_lit(buf, rem, "(sys . Died)"); break;
        case SYS_MATCH_ERROR: do_put_lit(buf, rem, "(sys . Match_error)"); break;
        default: failwith("unprintable sys atom");
        }
       break;
    }
    case ACTOR_ID:
        do_put_lit(buf, rem, "<");
        do_put_word(buf, rem, v.v);
        do_put_lit(buf, rem, ">");
        break;
    case CONS: {
        struct cons* c = (struct cons*)v.v;
        do_put_lit(buf, rem, "(");
        do_pretty_print(c->car, buf, rem);
        do_put_lit(buf, rem, " . ");
        do_pretty_print(c->cdr, buf, rem);
        do_put_lit(buf, rem, ")");
        break;
    }
    }
}

#define PRETTY_PRINT_BUF 1000

const char* pretty_print(struct value v)
{
    static char buf[PRETTY_PRINT_BUF];
    char* p = buf;
    int rem = PRETTY_PRINT_BUF;
    do_pretty_print(v, &p, &rem);
    do_put_lit(&p, &rem, "\0");

    size_t n = PRETTY_PRINT_BUF - rem;
    p = my_malloc(n); assert(p);
    memcpy(p, buf, n);
    return p;
}
