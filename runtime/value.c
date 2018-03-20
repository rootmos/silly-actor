#include <assert.h>
#include <stdlib.h>
#include <value.h>
#include <common.h>

struct cons {
    struct value car;
    struct value cdr;
};

struct value mk_cons(struct value v0, struct value v1)
{
    struct cons* c = (struct cons*)malloc(sizeof(*c));
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

void pretty_print(int fd, struct value v)
{
    switch (v.t) {
    case NIL: dprintf(fd, "'()"); break;
    case NUMBER: dprintf(fd, "(number . %d)", v.v); break;
    case ATOM: not_implemented();
    case CL: dprintf(fd, "<closure>"); break;
    case SYS: not_implemented();
    case ACTOR_ID: dprintf(fd, "<%d>", v.v); break;
    case CONS: {
        struct cons* c = (struct cons*)v.v;
        dprintf(fd, "(");
        pretty_print(fd, c->car);
        dprintf(fd, ",");
        pretty_print(fd, c->cdr);
        dprintf(fd, ")");
    }
    }
}
