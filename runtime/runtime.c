#include <assert.h>

#include <common.h>
#include <stubs.h>
#include <value.h>
#include <runtime.h>
#include <queue.h>

#include <stdlib.h>

struct system {
    struct actor** as;
    size_t N;

    struct queue* q;

    actor_id next_aid;

    actor_id current_aid;
    struct msg* current_msg;
};

struct system s;

void insert_actor(struct actor* a)
{
    struct actor** n = &s.as[a->aid % s.N];
    while (*n) { n = &(*n)->next; }
    *n = a;
}

struct actor* fetch_actor(actor_id aid)
{
    struct actor* n = s.as[aid % s.N];
    while(n) {
        if(n->aid == aid) return n;
        n = n->next;
    }
    failwith("unknown actor id: %d\n", aid);
}

struct actor* current_actor()
{
    return fetch_actor(s.current_aid);
}

void initialize_system(struct system* s)
{
    s->N = 100;
    s->as = (struct actor**)calloc(sizeof(struct actor*), s->N);

    s->q = fresh_queue();
    s->next_aid = SYS_ATOMS_END;

    s->current_aid = SYS_ROOT;
}

struct closure* cast_cl(struct value v)
{
    if(v.t != CL) {
        failwith("trying to cast %s to a closure", pretty_print(v));
    }
    return (struct closure*)v.v;
}

actor_id cast_aid(struct value v)
{
    assert(v.t == ACTOR_ID || (v.t == SYS && sys_actor(v.v)));
    return (actor_id)v.v;
}

struct trampoline yield()
{
    return (struct trampoline){.a=YIELD};
}

struct trampoline cont(struct value cl, struct value v)
{
    return (struct trampoline){.a=CONTINUE,.cl=cl,.v=v};
}

void send(actor_id to, struct value v)
{
    struct msg* m = (struct msg*)malloc(sizeof(*m));
    m->to = to;
    m->from = s.current_aid;
    m->v = v;
    m->next = NULL;
    enqueue(s.q, m);

    debug("enqueued msg: %d -> %d, %s", m->from, m->to, pretty_print(v));
}

struct value sendR(struct value to, struct value data)
{
    send(cast_aid(to), data);
    return mk_nil();
}

struct trampoline match_error(struct value v)
{
    send(current_actor()->parent,
         mk_cons(
             mk_sys(DIED),
             mk_cons(
                 mk_sys(MATCH_ERROR),
                 mk_cons(v, mk_nil()))));

    // TODO: remove actor

    return (struct trampoline){.a=MATCH_ERROR};
}

struct trampoline value_error()
{
    not_implemented();
}


struct value fromR()
{
    return mk_aid(s.current_msg->from);
}

struct value msgR()
{
    return s.current_msg->v;
}

struct value parentR()
{
    return mk_aid(current_actor()->parent);
}

struct value equalR(struct value a, struct value b)
{
    if (eq(a, b)) {
        return ATOM_TRUE;
    } else {
        return ATOM_FALSE;
    }
}

struct actor* spawn(struct closure* cl, actor_id aid, struct value state)
{
    struct actor* a = (struct actor*)malloc(sizeof(*a));
    a->aid = aid;

    a->cl = cl;
    a->state = state;
    a->parent = s.current_aid;

    insert_actor(a);
}

struct value spawnR(struct value cl, struct value state)
{
    actor_id aid = s.next_aid;
    s.next_aid += 1;

    spawn(cast_cl(cl), aid, state);
    send(aid, mk_sys(INIT));

    debug("spawning actor with id: %d", aid);

    return mk_aid(aid);
}


struct value stateR()
{
    return current_actor()->state;
}

struct value set_stateR(struct value state)
{
   current_actor()->state = state;
   return mk_nil();
}

struct value set_clR(struct value cl)
{
    current_actor()->cl = cast_cl(cl);
    return mk_nil();
}

struct value selfR()
{
    return (struct value){.t=ACTOR_ID,.v=(word_t)(s.current_aid)};
}

struct value make_closure(cl_t cl, struct stack* st)
{
    struct closure* c = (struct closure*)malloc(sizeof(*c));
    c->f = cl;
    c->st = st;
    return (struct value){.t=CL,.v=(word_t)c};
}

struct trampoline output(struct stack* st, struct value v)
{
    dprintf(1, "%s", pretty_print(msgR()));
    return yield();
}

struct closure* null_closure(cl_t f)
{
    struct closure* cl = (struct closure*)malloc(sizeof(*cl));
    cl->f = f;
    cl->st = NULL;
    return cl;
}

void go(struct actor* a, struct closure* cl, struct value v)
{
    struct trampoline t;
    while (true) {
        t = (cl->f)(cl->st, v);
        switch (t.a) {
        case MATCH_ERROR:
        case YIELD: return;
        case CONTINUE: {
            cl = cast_cl(t.cl);
            v = t.v;
            break;
        }
        default: not_implemented();
        }
    }
}

int main()
{
    set_log_level();

    initialize_system(&s);

    struct stack* root_stack = stack_fresh();
    setup_system(root_stack);
    spawn(null_closure(output), SYS_OUTPUT, mk_nil());

    while(dequeue(s.q, &s.current_msg)) {
        struct actor* a = fetch_actor(s.current_msg->to);
        debug("processing msg: %d -> %d, %s",
             s.current_msg->from,
             s.current_msg->to,
             pretty_print(s.current_msg->v));
        s.current_aid = a->aid;
        go(a, a->cl, mk_nil());
    }

    return 0;
}
