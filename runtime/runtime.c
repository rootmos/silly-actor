#include <assert.h>
#include <common.h>

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

    s->current_aid = ROOT;
}



struct closure* cast_cl(struct value v)
{
    assert(v.t == CL);
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

struct trampoline match_error()
{
    not_implemented();
}

struct trampoline value_error()
{
    not_implemented();
}


struct value fromM()
{
    not_implemented();
}

struct value msgM()
{
    return s.current_msg->v;
}

void send(actor_id to, struct value v)
{
    struct msg* m = (struct msg*)malloc(sizeof(*m));
    m->to = to;
    m->from = s.current_aid;
    m->v = v;
    m->next = NULL;
    enqueue(s.q, m);
}

struct value sendM(struct value to, struct value data)
{
    send(cast_aid(to), data);
    return mk_nil();
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

struct value spawnM(struct value cl, struct value state)
{
    actor_id aid = s.next_aid;
    s.next_aid += 1;

    spawn(cast_cl(cl), aid, state);
    send(aid, mk_sys(INIT));

    info("spawning actor with id: %d", aid);

    return mk_aid(aid);
}


struct value stateM()
{
    return current_actor()->state;
}

struct value set_stateM(struct value state)
{
   current_actor()->state = state;
   return mk_nil();
}

struct value set_clM(struct value cl)
{
    current_actor()->cl = cast_cl(cl);
    return mk_nil();
}

struct value selfM()
{
    return (struct value){.t=ACTOR_ID,.v=(word_t)(s.current_aid)};
}

struct value mk_cl(cl_t cl, struct stack* st)
{
    struct closure* c = (struct closure*)malloc(sizeof(*c));
    c->f = cl;
    c->st = st;
    return (struct value){.t=CL,.v=(word_t)c};
}

struct trampoline output(struct stack* st, struct value v)
{
    pretty_print(1, msgM());
    return yield();
}

struct closure* null_closure(cl_t f)
{
    struct closure* cl = (struct closure*)malloc(sizeof(*cl));
    cl->f = f;
    cl->st = NULL;
    return cl;
}

extern void setup_system(struct stack*);

void go(struct actor* a, struct value v)
{
    struct trampoline t = (a->cl->f)(a->cl->st, v);
    switch (t.a) {
    case YIELD: return;
    default: not_implemented();
    case CONTINUE: {
        set_clM(t.cl);
        break;
    }
    }
    go(a, t.v);
}

int main()
{
    initialize_system(&s);

    struct stack* root_stack = stack_fresh();
    setup_system(root_stack);
    spawn(null_closure(output), OUTPUT, mk_nil());

    while(dequeue(s.q, &s.current_msg)) {
        struct actor* a = fetch_actor(s.current_msg->to);
        info("processing msg: %d -> %d", s.current_msg->from, s.current_msg->to);
        s.current_aid = a->aid;
        go(a, mk_nil());
    }

    return 0;
}
