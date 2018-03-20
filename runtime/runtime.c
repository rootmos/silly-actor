#include <common.h>

#include <value.h>
#include <runtime.h>
#include <queue.h>

#include <stdlib.h>

struct actor_node {
    struct actor* a;
    struct actor_node* n;
};

struct system {
    struct actor_node* as;
    size_t N;

    struct queue* q;

    actor_id next_aid;
};

void initialize_system(struct system* s)
{
    s->N = 100;
    s->as = (struct actor_node*)calloc(sizeof(struct actor_node), s->N);

    s->q = fresh_queue();
    s->next_aid = SYS_ATOMS_END;
}

struct trampoline yield()
{
    not_implemented();
}

struct trampoline cont(struct value cl, struct value v)
{
    not_implemented();
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
    not_implemented();
}

struct value sendM(struct value to, struct value msg)
{
    not_implemented();
}

struct value spawnM(struct value cl, struct value state)
{
    not_implemented();
}

struct value stateM()
{
    not_implemented();
}

struct value selfM()
{
    not_implemented();
}

struct value set_stateM(struct value state)
{
    not_implemented();
}

struct value set_clM(struct value cl)
{
    not_implemented();
}

struct value mk_cl(cl_t cl, struct stack* st)
{
    not_implemented();
}

extern void setup_system(struct stack*);

int main()
{
    struct system s;
    initialize_system(&s);

    struct stack* root_stack = stack_fresh();
    setup_system(root_stack);

    return 0;
}
