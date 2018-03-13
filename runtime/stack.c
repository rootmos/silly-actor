#include <common.h>

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>

struct slot {
    void* p;
    uint16_t fork_count;
};

struct block {
    bool claimed;
    size_t N;
    size_t msp; /* TODO: maybe set this as a *size_t to the owners sp? */
    struct slot slots[];
};

struct stack {
    struct block* block;
    size_t sp;
    bool fork;
};

#define STACK_NO_INITIAL_SLOTS 100

struct stack* stack_fresh()
{
    struct stack* st = (struct stack*)malloc(sizeof(*st));
    st->sp = -1;
    st->fork = false;

    size_t block_size =
        sizeof(struct block) +
        STACK_NO_INITIAL_SLOTS * sizeof(struct slot);
    struct block* b = st->block =
        (struct block*)calloc(block_size, 1);
    b->claimed = true;
    b->msp = st->sp;
    b->N = STACK_NO_INITIAL_SLOTS;
    return st;
}

void stack_push(struct stack* st, void* p)
{
    size_t sp0 = st->sp;
    size_t sp1 = st->sp += 1;
    struct block* block = st->block;

    if (st->fork) {
        block->slots[sp0].fork_count -= 1;
        block->slots[sp1].fork_count += 1;

        if (sp0 == block->msp && !block->claimed) {
            block->claimed = true;
            st->fork = false;
            block->msp += 1;
        }

        not_implemented();
    } else {
        block->msp += 1;
        block->slots[sp1].p = p;
        /* TODO: clear fork_count - test it! */
    }
}

void stack_pop(struct stack* st)
{
    size_t sp = st->sp;
    st->sp -= 1;
    struct block* block = st->block;

    if (st->fork) {
        block->slots[sp].fork_count -= 1;
        block->slots[st->sp].fork_count += 1;

        if (sp == block->msp
           && block->slots[sp].fork_count == 0
           && !block->claimed) {
           block->msp -= 1;
        }
    } else {
        if (block->slots[sp].fork_count == 0) {
            block->msp -= 1;
        } else {
            st->fork = true;
            st->block->claimed = false;
        }
    }
}

void* stack_nth(const struct stack* st, size_t n)
{
    return st->block->slots[st->sp - n].p;
}

struct stack* stack_fork(const struct stack* st)
{
    struct stack* tt = (struct stack*)malloc(sizeof(*tt));
    tt->block = st->block;
    tt->sp = st->sp;
    tt->fork = true;
    tt->block->slots[tt->sp].fork_count += 1;
    return tt;
}

#ifdef TEST
#include <tests.h>

test_suite(stack)

test_case(stack_fresh)
{
    struct stack* st = stack_fresh();
    assert(st != NULL);
    assert(st->fork == false);
    assert(st->sp == -1);
    assert(st->block->N == STACK_NO_INITIAL_SLOTS);
    assert(st->block->msp == -1);
    for (size_t i = 0; i < st->block->N; i++) {
        assert(st->block->slots[i].p == NULL);
        assert(st->block->slots[i].fork_count == 0);
    }
    ok = true;
}
test_case_end(stack_fresh);

test_case(stack_push)
{
    struct stack* st = stack_fresh();
    fresh(void*, p1);
    stack_push(st, p1);
    assert(st->sp == 0);
    assert(st->block->msp == 0);
    assert(st->block->slots[0].p == p1);

    fresh(void*, p2);
    stack_push(st, p2);
    assert(st->sp == 1);
    assert(st->block->msp == 1);
    assert(st->block->slots[1].p == p2);

    ok = true;
}
test_case_end(stack_push);

test_suite_end()

#endif
