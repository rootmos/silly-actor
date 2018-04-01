#include <assert.h>
#include <stdlib.h>
#include <common.h>
#include <queue.h>

struct queue {
    struct msg* head;
    struct msg* tail;
};

struct queue* fresh_queue()
{
    struct queue* q = calloc(sizeof(struct queue), 1); assert(q);
    return q;
}

void enqueue(struct queue* q, struct msg* m)
{
    assert(m->next == NULL);

    if(q->head == NULL) {
        assert(q->head == NULL);
        q->head = q->tail = m;
    } else {
        assert(q->tail->next == NULL);
        q->tail->next = m;
        q->tail = m;
    }
}

bool dequeue(struct queue* q, struct msg** m)
{
    if (q->head == NULL) return false;

    *m = q->head;
    q->head = (*m)->next;
    return true;
}
