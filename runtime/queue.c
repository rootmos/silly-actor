#include <stdlib.h>
#include <common.h>
#include <queue.h>

struct queue_node {
    void* p;
    struct queue_node* next;
};

struct queue {
    struct queue_node* head;
    struct queue_node* tail;
};

struct queue* fresh_queue()
{
    return calloc(sizeof(struct queue), 1);
}

void enqueue(struct queue* q, void* m)
{
    not_implemented();
}

bool dequeue(struct queue* q, void** m)
{
    not_implemented();
}
