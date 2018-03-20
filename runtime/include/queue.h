#pragma once

#include <runtime.h>

struct queue;

struct queue* fresh_queue();
void enqueue(struct queue*, void*);
bool dequeue(struct queue*, void**);
