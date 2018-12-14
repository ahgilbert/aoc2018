#include <stdlib.h>
#include <stdio.h>

struct Node {
    long val;
    struct Node *next;
    struct Node *prev;
};

struct Node* mkNode(long v) {
    struct Node *newNode = (struct Node*)malloc(sizeof(struct Node));
    newNode->val = v;
    return newNode;
}

struct Node* mkRing(long v) {
    struct Node *current = mkNode(v);
    current->next = current;
    current->prev = current;
    return current;
}

struct Node* insert(struct Node* current, long v) {
    // insert v clockwise of current
    struct Node *newNode = mkNode(v);
    newNode->next = current->next;
    newNode->prev = current;
    current->next->prev = newNode;
    current->next = newNode;
    return newNode;
}

struct Node* removeNode(struct Node* current) {
    current->prev->next = current->next;
    current->next->prev = current->prev;
    // printf("removing %d, new current is %d\n", current->val, current->next->val);
    struct Node *retVal = current->next;
    free(current);
    return retVal;
}

struct Node *moveCounterClockwise(struct Node *current, long steps) {
    for(long i = 0; i < steps; i++) {
        current = current->prev;
    }
    return current;
}

struct Node *moveClockwise(struct Node *current, long steps) {
    for(long i = 0; i < steps; i++) {
        current = current->next;
    }
    return current;
}

void walkArray(struct Node* current) {
    struct Node *here = current->next;
    printf("(%ld) ", current->val);
    while(here != current) {
        printf("%ld ", here->val);
        here = here->next;
    }
    printf("\n");
}

int main(int argc, char *argv[]){
    long stop = atoi(argv[1]);
    struct Node *origin = mkRing(3);
    insert(origin, 7);
    struct Node* elves[2] = {origin,(origin->next)};

    for(long m=1; m < stop + 10; m++) {
        for(long n=0; n < 2; n++) {
            printf("elf %ld: %ld\n", n+1, elves[n]->val);
        }
    }
    return 0;
}
