#include <stdlib.h>
#include <stdio.h>

struct Node {
    int marble;
    struct Node *next;
    struct Node *prev;
};

struct Node* mkRing(int v) {
    struct Node *current = (struct Node*)malloc(sizeof(struct Node));
    current->next = current;
    current->prev = current;
    current->marble = v;
    return current;
}

int main(){
    struct Node *current = mkRing(0);
    printf("hello, world");
    return 0;
}
