#include <stdlib.h>
#include <stdio.h>

struct Node {
    long marble;
    struct Node *next;
    struct Node *prev;
};

struct Node* mkNode(long v) {
    struct Node *newNode = (struct Node*)malloc(sizeof(struct Node));
    newNode->marble = v;
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

struct Node* removeMarble(struct Node* current) {
    current->prev->next = current->next;
    current->next->prev = current->prev;
    // printf("removing %d, new current is %d\n", current->marble, current->next->marble);
    return current->next;
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

long* initScores(long n) {
    long *scores = (long*)malloc(sizeof(long) * n);
    for(long i=0; i<n; i++) {
        scores[i] = 0;
    }
    return scores;
}

void walkArray(struct Node* current) {
    struct Node *here = current->next;
    printf("(%ld) ", current->marble);
    while(here != current) {
        printf("%ld ", here->marble);
        here = here->next;
    }
    printf("\n");
}

long arrayMax(long *arr, long n) {
    long winner = 0;
    for(long i=0; i<n; i++) {
        if(arr[i] > winner) {
            winner = arr[i];
        }
    }
    return winner;
}

int main(int argc, char *argv[]){
    long numPlayers = atoi(argv[1]);
    long numMarbles = atoi(argv[2]);
    long *scores = initScores(numPlayers);
    struct Node *current = mkRing(0);
    for(long m=1; m < numMarbles; m++) {
        // walkArray(current);
        if(m % 23 == 0) {
            // add scores to current player
            current = moveCounterClockwise(current, 7);
            long points = m + current->marble;
            current = removeMarble(current);
            scores[m % numPlayers] += points;
        } else {
            // drop marble, update current
            current = insert(moveClockwise(current,1), m);
        }
    }

    printf("high score: %ld", arrayMax(scores, numPlayers));
    return 0;
}
