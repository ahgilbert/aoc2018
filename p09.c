#include <stdlib.h>
#include <stdio.h>

struct Node {
    int marble;
    struct Node *next;
    struct Node *prev;
};

struct Node* mkNode(int v) {
    struct Node *newNode = (struct Node*)malloc(sizeof(struct Node));
    newNode->marble = v;
    return newNode;
}

struct Node* mkRing(int v) {
    struct Node *current = mkNode(v);
    current->next = current;
    current->prev = current;
    return current;
}

struct Node* insert(struct Node* current, int v) {
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

struct Node *moveCounterClockwise(struct Node *current, int steps) {
    for(int i = 0; i < steps; i++) {
        current = current->prev;
    }
    return current;
}

struct Node *moveClockwise(struct Node *current, int steps) {
    for(int i = 0; i < steps; i++) {
        current = current->next;
    }
    return current;
}

int* initScores(int n) {
    int *scores = (int*)malloc(sizeof(int) * n);
    for(int i=0; i<n; i++) {
        scores[i] = 0;
    }
    return scores;
}

void walkArray(struct Node* current) {
    struct Node *here = current->next;
    printf("(%d) ", current->marble);
    while(here != current) {
        printf("%d ", here->marble);
        here = here->next;
    }
    printf("\n");
}

int arrayMax(int *arr, int n) {
    int winner = 0;
    for(int i=0; i<n; i++) {
        if(arr[i] > winner) {
            winner = arr[i];
        }
    }
    return winner;
}

int main(int argc, char *argv[]){
    int numPlayers = atoi(argv[1]);
    int numMarbles = atoi(argv[2]);
    int *scores = initScores(numPlayers);
    struct Node *current = mkRing(0);
    for(int m=1; m < numMarbles; m++) {
        // walkArray(current);
        if(m % 23 == 0) {
            // add scores to current player
            current = moveCounterClockwise(current, 7);
            int points = m + current->marble;
            current = removeMarble(current);
            scores[m % numPlayers] += points;
        } else {
            // drop marble, update current
            current = insert(moveClockwise(current,1), m);
        }
    }

    printf("high score: %d", arrayMax(scores, numPlayers));
    return 0;
}
