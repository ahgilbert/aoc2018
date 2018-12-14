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

void walkArray(struct Node* current, struct Node* one, struct Node* two) {
    struct Node *here = current;
    do {
        printf("%ld", here->val);
        /*
        if(here == one) {
            printf("(%ld) ", here->val);
        } else if(here == two) {
            printf("[%ld] ", here->val);
        } else {
            printf(" %ld  ", here->val);
        }
        */
        here = here->next;
    } while(here != current);
    printf("\n");
}

int numDigits(long n) {
    int d = 1;
    while (n >= 10) {
        d++;
        n /= 10;
    }
    return d;
}

struct Node *elfStep(struct Node *elf) {
    long score = 1 + elf->val;
    return moveClockwise(elf, score);
}

long scoreRecipes(struct Node *elves[], int nElves){
    long s = 0;
    for (int i = 0; i < nElves; i++) {
        s += elves[i]->val;
    }
    return s;
}

void part1(struct Node *origin, int skip) {
    for(int i = 0; i < skip; i++){
        origin = origin->next;
    }
    printf("part 1: ");
    for(int i = 0; i < 10; i++){
        printf("%ld", origin->val);
        origin = origin->next;
    }
    printf("\n");
}

int matchGo(struct Node *origin){
    int target[] = {1,6,0,7,4,1}; int d = 6; // from reddit --- input 147061 expect 20283721
    // int target[] = {1,0,5,4,7,0}; int d = 6; // {0,7,4,5,0,1};
    // int target[] = {9,8,5,1,5}; int d = 5; // expect 9
    // int target[] = {5,4,2,1,0}; int d = 5; // expect 5
    // int target[] = {0,1,5,2,9}; int d = 5; // expect 18
    // int target[] = {4,1,4,9,5}; int d = 5; // expect 2018
    // int target[] = {9};
    struct Node *current = origin;
    for(int i = 0; i < d; i++) {
        if(current->val != target[i]){
            // printf("x");
            return 0;
        }
        current = current->prev;
    }
    return d;
}

int match(struct Node *origin){
    // TODO report how many cells to step back to get to the beginning of the match
    if(matchGo(origin->prev)) {
        return 1;
    }

    return matchGo(origin->prev->prev);
}

int main(int argc, char *argv[]){
    long stop = 074501;
    struct Node *origin = mkRing(3);
    insert(origin, 7);
    int numElves = 2;
    struct Node* elves[2] = {origin,(origin->next)};

    int m = 0;
    int nDig = 0;
    while(!match(origin)) {
        long score = scoreRecipes(elves, 2);

        // break score into its digits
        nDig = numDigits(score);
        long nn = score;
        int digs[nDig];
        for(int i = 1; i <= nDig; i++){
            digs[nDig - i] = nn % 10;
            nn /= 10;
        }

        for(int i = 0; i < nDig; i++){
            insert(origin->prev, digs[i]);
        }

        for(int i = 0; i < numElves; i++) {
            elves[i] = elfStep(elves[i]);
        }
        // printf("%d:\t", m);
        // walkArray(origin, elves[0], elves[1]);
        m += nDig;
    }
    m -= nDig;

    // walkArray(origin, 0, 0);
    // 20288092 is too high
    // todo: calculate the sequence length between start and (start of match)
    printf("part 2: %d", m - 3);

    return 0;
}
