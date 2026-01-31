#include <stdio.h>
#include <stdlib.h>

/*
 * Sums the integers in a file
 */
int sumFile(FILE *fp) {
    int freqChange;
    int currentFreq = 0;

    // 1 int on line
    while (fscanf(fp, "%d", &freqChange) == 1) {
        currentFreq += freqChange;
        printf("Current freq is %d, freq change is %d\n", currentFreq, freqChange);
    }
    return currentFreq;
}

int main() {
    FILE *fp;
    char *mode = "r";
    char *inFile = "input";

    fp = fopen(inFile, mode);
    if (fp == NULL) {
        fprintf(stderr, "Can't open input file %s!\n", inFile);
        exit(1);
    }

    int resultingFreq = sumFile(fp);
    printf("Resulting frequency is %d\n", resultingFreq);

    fclose(fp);
}
