#include <cstdio>

int main(int argc, char ** argv) {
	FILE * in = fopen("in/day-01.in", "r");

	int cum_freq = 0, freq;
	while(fscanf(in, "%d", &freq) != EOF) {
		cum_freq += freq;
	}
	printf("%d\n", cum_freq);
	return 0;
}
