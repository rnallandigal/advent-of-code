#include <cstdio>

#include <unordered_set>

int main(int argc, char ** argv) {
	FILE * in = fopen("in/day-1.2.in", "r");

	int cum_freq = 0, freq;
	std::unordered_set<int> seen;
	for(bool flag = 0; !flag; rewind(in)) {
		while(fscanf(in, "%d", &freq) != EOF) {
			cum_freq += freq;
			auto [it, b] = seen.emplace(cum_freq);
			if(b == false) {
				flag = 1;
				break;
			}
		}
	}
	printf("%d\n", cum_freq);
	fclose(in);
	return 0;
}
