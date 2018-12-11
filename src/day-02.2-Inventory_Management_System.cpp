#include <cstdio>
#include <cstring>

#include <string>
#include <unordered_set>

// n = number of strings, k = (max) length of string
// Naive: O(n * n * k) time, O(n * k) space
// Optimized: O(n * k * k) time, O(n * k * k) space
// last multiple of k in time bound is for computing hash of substituted string
int main(int argc, char ** argv) {
	FILE * in = fopen("in/day-02.in", "r");

	std::unordered_set<std::string> ids;
	bool flag = false;
	char line[64];
	while(!flag && fgets(line, sizeof(line), in) != nullptr) {
		int len = strlen(line) - 1;
		for(int i = 0; i < len; i++) {
			char tmp = line[i];
			line[i] = '_';
			auto [_, b] = ids.emplace(line, len);
			if(b == false) {
				printf("%.*s%.*s\n", i, line, len - i - 1, &line[i + 1]);
				flag = true;
				break;
			}
			line[i] = tmp;
		}
	}
	fclose(in);
	return 0;
}
