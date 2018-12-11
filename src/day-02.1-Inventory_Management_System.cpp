#include <cstdio>
#include <cstring>

#include <unordered_map>

int main(int argc, char ** argv) {
	FILE * in = fopen("in/day-02.in", "r");

	int num_twos = 0, num_threes = 0;
	char line[64];
	while(fgets(line, sizeof(line), in) != nullptr) {
		std::unordered_map<char, int> alphabet;
		for(int i = 0; line[i] != '\n' && line[i] != '\0'; i++) {
			alphabet[line[i]]++;
		}

		bool two = false, three = false;
		for(auto [_, v] : alphabet) {
			if(v == 2) two = true;
			if(v == 3) three = true;
		}
		num_twos += two;
		num_threes += three;
	}
	printf("%d\n", num_twos * num_threes);
	fclose(in);
	return 0;
}
