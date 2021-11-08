#include <vector>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <tuple>

#include <fmt/format.h>

#include "utils.h"

namespace aoc2020::day20 {

using vertex_t = std::pair<int, int>;
using tile_t = std::vector<std::string>;
using jigsaw_t = std::unordered_map<int, tile_t>;

static const int T = 8;

enum TransformID {
	ORIGINAL = 0, FLIP_X = 1, FLIP_Y = 2, ROTATE_180 = 3,
	FLIP_DESC_DIAG = 4, ROTATE_LEFT = 5, ROTATE_RIGHT = 6, FLIP_ASC_DIAG = 7
};

enum BorderID {
	TOP = 0, BOTTOM = 1, FLIPPED_TOP = 2, FLIPPED_BOTTOM = 3,
	LEFT = 4, FLIPPED_LEFT = 5, RIGHT = 6, FLIPPED_RIGHT = 7
};

#define TRANSFORM_FN(a, b) \
	[](int x, int y, int D) -> std::pair<int, int> { return { a, b }; }
std::array<std::function<std::pair<int, int>(int, int, int)>, T>
transforms {						// transform    -> border
	TRANSFORM_FN( x    , y     ),	// original     -> top
	TRANSFORM_FN( x    , D - y ),	// flip x-axis  -> bottom
	TRANSFORM_FN( D - x, y     ),	// flip y-axis  -> flipped top
	TRANSFORM_FN( D - x, D - y ),	// rotate 180   -> flipped bottom
	TRANSFORM_FN( y    , x     ),	// flip y = -x  -> left
	TRANSFORM_FN( y    , D - x ),	// rotate left  -> flipped left, right
	TRANSFORM_FN( D - y, x     ),	// rotate right -> right, flipped left
	TRANSFORM_FN( D - y, D - x )	// flip y = x   -> flipped right
};
std::array<int, T> inv { 0, 1, 2, 3, 4, 6, 5, 7 };
std::array<std::array<int, T>, T> compose {{
	{ 0, 1, 2, 3, 4, 5, 6, 7 },
	{ 1, 0, 3, 2, 6, 7, 4, 5 },
	{ 2, 3, 0, 1, 5, 4, 7, 6 },
	{ 3, 2, 1, 0, 7, 6, 5, 4 },
	{ 4, 5, 6, 7, 0, 1, 2, 3 },
	{ 5, 4, 7, 6, 2, 3, 0, 1 },
	{ 6, 7, 4, 5, 1, 0, 3, 2 },
	{ 7, 6, 5, 4, 3, 2, 1, 0 }
}};

uint64_t calc_border(tile_t const & tile, int t) {
	uint64_t ret = 0, m = tile.size();
	for(uint64_t i = 0; i < m; i++) {
		auto [x, y] = transforms[t](i, 0, m - 1);
		ret |= ((uint64_t)(tile[y][x] == '#')) << i;
	}
	return ret;
}

auto construct_graph(jigsaw_t const & jigsaw) {
	std::unordered_map<vertex_t, vertex_t> edges;
	std::unordered_map<uint64_t, std::vector<vertex_t>> bordermap;
	std::unordered_map<int, std::unordered_set<int>> corners;

	for(auto [id, tile] : jigsaw) {
		for(int t = 0; t < T; t++) {
			bordermap[calc_border(tile, t)].emplace_back(id, t);
		}
	}

	for(auto const & [_, neighbors] : bordermap) {
		if(neighbors.size() > 1) {
			edges.emplace(neighbors.front(), neighbors.back());
			edges.emplace(neighbors.back(), neighbors.front());
		} else {
			auto [id, t] = neighbors.front();
			corners[id].insert(t);
		}
	}
	
	std::erase_if(corners, [](auto const & entry) {
		return entry.second.size() != 4;
	});
	return std::tuple(edges, corners);
}

int orient_corner(std::unordered_set<int> const & unmatched) {
	// orient based on existing unmatched borders
	if(unmatched.count(LEFT) && unmatched.count(TOP)) return ORIGINAL;
	if(unmatched.count(TOP) && unmatched.count(RIGHT)) return ROTATE_LEFT;
	if(unmatched.count(RIGHT) && unmatched.count(BOTTOM)) return FLIP_ASC_DIAG;
	if(unmatched.count(BOTTOM) && unmatched.count(LEFT)) return ROTATE_RIGHT;
	fmt::print("invalid input - unmatched borders are not adjacent");
	exit(1);
}

std::vector<vertex_t> arrange_tiles(jigsaw_t const & jigsaw) {
	auto [edges, corners] = construct_graph(jigsaw);

	int N = (int)jigsaw.size(), D = (int)std::sqrt(N);
	std::vector<vertex_t> tilemap;

	// Place corner
	auto [corner_id, unmatched_borders] = *corners.begin();
	tilemap.emplace_back(corner_id, orient_corner(unmatched_borders));
	
	// Place remaining tiles
	for(int i = 1; i < N; i++) {
		auto [prev_id, prev_t] = tilemap[i - (i % D ? 1 : D)];
		auto [prev_target_t, curr_target_t] = i % D
			? std::pair(RIGHT, LEFT) : std::pair(BOTTOM, TOP);

		int prev_border_t = compose[prev_target_t][inv[prev_t]];
		auto [curr_id, curr_border_t] = edges[std::pair(prev_id, prev_border_t)];
		int curr_t = compose[inv[curr_border_t]][curr_target_t];
		tilemap.emplace_back(curr_id, curr_t);
	}
	return tilemap;
}

tile_t assemble_image(
	jigsaw_t const & jigsaw,
	std::vector<vertex_t> const & tilemap
) {
	int N = (int)jigsaw.size(), D = (int)std::sqrt(N),
		M = (int)jigsaw.begin()->second.size();

	tile_t image(D * (M - 2), std::string(D * (M - 2), '\0'));
	for(int i = 0; i < D; i++) {
		for(int j = 0; j < D; j++) {
			auto [id, t] = tilemap[D * i + j];
			auto & tile = jigsaw.find(id)->second;
			for(int y = 1; y < M - 1; y++) {
				for(int x = 1; x < M - 1; x++) {
					auto [dst_x, dst_y] = transforms[t](x, y, M - 1);
					image[(M - 2) * i + dst_y - 1][(M - 2) * j + dst_x - 1]
						= tile[y][x];
				}
			}
		}
	}
	return image;
}

int count_roughness(tile_t & image, tile_t const & monster) {
	int N = (int)image.size(), X = (int)monster[0].size(), Y = (int)monster.size();

	int min_roughness = N * N;
	for(int t = 0; t < T; t++) {
		int final_t = compose[inv[std::max(0, t - 1)]][t];

		tile_t tmpimg(N, std::string(N, '\0'));
		for(int y = 0; y < N; y++) {
			for(int x = 0; x < N; x++) {
				auto [x2, y2] = transforms[final_t](x, y, N - 1);
				tmpimg[y2][x2] = image[y][x];
			}
		}
		std::swap(image, tmpimg);

		for(int iy = 0; iy < N - Y; iy++) {
			for(int ix = 0; ix < N - X; ix++) {
				bool matched = true;
				for(int my = 0; matched && my < Y; my++) {
					for(int mx = 0; matched && mx < X; mx++) {
						if(monster[my][mx] == '#' && image[iy + my][ix + mx] != '#') {
							matched = false;
						}
					}
				}
				if(!matched) continue;

				for(int my = 0; my < Y; my++) {
					for(int mx = 0; mx < X; mx++) {
						if(monster[my][mx] == '#') {
							image[iy + my][ix + mx] = 'O';
						}
					}
				}
			}
		}

		int roughness = 0;
		for(int y = 0; y < N; y++) {
			for(int x = 0; x < N; x++) {
				if(image[y][x] == '#') roughness++;
				else if(image[y][x] == 'O') image[y][x] = '#';
			}
		}
		min_roughness = std::min(min_roughness, roughness);
	}
	return min_roughness;
}

jigsaw_t input(std::string const & in) {
	jigsaw_t jigsaw;
	for(auto tiletext : aoc::split(in, "\n\n")) {
		auto lines = aoc::split(tiletext, "\n");
		jigsaw.emplace(
			std::stoi(lines[0].substr(5, lines[0].size() - 6)),
			std::vector<std::string>(lines.begin() + 1, lines.end())
		);
	}
	return jigsaw;
}

std::string part1(std::string const & in) {
	auto [_, corners] = construct_graph(input(in));
	int64_t ret = 1;
	for(auto [id, _] : corners) {
		ret *= id;
	}
	return fmt::format("{}", ret);
}

std::string part2(std::string const & in) {
	auto jigsaw = input(in);
	auto tilemap = arrange_tiles(jigsaw);
	auto image = assemble_image(jigsaw, tilemap);

	std::vector<std::string> monster {
		"                  # ", 
		"#    ##    ##    ###",
		" #  #  #  #  #  #   "
	};
	return fmt::format("{}", count_roughness(image, monster));
}

}	// namespace aoc2020::day20
