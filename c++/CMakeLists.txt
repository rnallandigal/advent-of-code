cmake_minimum_required(VERSION 3.16)

set(MAIN aoc)
project(advent-of-code)

# top level properties
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED True)
set(CMAKE_BUILD_TYPE Debug)
set(CMAKE_USE_RELATIVE_PATHS True)

set(INCLUDE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/include)
set(CMAKE_CXX_FLAGS "-Wall -Wextra -pedantic -Wno-unused-parameter -Wno-missing-field-initializers")

# functions
function(import_libraries)
	foreach(name IN LISTS ARGN)
		string(TOUPPER ${name} uppername)
		add_library(lib${name} UNKNOWN IMPORTED)
		find_library(${uppername}_LIB ${name} REQUIRED)
		set_target_properties(lib${name} PROPERTIES IMPORTED_LOCATION ${${uppername}_LIB})
	endforeach()
endfunction()

include_directories(BEFORE ${INCLUDE_DIR})
import_libraries(fmt docopt benchmark benchmark_main pthread)

# main
add_executable(${MAIN} $<TARGET_OBJECTS:objects-0> ${MAIN}.cpp)
target_link_libraries(${MAIN} fmt docopt)

add_executable(${MAIN}-O3 $<TARGET_OBJECTS:objects-3> ${MAIN}.cpp)
target_link_libraries(${MAIN}-O3 fmt docopt)

# bench
add_executable(bench $<TARGET_OBJECTS:objects-3> bench.cpp)
target_link_libraries(bench fmt pthread benchmark benchmark_main)

# source files
set(SOURCE_FILES
	src/utils.cpp

	src/2018/01.cpp
	src/2018/02.cpp
	src/2018/03.cpp
	src/2018/04.cpp

	src/2020/01.cpp
	src/2020/02.cpp
	src/2020/03.cpp
	src/2020/04.cpp
	src/2020/05.cpp
	src/2020/06.cpp
	src/2020/07.cpp
	src/2020/08.cpp
	src/2020/09.cpp
	src/2020/10.cpp
	src/2020/11.cpp
	src/2020/12.cpp
	src/2020/13.cpp
	src/2020/14.cpp
	src/2020/15.cpp
	src/2020/16.cpp
	src/2020/17.cpp
	src/2020/18.cpp
	src/2020/19.cpp
	src/2020/20.cpp
	src/2020/21.cpp
	src/2020/22.cpp
	src/2020/23.cpp
	src/2020/24.cpp
	src/2020/25.cpp

	src/2021/01.cpp
	src/2021/02.cpp
	src/2021/03.cpp
	src/2021/04.cpp
	src/2021/05.cpp
	src/2021/06.cpp
	src/2021/07.cpp
	src/2021/08.cpp
	src/2021/09.cpp
	src/2021/10.cpp
	src/2021/11.cpp
	src/2021/12.cpp
	src/2021/13.cpp
	src/2021/14.cpp
	src/2021/15.cpp
	src/2021/16.cpp
	src/2021/17.cpp
	src/2021/18.cpp
	src/2021/19.cpp
	src/2021/20.cpp
	src/2021/21.cpp
	src/2021/22.cpp
	src/2021/23.cpp
	src/2021/24.cpp
	src/2021/25.cpp

	src/2022/01.cpp
	src/2022/02.cpp
	src/2022/03.cpp
	src/2022/04.cpp
	src/2022/05.cpp
	src/2022/06.cpp
	src/2022/07.cpp
	src/2022/08.cpp
	src/2022/09.cpp
	src/2022/10.cpp
	src/2022/11.cpp
	src/2022/12.cpp
	src/2022/13.cpp
	src/2022/14.cpp
	src/2022/15.cpp
	src/2022/16.cpp
	src/2022/17.cpp
	src/2022/18.cpp
	src/2022/19.cpp
	src/2022/20.cpp
	src/2022/21.cpp
	src/2022/22.cpp
	src/2022/23.cpp
	src/2022/24.cpp
	src/2022/25.cpp
)

# object libs
add_library(objects-0 OBJECT ${SOURCE_FILES})
target_compile_options(objects-0 PRIVATE -O0)

add_library(objects-3 OBJECT ${SOURCE_FILES})
target_compile_options(objects-3 PRIVATE -O3)

# entr commands to build/run/test/bench on code change
function(add_watch_target name cmd)
	add_custom_target(watch-${name}
		COMMAND bash -c "while true; do fd -e cpp -e h | entr -ccds \"${cmd}\"; [ $? -eq 2 ] || break; done"
		WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
		VERBATIM
	)
endfunction()

set(makecmd "(cd ${CMAKE_CURRENT_BINARY_DIR} && time make -j`nproc` ${MAIN})")
set(makecmd-all "(cd ${CMAKE_CURRENT_BINARY_DIR} && time make -j`nproc`)")
add_watch_target(rebuild ${makecmd})
add_watch_target(rebuild-all ${makecmd-all})
add_watch_target(rerun "${makecmd} && ${CMAKE_CURRENT_BINARY_DIR}/aoc run 2022")
