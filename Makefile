# GNU Make Special Characters
COMMA := ,
EMPTY :=
SPACE := $(EMPTY) $(EMPTY)

# Compiler Variables
CXX = g++
WARNINGS = -Wall -Werror -Wfatal-errors -Wextra -pedantic -Wno-unused-parameter -Wno-unused-variable -Wno-unused-but-set-variable
CXXFLAGS = -g -O0 -std=c++17 -I$(SRCDIR) $(WARNINGS)

# project directory variables
INDIR = in
OUTDIR = out
SRCDIR = src
BINDIR = bin

TMPDIRS = $(OUTDIR) $(BINDIR)
SRCDIRS = $(SRCDIR) $(INDIR)

# project file searches
ALL_SRCS = $(patsubst $(SRCDIR)/%.cpp,%,$(wildcard $(SRCDIR)/*.cpp))
EXCLUDE_SRCS =

expand_id = $(foreach num,$(1),$(filter day-$(num)-%,$(2)))
BUILD_SRCS = $(patsubst %,$(BINDIR)/%,$(filter-out $(call expand_id,$(EXCLUDE_SRCS),$(ALL_SRCS)),$(ALL_SRCS)))

# vpath directives
vpath %.cpp $(SRCDIR) $(TESTDIR)

# -------------
# Rules
# -------------
all: $(BUILD_SRCS)

# rule to create new workspace
workspace:
	@mkdir -p $(TMPDIRS) $(SRCDIRS)

$(BUILD_SRCS): $(BINDIR)/%: $(SRCDIR)/%.cpp
	$(CXX) $(CXXFLAGS) $< -o $@

# rules for cleaning workspace
clean:
	rm -r $(TMPDIRS)
	mkdir $(TMPDIRS)

.PHONY: all workspace clean
.SUFFIXES:
