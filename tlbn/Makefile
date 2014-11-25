#
# Copyright (c) 2014 Samuel K. Gutierrez All rights reserved.
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

# A makefile that helps generate executables and documentation from litterate
# Haskell scripts.

SHELL = /bin/bash

.PHONY: all clean test doc
.SUFFIXES: .lhs .o .tex

TARGET      := Main
APP_NAME    := TLBN
LHS_SCRIPTS := $(wildcard *.lhs)
TEX         := $(LHS_SCRIPTS:.lhs=.tex)
DOC_NAME    := tlbn.pdf
TEST_LOG    := testlog.txt
TEST_TEX    := $(TEST_LOG:.txt=.tex) 

#GHC_FLAGS := -j2 -Wall
GHC_FLAGS := -j2

#all: $(TARGET) $(TEX)
all: $(TARGET)

$(TARGET):
	ghc --make $(GHC_FLAGS) $@ -o $(APP_NAME)

%.tex: %.lhs
	@ \
	echo "######################### building tex for $<" && \
	lhs2TeX $< -o $@

doc: $(TEX) $(TEST_TEX) 
	latexmk -pdf Main.tex && \
	mv Main.pdf $(DOC_NAME)

$(TEST_TEX):
	@ \
	make test && \
	echo '\section{Test Results}' > $(TEST_TEX) && \
	echo '\begin{verbatim}' >> $(TEST_TEX) && \
	cat $(TEST_LOG) >> $(TEST_TEX) && \
	echo '\end{verbatim}' >> $(TEST_TEX);

test: $(TARGET)
	@ \
	echo -e "# Test Log\n" > $(TEST_LOG); \
	for testInput in `ls -1 ./inputs/*.TLBN | sort -n` ; do \
		./$(APP_NAME) $$testInput >> $(TEST_LOG) 2>&1; \
		echo >> $(TEST_LOG); \
	done; \
	echo "Test Log Written to: $(TEST_LOG)";

clean:
	rm -f *.o *.hi $(APP_NAME) $(DOC_NAME) $(TEX) *.ptb *.aux *.log
	rm -f $(TEST_LOG) $(TEST_TEX) 
	latexmk -f -C Main.tex

