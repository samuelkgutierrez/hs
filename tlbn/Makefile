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

.PHONY: all clean test
.SUFFIXES: .lhs .o .tex

TARGET      := Main
APP_NAME    := TLBN
#LHS_SCRIPTS := $(wildcard *.lhs)
TEX         := $(LHS_SCRIPTS:.lhs=.tex)

GHC_FLAGS := -j2 -Wall

#all: $(TARGET) $(TEX)
all: $(TARGET)

$(TARGET): clean
	ghc --make $(GHC_FLAGS) $@ -o $(APP_NAME)

%.tex: %.lhs
	@ \
	echo "######################### building docs for $<" && \
	lhs2TeX $< -o $@ && \
	latexmk -pdf $@

clean:
	rm -f *.o *.hi $(APP_NAME) $(TEX) *.ptb
	#latexmk -f -C $(TARGETS)

test:
	@ \
	for testInput in `ls -1 ./inputs/*.TLBN | sort -n` ; do \
		./$(APP_NAME) $$testInput; \
		echo; \
	done
