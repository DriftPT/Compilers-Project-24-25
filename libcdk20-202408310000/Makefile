#=====================================================================
#==========      TWEAK THE 'ROOT' VARIABLE IF NECESSARY     ==========
#=====================================================================

# install in user's home directory
ROOT = ${HOME}/compiladores/root

# if installing in system dirs, use the following line instead
#ROOT= 

#=====================================================================
#==========      DO NOT CHANGE ANYTHING AFTER THIS LINE     ==========
#=====================================================================

LIBNAME=cdk

CXXFLAGS=-I. -Isrc -Isrc/cdk -ggdb -Wall -Wextra -std=c++23 -pedantic -msse2 -mfpmath=sse
#CXXFLAGS=-O3 -I. -ggdb -Wall -std=c++23 -pedantic

INST_INC_DIR = $(ROOT)/usr/include/$(LIBNAME)
INST_BIN_DIR = $(ROOT)/usr/bin
INST_LIB_DIR = $(ROOT)/usr/lib
INST_DOC_DIR = $(ROOT)/usr/share/doc/packages/$(LIBNAME)

SRC_CPP = $(wildcard src/*.cpp) $(wildcard src/cdk/*.cpp) $(wildcard src/cdk/ast/*.cpp) \
    $(wildcard src/cdk/emitters/*.cpp) $(wildcard src/cdk/targets/*.cpp) \
    $(wildcard src/cdk/types/*.cpp)
	        
OFILES = $(SRC_CPP:%.cpp=%.o)

%.o:: %.cpp
	$(CXX) $(CXXFLAGS) -c $< -o $@

all: src/cdk/ast/all.h src/cdk/ast/visitor_decls.h lib$(LIBNAME).a docs

docs: doc/.docsdone

src/cdk/ast/all.h: 
	(cd src/cdk; ../../bin/cdk ast --decls ast --cdk > ast/all.h)

src/cdk/ast/visitor_decls.h:
	(cd src/cdk; ../../bin/cdk ast --decls target --cdk > ast/visitor_decls.h)

doc/.docsdone: $(LIBNAME).doxyfile
	doxygen $<
	touch $@

$(LIBNAME).doxyfile: $(SRC_CPP)

lib$(LIBNAME).a: $(OFILES)
	ar cq $@ $^

clean:
	rm -f lib$(LIBNAME).a $(OFILES) src/cdk/ast/all.h src/cdk/ast/visitor_decls.h

install: all
	mkdir -p $(INST_LIB_DIR) $(INST_DOC_DIR) $(INST_BIN_DIR)
	mkdir -p $(INST_INC_DIR)/ast $(INST_INC_DIR)/emitters $(INST_INC_DIR)/targets $(INST_INC_DIR)/types
	cp -a bin/* $(INST_BIN_DIR)
	cp -a src/cdk/*.h $(INST_INC_DIR)
	cp -a src/cdk/ast/*.h $(INST_INC_DIR)/ast
	cp -a src/cdk/emitters/*.h $(INST_INC_DIR)/emitters
	cp -a src/cdk/targets/*.h $(INST_INC_DIR)/targets
	cp -a src/cdk/types/*.h $(INST_INC_DIR)/types
	cp -a lib$(LIBNAME).a $(INST_LIB_DIR)
	#DOC cp -a doc/. $(INST_DOC_DIR)/
	# remove these files: the actual ones will be provided by each compiler
	rm $(INST_INC_DIR)/targets/basic_ast_visitor.h

depend:
	$(CXX) $(CXXFLAGS) -MM $(SRC_CPP) > .makedeps

-include .makedeps

#=====================================================================
#==========             T  H  E         E  N  D             ==========
#=====================================================================
