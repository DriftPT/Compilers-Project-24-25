#=====================================================================
#==========    TWEAK THE 'PREFIX' VARIABLE IF NECESSARY     ==========
#=====================================================================

# install in user's home directory
PREFIX = ${HOME}/compiladores/root

# if installing in system dirs, use the following line instead
#PREFIX= 

#=====================================================================
#==========      DO NOT CHANGE ANYTHING AFTER THIS LINE     ==========
#=====================================================================

LIBNAME=rts

INST_INC_DIR = $(PREFIX)/usr/include/$(LIBNAME)
INST_LIB_DIR = $(PREFIX)/usr/lib
INST_DOC_DIR = $(PREFIX)/usr/share/doc/packages

FORMAT=elf32

COMMON_FLAGS = -O3 -ggdb -Wall -Wextra -pedantic -nostdlib -fno-builtin -m32 -msse2 -mfpmath=sse
CFLAGS       = $(COMMON_FLAGS) -std=c23
CXXFLAGS     = $(COMMON_FLAGS) -std=c++23

SRC_C   = $(wildcard *.c)
SRC_CPP = $(wildcard *.cpp)
SRC_ASM = $(wildcard *.asm)

OFILES = $(SRC_C:%.c=%.o) $(SRC_CPP:%.cpp=%.o) $(SRC_ASM:%.asm=%.o)

YASM=yasm -f$(FORMAT) -gDWARF2

%.o: %.asm
	$(YASM) $<

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

%.o: %.cpp
	$(CXX) $(CXXFLAGS) -c $< -o $@

all: lib$(LIBNAME).a

lib$(LIBNAME).a: $(OFILES)
	ar cq $@ $^

install: all
	mkdir -p $(INST_INC_DIR) $(INST_LIB_DIR) $(INST_DOC_DIR)
	cp -a lib$(LIBNAME).a $(INST_LIB_DIR)

clean:
	rm -f lib$(LIBNAME).a $(OFILES) a.out core

#=====================================================================
#==========             T  H  E         E  N  D             ==========
#=====================================================================
