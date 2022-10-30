
CC = clang++
CFLAGS = `llvm-config --cxxflags`
LDFLAGS = `llvm-config --cxxflags --ldflags --libs`

all: cc
# sudo apt-get install llvm-dev
cc: cc.cpp c.tab.o c.lex.o ast.o
	$(CC) $^ -g $(LDFLAGS) -o $@

c.tab.o: c.tab.cpp c.tab.hpp
	$(CC) -c $^

c.lex.o: c.lex.cpp
	$(CC) -c $<

ast.o: ast.cpp ast.hpp
	$(CC) -c $(CFLAGS) ast.cpp

c.tab.cpp c.tab.hpp: c.y
	bison -o c.tab.cpp -d c.y

c.lex.cpp: c.l c.tab.hpp
	flex -o c.lex.cpp -l c.l

clean::
	rm -f c.tab.cpp c.tab.hpp c.lex.cpp cc c.output *.o *.gch
