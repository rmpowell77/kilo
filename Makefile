all: kilo

kilo: kilo.cpp
	$(CXX) -std=c++1z -o $@ $< -Wall -W -pedantic 

clean:
	rm kilo
