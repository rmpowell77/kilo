all: kilo

kilo: kilo.cpp
	$(CXX) $(CXXFLAGS) -std=c++1z -o $@ $< -Wall -W -pedantic 

clean:
	rm kilo
