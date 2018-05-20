all: kilo

kilo: kilo.cpp termconfig.cpp
	$(CXX) $(CXXFLAGS) -std=c++1z -o $@ $^ -Wall -W -pedantic 

clean:
	rm kilo
