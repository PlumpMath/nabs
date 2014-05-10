$(guile (add-to-load-path "$(lastword $(dir $(MAKEFILE_LIST)))/../.."))
$(guile (use-modules (nabs configure) (nabs predicates)))
$(guile (add-search-path bin /usr/bin /usr/local/bin))
$(guile (declare bin \
		  "C++ compiler" CXX \
		  prefix g++ (verifies (compiler-version>=? "4.9")) \
		  prefix clang++ (verifies (compiler-version>=? "3.4")) \
))

$(guile (configure-frontend))

all: hello

hello: hello.cc
	$(CXX) $(CXXFLAGS) $< -o $@
