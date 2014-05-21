define nabs :=
(add-to-load-path "$(lastword $(dir $(MAKEFILE_LIST)))/../..")
(use-modules (nabs configure) (nabs predicates))
(add-search-path bin /usr/bin /usr/local/bin)
(declare bin
		  "C++ compiler" CXX
		  prefix g++ (verifies (compiler-version>=? "4.9") (compiles? "-x c++" "" ""))
		  prefix clang++ (verifies (compiler-version>=? "3.4") (compiles? "-x c++" "" ""))
)
(configure-frontend)
endef

$(guile $(nabs))

all: hello

hello: hello.cc
	$(CXX) $(CXXFLAGS) $< -o $@
