$(guile (add-to-load-path "$(lastword $(dir $(MAKEFILE_LIST)))/../.."))
$(guile (use-modules (nabs configure) (nabs predicates)))
$(guile (declare "C++ compiler" "CXX" \
				 '("/usr/bin" "/usr/local/bin") \
				 'prefix "g++" \
				 (checks '(compiler-version>=? "4.9"))))

$(guile (configure-frontend))

all: hello

hello: hello.cc
	$(CXX) $(CXXFLAGS) $< -o $@
