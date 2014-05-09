This is to demonstrate the use of nabs for out-of-source builds.

$ mkdir build  # or wherever
$ cd build
$ make -f ../Makefile .configure  # will create a Makefile in the current directory
$ make
$ ./hello
