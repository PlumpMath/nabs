nabs
====

Not A Build System - Set of tools written with GNU Guile to make (semi-)auto-configurable Makefiles (requires GNU Make 4). No autotools, no CMake.

What?
=====

GNU Make 4.0 introduces the support for Guile. This allows for extremely powerful (thanks, Scheme!) scripting *inside* a Makefile.
NABS is a set of Guile scripts meant to help create highly configurables Makefile while preserving the maintainer's sanity.

Why?
====

The traditional approaches to creating configurable Makefiles, like the autotools and CMake, have their own
[drawbacks](http://voices.canonical.com/jussi.pakkanen/2011/09/13/autotools/ "Jussi Pakkanen's development blog - Why GNU Autotools is not my favorite build system")
and [shortcomings](https://blog.flameeyes.eu/2008/01/im-not-an-happy-maintainer-working-with-cmake "I'm not an happy maintainer working with CMake").

I started out with plain Makefiles, then embraced the autotools. Eventually I moved to CMake, and after a while ended up favoring plain Makefiles again.

How?
====

Work in progress. You can't expect a final API right after the first commit.
