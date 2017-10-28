all .c generated from scheme code except runtime.c

I don't know why need to handcode chicken.h while checken.c is generate from chicken.scm
don't konw why build process create chicken-config.h


build-version.c
buildtag.h

Above files are chicken runtime


basic library:
library.c
eval.c
expand.c
modules.c
extras.c - ? don't know whether needed, if this included it will cause error


Don't konw what is chicken-syntax.c, but it's required by gcc -static

When gcc -static to build independt programe, only .c is used

E.g.
gcc -static -Os -fomit-frame-pointer -DHAVE_CHICKEN_CONFIG_H runtime.c chicken-syntax.c build-version.c library.c eval.c expand.c modules.c simple-hello.c -o simple-hello  -lm

gcc -static -Os -fomit-frame-pointer -DHAVE_CHICKEN_CONFIG_H runtime.c chicken-syntax.c build-version.c library.c eval.c expand.c modules.c bar.c foo.c -o foo-full  -lm




debug:
software install(provide bin:wish, witch used by feathers witch is a buit-in debugger server):
sudo apt install tk

start up debugger server(default port is 9999, which I don't feel good):
feathers -port 9922

compile with debug info:
csc -d3 bar.scm foo.scm -o foo

env variable:
export CHICKEN_DEBUGGER=localhost:9922

run program (e.g.):
./foo


I found a issue in official doc: can't directly pass programe to the feather command
