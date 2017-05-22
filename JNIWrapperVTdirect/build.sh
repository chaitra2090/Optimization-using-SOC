#!/bin/bash
/opt/bin/gfortran -c -g -fno-underscoring -fPIC -O4 -o shared_modules.o shared_modules.f95

/opt/bin/gfortran -c -g -fno-underscoring -fPIC -O4 -o objfunc.o objfunc.f95

/opt/bin/gfortran -c -g -fno-underscoring -fPIC -O4 -o VTdirect.o VTdirect.f95

/opt/bin/gfortran -c -g -fno-underscoring -fPIC -O4 -o main.o simple_main.f95

gcc -c -D_REENTRANT -fPIC -I/home/chaitra/MSTC-Eng/support/java/linux64/jdk1.8.0_25/include/ -I/home/chaitra/MSTC-Eng/support/java/linux64/jdk1.8.0_25/include/linux/ -L/home/chaitra/MSTC-Eng/support/java/linux64/jdk1.8.0_25/bin/java -L/home/chaitra/MSTC-Eng/support/java/linux64/jdk1.8.0_25/jre/lib/amd64/server/ -c objFuncGlue.c -ljvm

/opt/bin/gfortran -O4 -o vtdirect shared_modules.o objfunc.o VTdirect.o main.o objFuncGlue.o -I/home/chaitra/MSTC-Eng/support/java/linux64/jdk1.8.0_25/include/ -I/home/chaitra/MSTC-Eng/support/java/linux64/jdk1.8.0_25/include/linux/ -L/home/chaitra/MSTC-Eng/support/java/linux64/jdk1.8.0_25/bin/java -L/home/chaitra/MSTC-Eng/support/java/linux64/jdk1.8.0_25/jre/lib/amd64/server/ -ljvm

javac objFuncJ.java


