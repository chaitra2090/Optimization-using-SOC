#!/bin/bash
/opt/bin/gfortran -c -g -fno-underscoring -fPIC -O4 -fmax-errors=5 -ffpe-trap=invalid,overflow,zero -std=f2003 -Wall -Wno-maybe-uninitialized -o real_precision.o real_precision.f95

/opt/bin/gfortran -c -g -fno-underscoring -fPIC -O4 -fmax-errors=5 -ffpe-trap=invalid,overflow,zero -std=f2003 -Wall -Wno-maybe-uninitialized -o zigarray.o zigarray.f95

/opt/bin/gfortran -c -g -fno-underscoring -fPIC -O4 -fmax-errors=5 -ffpe-trap=invalid,overflow,zero -std=f2003 -Wall -Wno-maybe-uninitialized -o objfunc.o objfunc.f95

/opt/bin/gfortran -c -g -fno-underscoring -fPIC -O4 -fmax-errors=5 -ffpe-trap=invalid,overflow,zero -std=f2003 -Wall -Wno-maybe-uninitialized -fopenmp -o qnstopp.o qnstopp.f95

/opt/bin/gfortran -c -g -fno-underscoring -fPIC -O4 -fmax-errors=5 -ffpe-trap=invalid,overflow,zero -std=f2003 -Wall -Wno-maybe-uninitialized -o pmain.o sample_main_p.f95

/opt/bin/gfortran -c -g -fno-underscoring -fPIC -O4 -fmax-errors=5 -ffpe-trap=invalid,overflow,zero -std=f2003 -Wall -Wno-maybe-uninitialized -o blas.o blas.f

/opt/bin/gfortran -c -g -fno-underscoring -fPIC -O4 -fmax-errors=5 -ffpe-trap=invalid,overflow,zero -std=f2003 -Wall -Wno-maybe-uninitialized -o lapack.o lapack.f

gcc -c -D_REENTRANT -fPIC -I/home/chaitra/MSTC-Eng/support/java/linux64/jdk1.8.0_25/include/ -I/home/chaitra/MSTC-Eng/support/java/linux64/jdk1.8.0_25/include/linux/ -L/home/chaitra/MSTC-Eng/support/java/linux64/jdk1.8.0_25/bin/java -L/home/chaitra/MSTC-Eng/support/java/linux64/jdk1.8.0_25/jre/lib/amd64/server/ -c objFuncGlue.c -ljvm

/opt/bin/gfortran -O4 -fmax-errors=5 -ffpe-trap=invalid,overflow,zero -std=f2003 -Wall -Wno-maybe-uninitialized -fopenmp -o qnstopp real_precision.o zigarray.o qnstopp.o objfunc.o pmain.o blas.o lapack.o objFuncGlue.o -I/home/chaitra/MSTC-Eng/support/java/linux64/jdk1.8.0_25/include/ -I/home/chaitra/MSTC-Eng/support/java/linux64/jdk1.8.0_25/include/linux/ -L/home/chaitra/MSTC-Eng/support/java/linux64/jdk1.8.0_25/bin/java -L/home/chaitra/MSTC-Eng/support/java/linux64/jdk1.8.0_25/jre/lib/amd64/server/ -ljvm

javac objFuncJ.java


