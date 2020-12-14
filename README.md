Service ORiented Computing EnviRonment (SORCER) is a Java-based network-centric computing platform.
SORCER provides a service oriented architecture, which enables the implementation of parallel algorithms
in a dynamic distributed computing environment. SORCER is often used for multidisciplinary aircraft design
analysis and optimization. However, the current approach often assigns intense optimization algorithms to
run entirely on single overloaded nodes, rather than evenly distributing the workload. The goal of this work
is to provide lower-level optimization algorithms as integrated SORCER services and study the overhead
of doing so. VTDIRECT95, a Fortran 95 implementation of D. R. Jones’ algorithm DIRECT, is a highly
parallelizable derivative-free deterministic global optimization algorithm. QNSTOP is a parallel quasiNewton algorithm for stochastic optimization problems. The potential benefit of integrating VTDIRECT95
and QNSTOP into the SORCER framework is to provide dynamic load balancing among computational
resources at the optimization level, resulting in a dynamically scalable process.
