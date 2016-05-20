if (Sys.getenv("RONDO") != "") {
    print("On Rondo, disabling multithreaded BLAS and OMP")
    library(RhpcBLASctl)
    blas_set_num_threads(1)
    omp_set_num_threads(1)
}

#### -- Packrat Autoloader (version 0.4.6-19) -- ####
source("packrat/init.R")
#### -- End Packrat Autoloader -- ####
