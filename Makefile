LOGDIR=log
BATCHES_TTL=4

QSUB=-cwd -j y -o $(LOGDIR) -V                                                 
QSUB_PARALLEL=-t 1-$(BATCHES_TTL)   

test:
	R -e "library(testthat); test_dir('tests/testthat')"

simpow:
	$(MAKE) -C vignettes/0_power_analysis
	
	
