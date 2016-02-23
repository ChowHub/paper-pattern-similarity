LOGDIR=log
BATCHES_TTL=4

QSUB=-cwd -j y -o $(LOGDIR) -V                                                 
QSUB_PARALLEL=-t 1-$(BATCHES_TTL)   

test:
	Rscript -e "source('tests/testthat.R', chdir=TRUE)" na --bootstrap-packrat

simpow:
	$(MAKE) -C vignettes/0_power_analysis
	
	
