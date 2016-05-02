LOGDIR=log
BATCHES_TTL=4

QSUB=-cwd -j y -o $(LOGDIR) -V                                                 
QSUB_PARALLEL=-t 1-$(BATCHES_TTL)   

test: test_R test_vignettes

test_R:
	Rscript -e "source('tests/testthat.R', chdir=TRUE)" na --bootstrap-packrat

test_vignettes:
	$(MAKE) -C vignettes/0_power_analysis N_REPS=1

simpow:
	$(MAKE) -C vignettes/0_power_analysis
