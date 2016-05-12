LOGDIR=log
BATCHES_TTL=4

QSUB=-cwd -j y -o $(LOGDIR) -V                                                 
QSUB_PARALLEL=-t 1-$(BATCHES_TTL)   

VIGNETTES=$(addprefix test_vignettes_, 0_power_analysis)

test: test_R $(VIGNETTES)

test_R:
	Rscript -e "source('tests/testthat.R', chdir=TRUE)" na --bootstrap-packrat

test_vignettes_%:
	$(MAKE) -C vignettes/$* N_REPS=1 N_BATCHES=1

simpow:
	$(MAKE) -C vignettes/0_power_analysis
