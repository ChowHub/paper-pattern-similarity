TMPDIR=out
LOGDIR=out/log

BATCHES_TTL=0
N_REPS=1000
PVALS_FILE=$(TMPDIR)/pvals-0.csv

QSUB=-cwd -j y -o $(LOGDIR) -V                                                 
QSUB_PARALLEL=-t 1-$(BATCHES_TTL)   
	
all: clean errors_by_approach.html

parallel: clean $(TMPDIR)/pars-$(BATCHES_TTL).csv batch out/stitched.csv

clean: 
	rm -rf $(TMPDIR)/*

$(TMPDIR)/pars-%.csv:
	# creates all parameter files up to %
	# pass in (1) output file prefix, (2) batch number
	Rscript 0_create_sim_pars.R "$(TMPDIR)/pars-" $(@:$(TMPDIR)/pars-%.csv=%)

$(TMPDIR)/pvals-%.csv: $(TMPDIR)/pars-%.csv
	# pass in (1) output file, (2) input file, (3) repetitions
	Rscript 1_run_power_sim.R $@ $^ $(N_REPS)

errors_by_approach.html: $(PVALS_FILE)
	R -e "library(rmarkdown); rmarkdown::render('errors_by_approach.Rmd', params=list(p_vals='$(PVALS_FILE)'))"

batch:
	# TODO finish implementing
	$(QSUB) $(QSUB_PARALLEL) parallel.sh $@ $^ $(N_REPS)
	#$(MAKEFILE)

$(TMPDIR)/stitched.csv:
	Rscript 2_stitch_results.R $(TMPDIR)/pvals- $@

