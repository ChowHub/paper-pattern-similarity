TMPDIR=out
LOGDIR=out/log

BATCHES_TTL=0
N_REPS=1000

QSUB=-cwd -j y -o $(LOGDIR) -V                                                 
QSUB_PARALLEL=-t 1-$(BATCHES_TTL)   
	
all: clean $(TMPDIR)/pvals-0.csv

parallel: clean $(TMPDIR)/pars-$(BATCHES_TTL).csv batch out/stitched.csv

clean: 
	rm -rf $(TMPDIR)/*


$(TMPDIR)/pars-%.csv:
	Rscript 0_create_sim_pars.R "$(TMPDIR)/pars-" $(@:$(TMPDIR)/pars-%.csv=%)

$(TMPDIR)/pvals-%.csv: $(TMPDIR)/pars-%.csv
	Rscript 1_run_power_sim.R $@ $^ $(N_REPS)

batch:
	$(QSUB) $(QSUB_PARALLEL) parallel.sh $@ $^ $(N_REPS)
	$(MAKEFILE)

out/stitched.csv:
	Rscript 2_stitch_results.R $(TMPDIR)/pvals- $@

