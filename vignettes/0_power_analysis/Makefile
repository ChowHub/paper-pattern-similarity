TMP_DIR=out
LOGDIR=out/log

BATCHES_TTL=10
N_REPS=1000
PVALS_FILE=$(TMP_DIR)/pvals-0.csv

QSUB=-r y -cwd -j y -V -o $(LOGDIR)/
QSUB_PARALLEL=-t 1-$(BATCHES_TTL)
	
all: clean errors_by_approach.html

parallel: clean batch-$(BATCHES_TTL)

clean: 
	rm -rf $(TMP_DIR)/p*

omg:
	./test.sh

$(TMP_DIR)/pars-%.csv:
	# creates all parameter files up to %
	# pass in (1) output file prefix, (2) batch number
	Rscript 0_create_sim_pars.R "$(TMP_DIR)/pars-" $(@:$(TMP_DIR)/pars-%.csv=%)

$(TMP_DIR)/pvals-%.csv: $(TMP_DIR)/pars-%.csv
	# pass in (1) output file, (2) input file, (3) repetitions
	qstat
	Rscript 1_run_power_sim.R $@ $^ $(N_REPS)

errors_by_approach.html: $(PVALS_FILE)
	R -e "library(rmarkdown); rmarkdown::render('errors_by_approach.Rmd', params=list(p_vals='$(PVALS_FILE)'))"

batch-%: $(TMP_DIR)/pars-%.csv
	# parallel will append SGE task id to pval file name
	qsub -N $@ $(QSUB) $(QSUB_PARALLEL) parallel.sh $(TMP_DIR)/pvals-
	#qsub -N stitch_$@ -hold_jid $@ $(QSUB)

$(TMP_DIR)/stitched.csv:
	Rscript 2_stitch_results.R $(TMP_DIR)/pvals- $@

