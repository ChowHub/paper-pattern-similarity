TMP_DIR=out
LOGDIR=$(TMP_DIR)/log

N_BATCHES=18
N_REPS=1000

QSUB=-sync y -r y -cwd -j y -V -o $(LOGDIR)/
QSUB_PARALLEL=-t 1-$(N_BATCHES)

# Since one parameter csv is created per batch, use a wildcard to find
# all parameter files created, and derive correspond pvalue file name
PAR_BATCH=$(notdir $(wildcard $(TMP_DIR)/pars-*))
PVAL_BATCH=$(addprefix $(TMP_DIR)/, $(PAR_BATCH:pars-%=pvals-%))

# The last parameter file name, invokes rule to make all parameter files
PAR_FILE:=$(TMP_DIR)/pars-$(N_BATCHES).csv

# Targets ---------------------------------------------------------------------
all: clean $(PAR_FILE) errors_by_approach.html

parallel: clean batch-$(N_BATCHES) $(TMP_DIR)/stitched.csv

clean: 
	rm -rf $(TMP_DIR)/*
	mkdir -p $(LOGDIR)

$(TMP_DIR)/pars-%.csv:
	# creates all parameter files up to %
	# pass in (1) output file prefix, (2) total batch number
	Rscript 0_create_sim_pars.R "$(TMP_DIR)/pars-" $(@:$(TMP_DIR)/pars-%.csv=%)

$(TMP_DIR)/pvals-%.csv: $(TMP_DIR)/pars-%.csv
	# pass in (1) output file, (2) input file, (3) repetitions
	Rscript 1_run_power_sim.R $@ $^ $(N_REPS)

$(TMP_DIR)/stitched.csv: $(PVAL_BATCH)
	Rscript 2_stitch_results.R $^ $@

errors_by_approach.html: $(TMP_DIR)/stitched.csv
	R -e "library(rmarkdown); rmarkdown::render('errors_by_approach.Rmd', params=list(p_vals='$^'))"

batch-%: $(TMP_DIR)/pars-%.csv
	# parallel will append SGE task id to pval file name
	qsub -N $@ $(QSUB) $(QSUB_PARALLEL) parallel.sh $(TMP_DIR)/pvals-
