TMP_DIR=out
LOGDIR=$(TMP_DIR)/log
# Parameters each batch will run for N_REPS
PAR_FILE:=$(TMP_DIR)/pars.csv

N_BATCHES=10
N_REPS=100

QSUB=-sync y -r y -cwd -j y -V -o $(LOGDIR)/
QSUB_PARALLEL=-t 1-$(N_BATCHES)

# p-value files corresponding to batch number
NUMBERS:=$(shell seq 1 $(N_BATCHES))
PVAL_FILES:=$(NUMBERS:%=$(TMP_DIR)/pvals-%.csv)

# Targets ---------------------------------------------------------------------
all: clean $(TMP_DIR)/errors_by_approach.html

parallel: clean batch-$(N_BATCHES) $(TEMP_DIR)/errors_by_approach.html

clean: 
	rm -rf $(TMP_DIR)/*
	mkdir -p $(LOGDIR)

$(PAR_FILE):
	# creates all parameter files up to %
	# pass in (1) output file prefix, (2) total batch number
	Rscript 0_create_sim_pars.R $@

$(TMP_DIR)/pvals-%.csv: $(PAR_FILE)
	# pass in (1) output file, (2) input file, (3) repetitions
	Rscript 1_run_power_sim.R $@ $^ $(N_REPS)

$(TMP_DIR)/stitched.csv: $(PVAL_FILES)
	Rscript 2_stitch_results.R $^ $@

$(TMP_DIR)/errors_by_approach.html: $(TMP_DIR)/stitched.csv
	R -e "library(rmarkdown); rmarkdown::render('errors_by_approach.Rmd', output_file='$@', params=list(p_vals='$^'))"

batch-%: $(PAR_FILE)
	# parallel will append SGE task id to pval file name
	qsub -N $@ $(QSUB) $(QSUB_PARALLEL) parallel.sh $(TMP_DIR)/pvals- $(PAR_FILE) $(N_REPS)
