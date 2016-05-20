# see Makefile for details
OUTFILE="$1$SGE_TASK_ID.csv"
Rscript 1_run_power_sim.R $OUTFILE $2 $3
