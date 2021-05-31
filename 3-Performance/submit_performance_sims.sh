#!/bin/bash


#SBATCH --job-name=ridge_performance_sims
#SBATCH --nodes=1
#SBATCH --tasks-per-node=1
#SBATCH --cpus-per-task=32

ml R
Rscript replicationCode/3-run_all_cluster.R performance_sims_output.out

