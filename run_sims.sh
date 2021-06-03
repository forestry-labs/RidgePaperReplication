#!/bin/bash

#SBATCH --job-name=ridge_rr
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=32
#SBATCH --time=4-00:00:00
#SBATCH --mail-user=theo_s@berkeley.edu

ml R

Rscript 3-Performance/replicationCode/3-run_all_cluster.R
