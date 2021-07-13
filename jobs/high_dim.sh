#!/bin/bash

#SBATCH --job-name=ridge_high_dim
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=8
#SBATCH --time=1-00:00:00
#SBATCH --array=1-200

LINE=$(sed -n ${SLURM_ARRAY_TASK_ID}p "params.txt")
seed=$(echo $LINE | cut -d ' ' -f 3)

Rscript code/1-highDimensionalSims.R --seed "$seed"
