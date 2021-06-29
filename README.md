# Ridge Paper Replication

This repository contains the necessary code to replicate the results from the most recent version of the paper: "Linear Aggregation in Tree-based Estimators." 
This draft can be found at: https://arxiv.org/abs/1906.06463.

In order to run the replication code, the *Rforestry* package is required.
The *Rforestry* package can be installed using:
```
install.packages("Rforestry")
```

# Replicating Simulations

In order to replicate the simulations in Section 3, we provide the scripts in the `code/` folder.
For the paper, we tuned the hyperparameters for each estimator using the *caret* package, and 
averaged the results over four runs.
The hyperparameters found by *caret* can be found in the `tuningParam` folder.
This takes a long time even when running on the cluster, so we have currently configured
the code to use the hyperparameters selected by *caret*, and only run once. 
This should make the results run much quicker on either a personal computer, or the cluster.

In order to run all the simulations for Section 3 on a SLURM cluster, one should run:
```
sbatch jobs/submit_performance_sims.sh
```
In order to run all simulations locally, one should run:
```
Rscript code/3-run_all_sims.R
```
This file also contains instructions for how to run the simulations without using 
the already tuned hyperparameters.

The results will then be saved in `results`, and in order to create Figure 2 and Table 2, 
one should run:
```
Rscript code/3-plot_varyingN.R 
Rscript code/3-plot_table_out.R
```
Then the plots can be found in `figures`.

# Appendix Results

