# Ridge Paper Replication

This repository contains the necessary code to replicate the results from the most recent version of the paper: "Linear Aggregation in Tree-based Estimators." 
This draft can be found at: https://arxiv.org/abs/1906.06463.

In order to run the replication code, the **Rforestry** package is required.
The **Rforestry** package can be installed using:
```
install.packages("Rforestry")
```

# Introduction Figure

The script to recreate Figure 1 can be found in `code/1-createIntoFigure.R`.

# Performance Simulations

In order to replicate the simulations in Section 3, we provide the scripts in the `code/` folder.
For the paper, we tuned the hyperparameters for each estimator using the **caret** package, and 
averaged the results over four runs.
The hyperparameters found by **caret** can be found in the `tuningParam` folder.
This takes a long time even when running on the cluster, so we have currently configured
the code to use the hyperparameters selected by **caret**, and only run once. 
This should make the results run much quicker on either a personal computer, or the cluster.

In order to run all the simulations for Section 3 on a SLURM cluster, one should run:
```
sbatch jobs/submit_performance_sims.sh
```
In order to run all simulations locally, one should run:
```
Rscript code/3-run_all_sims.R
```
In order to run the simulation on a cluster, using the hyperparameters selected by **caret**,
one should run the script `jobs/submit_performance_sims.sh`.

In order to retune the hyperparameters, one can switch the final block in `code/3-run_all_sims.R` to use `batch_func(i = i, force = TRUE, run_saved = FALSE)`. 
This takes a very long time to run for all estimators and data sets and we don't recommend running this locally.

The results will then be saved in `results`, and in order to create Figure 2 and Table 2, 
one should run:
```
Rscript code/3-plot_varyingN.R 
Rscript code/3-plot_table_out.R
```
Then the plots can be found in `figures`.

# High Dimensional Simulations

In order to run the high dimensional simulations from Section 3.4, one should run the 
script `jobs/high_dim.sh`.

The random seeds used for different Monte Carlo runs can be found in `seeds.txt`.
If one wants to run more replications, this should be edited to include the additional seeds.

The plots can then be created using:

```
Rscript code/1-plot_high_dim.R
```
The plot will then be found in `figures`.


# Interpretability Plots

The code to generate the figures from Section 4 can be found in `code/4-gotv_interpret_allTM.R`.
This relies on the data in `data/GerberGreenLarimer_APSR.csv`.

# Appendix Results

The code to generate the figure from Appendix A can be found in `code/X_3-generateComparativeExample.R`.

