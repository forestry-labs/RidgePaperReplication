
# Ridge Paper Replication

This repository contains the necessary code to replicate the results
from the most recent version of the paper: “Linear Aggregation in
Tree-based Estimators.” This draft can be found at:
<https://arxiv.org/abs/1906.06463>.

In order to run the replication code, the **Rforestry** package is
required. The **Rforestry** package can be installed from CRAN using:

    install.packages("Rforestry")

Development versions of the package can be found at:
<https://github.com/forestry-labs/Rforestry>.

# Section 1

The script to recreate Figure 1 can be found in
`code/1-createIntoFigure.R`. The plot can be created by running:

    Rscript code/1-createIntoFigure.R

The figure can then be found in the `figures/` folder.

# Section 3

In order to replicate the performance simulations in Section 3, we
provide the scripts in the `code/` folder. For the paper, we tuned the
hyperparameters for each estimator using the **caret** package, and
averaged the results over four runs. The hyperparameters found by
**caret** can be found in the `tuningParam` folder. This takes a long
time even when running on the cluster, so we have currently configured
the code to use the hyperparameters selected by **caret**, and only run
once. This should make the results run much quicker on either a personal
computer, or the cluster.

In order to run all the simulations for Section 3 on a SLURM cluster,
one should run:

    sbatch jobs/submit_performance_sims.sh

In order to run all simulations locally, one should run:

    Rscript code/3-run_all_sims.R

In order to run the simulation on a cluster, using the hyperparameters
selected by **caret**, one should run the script
`jobs/submit_performance_sims.sh`.

In order to retune the hyperparameters, one can switch the final block
in `code/3-run_all_sims.R` to use
`batch_func(i = i, force = TRUE, run_saved = FALSE)`. This takes a very
long time to run for all estimators and data sets and we don’t recommend
running this locally.

The results will then be saved in `results`, and in order to create
Figure 2 and Table 2, one should run:

    Rscript code/3-plot_varyingN.R 
    Rscript code/3-plot_additional_varyingN.R
    Rscript code/3-plot_table_out.R

The plots will then be found in the `figures/` folder.

# Section 3.4

In order to run the high dimensional simulations from Section 3.4, one
should run the script `jobs/high_dim.sh`.

The random seeds used for different Monte Carlo runs can be found in
`seeds.txt`. If one wants to run more replications, this should be
edited to include the additional seeds.

The plots can then be created using:

    Rscript code/1-plot_high_dimensional.R

The plot will then be found in the `figures/` folder.

# Selected Hyperparameters

For the performance simulations in Section 3, we tune the
hyperparameters for each estimator over a random grid of size 100. The
final hyperparameters selected by **caret** for each estimator can be
found in the `tuningParam` folder. Below is a table of the
hyperparameters selected for LRF in each data set:

| Dataset              | mtry | nodesizeSpl | overfitPenalty | Log(minSplitGain) | sample.fraction |
|:---------------------|-----:|------------:|---------------:|------------------:|----------------:|
| artificial LM 128    |    2 |          17 |           9.47 |             -6.41 |            0.50 |
| artificial LM 256    |    3 |          50 |           5.57 |             -8.71 |            0.52 |
| artificial LM 512    |    2 |          16 |           0.19 |             -2.78 |            0.51 |
| artificial LM 1024   |    4 |           3 |           0.18 |             -2.82 |            0.63 |
| artificial LM 2048   |    9 |          17 |           0.23 |             -3.86 |            0.91 |
| Step 128             |    8 |           9 |           9.29 |             -8.39 |            0.92 |
| Step 256             |    9 |          30 |           0.30 |             -7.36 |            0.77 |
| Step 512             |    8 |          47 |           0.28 |            -12.75 |            0.89 |
| Step 1024            |    5 |          27 |           0.31 |            -18.42 |            0.73 |
| StepLinear 128       |   10 |           5 |           0.31 |             -2.83 |            0.89 |
| StepLinear 256       |   10 |          10 |           8.74 |             -3.00 |            0.92 |
| StepLinear 512       |   10 |          11 |           8.74 |             -3.00 |            0.92 |
| StepLinear 1024      |   10 |          12 |           8.74 |             -3.00 |            0.92 |
| Friedman 1           |    9 |          16 |           0.23 |             -3.86 |            0.91 |
| Friedman 2           |    3 |         195 |           0.43 |             -5.07 |            0.89 |
| Friedman 3           |    4 |          11 |           6.65 |             -3.16 |            0.65 |
| Boston Housing fold1 |   10 |           7 |           0.28 |             -7.86 |            0.95 |
| Boston Housing fold2 |    4 |          13 |           3.06 |             -4.81 |            0.99 |
| Boston Housing fold3 |    3 |          12 |           0.19 |             -4.94 |            0.94 |
| Boston Housing fold4 |    5 |          13 |           0.77 |             -9.12 |            0.91 |
| Boston Housing fold5 |    2 |          11 |           0.25 |            -13.71 |            0.99 |
| Ozone fold1          |    2 |          19 |           9.47 |             -6.41 |            0.50 |
| Ozone fold2          |    3 |          12 |           3.06 |             -4.81 |            0.99 |
| Ozone fold3          |    1 |          20 |           7.40 |            -10.41 |            0.90 |
| Ozone fold4          |    3 |          19 |           9.36 |             -4.76 |            0.92 |
| Ozone fold5          |    2 |           3 |           8.51 |             -7.12 |            0.88 |
| Servo fold1          |   12 |           5 |           0.31 |             -2.83 |            0.89 |
| Servo fold2          |    9 |          16 |           0.11 |             -6.78 |            0.97 |
| Servo fold3          |   11 |           2 |           0.87 |             -8.84 |            0.97 |
| Servo fold4          |   11 |          34 |           0.12 |             -3.22 |            0.87 |
| Servo fold5          |   11 |          33 |           0.12 |             -3.22 |            0.87 |
| Abalone              |    1 |         150 |           0.13 |             -6.25 |            0.92 |
| autos                |    5 |          18 |           0.80 |             -8.44 |            0.92 |
| bike                 |    8 |          23 |           0.11 |             -6.78 |            0.97 |

Hyperparameters selected for LRF by **caret** in Section 3.

# Section 4

The code to generate the figures from Section 4 can be found in
`code/4-gotv_interpret_allTM.R`. This relies on the data in
`data/GerberGreenLarimer_APSR.csv` and the plotting function defined in
`code/4-gotv_S_plot_fkt.R`.

# Appendix A

The code to generate the comparison figure from Appendix A can be found
in `code/X_3-generateComparativeExample.R`. This can be created by
running:

    Rscript code/X_3-generateComparativeExample.R

The figure can then be found in the `figures/` folder.

# Appendix I

The code to generate the figures in Appendix I can be found in
`code/X_9-variable_importances.R`. To run the experiment and save the
results, one should run:

    Rscript code/X_9-variable_importances.R

The figures can be created by running:

    Rscript code/X_9-plot_variable_importances.R

The two figures from this simulation will be saved in the `figures/`
folder.

# Appendix J

The code to run the runtime experiment in Appendix J can be found in
`code/X_10-runtime.R`. In order to run the naive algorithm, one must
have install the R package at: <https://github.com/theo-s/Rforestry_R>.
This can be installed using:

    devtools::install_github("theo-s/Rforestry_R")

Figure 8 can be generated using:

    Rscript code/X_10-plot_runtime.R

The figure can then be found in the `figures/` folder.
