Replication Package
===================

This repository contains the replication code and materials for the study. 
The project is structured to reproduce all data preparation, analysis, tables, and figures.

------------------------------------------------------------
Repository Structure
------------------------------------------------------------

replicate.R          Main replication script (entry point)
code/                Analysis scripts (master file + modules)
  ├── 0_masterfile.R
  ├── 1_data_filters.R
  ├── 2_data_recoding.R
  ├── 3_fairness.R
  ├── 4_most.R
  ├── 5_behavior.R
  ├── 6_opinions_US.R
  └── 7_assumptions.R
data/                Raw and cleaned data (not included if restricted)
figures/             Output figures
tables/              Output tables

------------------------------------------------------------
Software Requirements
------------------------------------------------------------

- R (>= 4.0.0)

Required R packages (loaded automatically using "pacman"):
tidyverse, tidymodels, tidylog, readstata13, foreign, stargazer,
glmnet, broom, modelr, ggsci, gridExtra, ggthemes, gnm, glmc,
logmult, devtools, reshape2, knitr, kableExtra, xtable, nnet,
scales, pals, Matrix, survey, cowplot, pwr, here, ineq, magick,
stringr, modelsummary

------------------------------------------------------------
How to Run the Replication
------------------------------------------------------------

1. Download this repository to your computer.

2. Open R (or RStudio) in the root directory of the project.

3. Run the main replication script:

   source("replicate.R")

This script will:
- Load all required packages
- Import and filter raw data
- Recode variables
- Run all analyses
- Generate tables and figures in the "tables/" and "figures/" folders

------------------------------------------------------------
Output
------------------------------------------------------------

- Figures are saved in "figures/"
- Tables are saved in "tables/"

