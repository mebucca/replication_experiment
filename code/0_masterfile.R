# dims figures

fig_h = 3.5
fig_w = fig_h*1.05

width = 0.22

# ======================================================== SET DIRECTORIES =======================================================


# Define path to root directory  

folder <- getwd() 

# ==================================================== DATA CLEANING & FILTERING ===================================================


# Load data and apply filters to data 

source(here("study_2/code","1_data_filters.R"))

# ========================================================== DATA RECODING =========================================================


# recode data 

source(here("study_2/code","2_data_recoding.R"))


# ========================================================== DATA ANALYSIS =========================================================

source(here("study_2/code","3_fairness.R"))

source(here("study_2/code","6_opinions_UK.R"))

