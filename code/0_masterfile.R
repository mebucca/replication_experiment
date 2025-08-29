
# ========================================================== PREAMBLE ============================================================

# Clear Screen 

cat("\014")
rm(list = ls())

# Load Packages 
library("pacman")
p_load(tidyverse,
       tidymodels,
       tidylog,
       readstata13,
       foreign,
       stargazer,
       glmnet,
       broom,
       modelr,
       ggsci,
       gridExtra,
       ggthemes,
       gnm,
       glmc,
       logmult,
       devtools,
       reshape2,
       knitr,
       kableExtra,
       xtable,
       nnet,
       scales,
       pals,
       Matrix,
       survey,
       cowplot,
       pwr,
       here,
       ineq,
       magick,
       stringr,
       modelsummary)

options(xtable.floating = FALSE)
options(xtable.timestamp = "")



# Set preferences ggplot theme


my_custom_theme <- function() {
  theme_bw() + 
    theme(
      plot.background = element_blank(),
      strip.background = element_blank(),
      strip.placement = "outside",
      panel.border = element_rect(size = 1, colour = "black"),
      panel.spacing = unit(0.3, "lines"),
      axis.line = element_line(size = 0.1, colour = "black"),
      axis.ticks.y = element_line(size = 0.5, colour = "black"),
      axis.ticks.length = unit(-0.15, "cm"),
      axis.text.x = element_text(size = 6.5, color = "black", margin = margin(t = 3)),
      axis.text.y = element_text(size = 6.5, color = "black", margin = margin(r = 3)),
      text = element_text(size = 12),
      legend.position = "bottom"
    )
}


theme_set(my_custom_theme())


# Set palettes
mypal_disc <- c("#34AF09", "#814BCF")

# dims figures

fig_h = 3.5
fig_w = fig_h*1.6

width = 0.22

# ======================================================== SET DIRECTORIES =======================================================


# Define path to root directory  

folder <- getwd() 

# ==================================================== DATA CLEANING & FILTERING ===================================================


# Load data and apply filters to data 

source(here("code","1_data_filters.R"))

# ========================================================== DATA RECODING =========================================================


# recode data 

source(here("code","2_data_recoding.R"))


# ========================================================== DATA ANALYSIS =========================================================


# recode data 

source(here("code","3_fairness.R"))

source(here("code","4_most.R"))

source(here("code","5_behavior.R"))

source(here("code","6_opinions_US.R"))

source(here("code","7_assumptions.R"))

