
# recode variables
data_experiment_complete <- 
  data_experiment_complete %>% 
  mutate(ineq_out = if_else(str_detect(conditions_DO, "high"), "high", "low"),
         ineq_out = factor(ineq_out, levels = c("low", "high"))) %>%
  mutate(ineq_opp = if_else(str_detect(conditions_DO, "unfair"), "more unequal", "more equal")) %>%
  mutate(fairness  = coalesce(unfair_fair_1_1,unfair_fair_2_1) %>% str_extract("\\d+") %>% as.numeric()) %>%
  mutate(distribution_winner_amount = coalesce(fairineq_low_1, fairineq_high_1) %>% str_extract("\\d+") %>% as.numeric() ) %>%
  mutate(distribution_loser_amount = coalesce(fairineq_low_2, fairineq_high_2) %>% str_extract("\\d+") %>% as.numeric() ) %>%
  mutate(base = 15500/1000, inequality = if_else(ineq_out=="high", 201000/1000 - base, 64800/1000 - base)) %>%
  mutate(distribution_keep =  if_else(distribution_loser_amount<distribution_winner_amount,1,0)) %>%
  mutate(fair_ineq  = distribution_winner_amount/(distribution_winner_amount + distribution_loser_amount)) %>%
  mutate(obs_ineq      = (inequality + base)/(inequality + base + base))  %>%
  mutate(just_ineq     = log((obs_ineq + 0.01)/(fair_ineq + 0.01))) %>%   
  mutate(get_ahead_1 = coalesce(important_factors_1_1,important_factors_2_1)) %>%
  mutate(get_ahead_2 = coalesce(important_factors_1_2,important_factors_2_2)) %>%
  mutate(get_ahead_3 = coalesce(important_factors_1_3,important_factors_2_3)) %>%
  mutate(get_ahead_4 = coalesce(important_factors_1_4,important_factors_2_4)) %>%
  mutate(ahead_fwealth   = if_else(get_ahead_1 %in% c("Very Important","Essential"),1,0)) %>%
  mutate(ahead_peduc     = if_else(get_ahead_2 %in% c("Very Important","Essential"),1,0)) %>%
  mutate(ahead_ambition  = if_else(get_ahead_3 %in% c("Very Important","Essential"),1,0)) %>%
  mutate(ahead_hardwork  = if_else(get_ahead_4 %in% c("Very Important","Essential"),1,0)) %>%
  mutate(
    income_midpoint = case_when(
      income_UK == "£10,000 or less"        ~ 5000,
      income_UK == "£10,001–£15,000"        ~ 12500,
      income_UK == "£15,001–£20,000"        ~ 17500,
      income_UK == "£20,001–£25,000"        ~ 22500,
      income_UK == "£25,001–£35,000"        ~ 30000,
      income_UK == "£35,001–£60,000"        ~ 47500,
      income_UK == "£60,001–£90,000"        ~ 75000,
      income_UK == "£90,001–£130,000"       ~ 110000,
      income_UK == "£130,001 or more"       ~ 150000,  # adjustable top-code midpoint
      is.na(income_UK)                      ~ NA_real_,
      TRUE                                  ~ NA_real_
    )
  ) %>%
  mutate(
    years_of_schooling = case_when(
      educ_UK == "Less than Secondary School"                                   ~ 10,
      educ_UK == "Secondary School (Completed)"                                  ~ 12,
      educ_UK == "Post-secondary Vocational or Technical Training"               ~ 14,
      educ_UK == "Post-secondary Academic Qualification (Did not Complete Degree)" ~ 15,
      educ_UK == "Bachelor’s degree or equivalent first degree"                  ~ 16,
      educ_UK == "Graduate or professional degree (e.g., MA, MSc, MBA, JD, MD, PhD)" ~ 18,
      educ_UK == "None of the above"                                              ~ NA_real_,
      is.na(educ_UK)                                                              ~ NA_real_,
      TRUE                                                                        ~ NA_real_
    )
  ) %>%
  mutate(gender = if_else(gender %in% c("Non-binary / third gender", "Prefer not to say"), "Other gender", gender))


# Sample sociodemographic composition table

# Clean data
data_experiment_complete_clean <- data_experiment_complete %>%
  select(age, pol, income_UK, gender, race, educ_UK, citizen) %>%
  mutate(
    age = as.numeric(age),
    across(where(is.character), ~ stringr::str_squish(.))
  )

# Recode labels to match target
data_experiment_complete_clean <- data_experiment_complete_clean %>%
  mutate(
    educ_UK = recode(educ_UK,
      "Bachelor's degree or equivalent first degree"          = "Bachelor's Degree",
      "Graduate or professional degree (e.g., MA, MSc, MBA, JD, MD, PhD)" = "Graduate/Professional Degree",
      "Secondary School (Completed)"                          = "Secondary School (Completed)",
      "Post-secondary Vocational or Technical Training"       = "Post-secondary Vocational/Technical",
      "Post-secondary Academic Qualification (Did not Complete Degree)" = "Post-secondary, No Degree",
      "Less than Secondary School"                            = "Less than Secondary School"
    ),
    income_UK = recode(income_UK,
      "£10,000 or less"      = "\u00a310,000 or less",
      "£10,001–£15,000"      = "\u00a310,001--\u00a315,000",
      "£15,001–£20,000"      = "\u00a315,001--\u00a320,000",
      "£20,001–£25,000"      = "\u00a320,001--\u00a325,000",
      "£25,001–£35,000"      = "\u00a325,001--\u00a335,000",
      "£35,001–£60,000"      = "\u00a335,001--\u00a360,000",
      "£60,001–£90,000"      = "\u00a360,001--\u00a390,000",
      "£90,001–£130,000"     = "\u00a390,001--\u00a3130,000",
      "£130,001 or more"     = "\u00a3130,001 or more"
    ),
    pol = recode(pol,
      "Extremely Liberal"    = "Extremely Liberal",
      "Liberal"              = "Liberal",
      "Slightly Liberal"     = "Slightly Liberal",
      "Moderate"             = "Moderate",
      "Slightly Conservative" = "Slightly Conservative",
      "Conservative"         = "Conservative",
      "Extremely Conservative" = "Extremely Conservative"
    ),
    gender = recode(gender,
      "Male"                      = "Male",
      "Female"                    = "Female",
      "Non-binary / third gender" = "Other",
      "Prefer not to say"         = "Other"
    ),
    race = recode(race,
      "White"                          = "White",
      "Black or African"               = "Black or African",
      "Asian"                          = "Asian",
      "Multiracial / Mixed race"       = "Multiracial/Mixed Race",
      "Hispanic or Latino/a/x"         = "Hispanic or Latino/a",
      "Middle Eastern or North African" = "Middle Eastern/North African",
      "Another group"                  = "Another Group"
    ),
    citizen = recode(citizen,
      "Yes" = "Yes",
      "No"  = "No"
    )
  ) %>%
  rename(
    Age                = age,
    `Political Ideology` = pol,
    Income             = income_UK,
    Gender             = gender,
    Race               = race,
    Education          = educ_UK,
    Citizenship        = citizen
  )

# Specify variable types
continuous_vars <- c("Age")
categorical_vars <- setdiff(names(data_experiment_complete_clean), continuous_vars)

# Summarize continuous variables
cont_summary <- data_experiment_complete_clean %>%
  select(all_of(continuous_vars)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Value") %>%
  group_by(Variable) %>%
  summarise(
    N       = sum(!is.na(Value)),
    Mean    = mean(Value, na.rm = TRUE),
    SD      = sd(Value, na.rm = TRUE),
    Min     = min(Value, na.rm = TRUE),
    Max     = max(Value, na.rm = TRUE)
  ) %>%
  mutate(across(where(is.numeric), ~ round(., 2))) %>%
  mutate(Value = "", Percent = NA_real_) %>%
  select(Variable, Value, N, Percent, Mean, SD, Min, Max)

# Summarize categorical variables
cat_summary <- data_experiment_complete_clean %>%
  select(all_of(categorical_vars)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Value") %>%
  filter(!is.na(Value)) %>%
  group_by(Variable, Value) %>%
  summarise(N = n(), .groups = "drop") %>%
  group_by(Variable) %>%
  mutate(Percent = round(100 * N / sum(N), 1)) %>%
  ungroup() %>%
  mutate(Mean = NA_real_, SD = NA_real_, Min = NA_real_, Max = NA_real_) %>%
  select(Variable, Value, N, Percent, Mean, SD, Min, Max)

# Define display order
var_order <- c("Age", "Citizenship", "Education", "Gender", "Income",
               "Political Ideology", "Race")

# Combine and sort
final_table <- bind_rows(cont_summary, cat_summary) %>%
  mutate(Variable = factor(Variable, levels = var_order)) %>%
  arrange(Variable, desc(N)) %>%
  mutate(Variable = as.character(Variable))

# Sanitize for LaTeX
final_table <- final_table %>%
  mutate(
    Value = str_replace_all(Value, "\u00a3", "\\\\pounds{}"),
    Value = str_replace_all(Value, "\u2013", "--")
  )

# --- Write LaTeX manually ---
file_table <- here("study_2/tables", "table_sociodemo_descriptives.tex")

format_cell <- function(x) ifelse(is.na(x), "", as.character(x))

rows <- purrr::pmap_chr(final_table, function(Variable, Value, N, Percent, Mean, SD, Min, Max) {
  paste(Variable, "&", Value, "&", N, "&",
        format_cell(Percent), "&", format_cell(Mean), "&",
        format_cell(SD), "&", format_cell(Min), "&",
        format_cell(Max), "\\\\")
})

# Suppress repeated variable names
current_var <- ""
rows_clean <- purrr::map_chr(seq_along(rows), function(i) {
  var <- final_table$Variable[i]
  if (var == current_var) {
    rows[[i]] <- sub(paste0("^", var), "", rows[[i]])
    rows[[i]] <- paste0(" ", rows[[i]])
  } else {
    current_var <<- var
  }
  rows[[i]]
})

latex_out <- paste0(
  "\\begin{table}[H]\n",
  "\\scriptsize\n",
  "\\centering\n",
  "\\caption{Descriptive Statistics: Sociodemographic Variables (UK)}\n",
  "\\label{tab:desc_stats_uk}\n",
  "\\begin{tabular}{llllllll}\n",
  "\\hline\n",
  "Variable & Value & N & Percent & Mean & SD & Min & Max \\\\\n",
  "\\hline\n",
  paste(rows_clean, collapse = "\n"), "\n",
  "\\hline\n",
  "\\end{tabular}\n",
  "\\end{table}\n"
)

writeLines(latex_out, file_table)