
# recode variables
data_experiment_complete <- 
  data_experiment_complete %>% 
  mutate(game_winner   = if_else(game_winner==T,"winner","loser")) %>% 
  mutate(fairness      = fair_unfair_bar_value) %>%
  mutate(distribution_winner_amount = distribution_winner_amount/250 ) %>%
  mutate(distribution_loser_amount = distribution_loser_amount/250 ) %>%
  mutate(distribution_keep =  if_else(distribution_loser_amount<distribution_winner_amount,1,0)) %>%
  mutate(fair_ineq  = distribution_winner_amount/(distribution_winner_amount + distribution_loser_amount)) %>%
  mutate(obs_ineq      = (inequality + base)/(inequality + base + base))  %>%
  mutate(just_ineq     = log((obs_ineq + 0.01)/(fair_ineq + 0.01) ) ) %>%
  mutate(most_luck     = if_else(most_important=="luck",1,0)) %>%
  mutate(most_talent   = if_else(most_important=="talent",1,0)) %>%
  mutate(most_rules    = if_else(most_important=="rules",1,0)) %>%
  mutate(least_luck    = if_else(least_important=="luck",1,0)) %>%
  mutate(least_talent  = if_else(least_important=="talent",1,0)) %>%
  mutate(least_rules   = if_else(least_important=="rules",1,0)) %>%
  mutate(ahead_fwealth   = if_else(get_ahead_1>=4,1,0)) %>%
  mutate(ahead_peduc     = if_else(get_ahead_2>=4,1,0)) %>%
  mutate(ahead_ambition  = if_else(get_ahead_3>=4,1,0)) %>%
  mutate(ahead_hardwork  = if_else(get_ahead_4>=4,1,0)) %>%
  mutate(ineq_toohigh    = if_else(income_inequality =="strongly_agree",1,0)) %>%
  mutate(gov_redineq     = if_else(government_opinion =="strongly_agree",1,0)) %>%
  mutate(feeling_pos     = if_else(feelings =="happy",1,0)) %>%
  mutate(feeling_neg     = if_else(feelings =="sad" | feelings =="angry",1,0)) %>%
  mutate(min_payment_UG = min_payment_UG*100) %>%
  mutate(max_payment_UG = max_payment_UG*100) %>%
  mutate(day = day(as.Date(date))) %>%
  mutate(income_midpoint = case_when(
    income == "$0-$9,999" ~ 5000,
    income == "$10,000-$24,999" ~ 17500,
    income == "$25,000-$39,999" ~ 32500,
    income == "$40,000-$54,999" ~ 47500,
    income == "$55,000-$69,999" ~ 62500,
    income == "$70,000-$84,999" ~ 77500,
    income == "$85,000-$99,999" ~ 92500,
    income == "$100,000-$114,999" ~ 107500,
    income == "$115,000-$129,999" ~ 122500,
    income == "$130,000-$144,999" ~ 137500,
    income == "$145,000-$159,999" ~ 152500,
    income == "$160,000-$174,999" ~ 167500,
    income == "$175,000+" ~ 175000, # Assuming 175000 as the midpoint for simplicity; adjust if necessary
    is.na(income) ~ NA_real_, # Handle NA values
    TRUE ~ NA_real_ # Default case if none of the above matches
  )) %>%
  mutate(years_of_schooling = case_when(
    education == " Less  than  High  School " ~ 10,
    education == " High  School " ~ 12,
    education == " Some  College " ~ 14,
    education == " College  Degree " ~ 16,
    education == " Graduate/Professional  Degree " ~ 20,
    is.na(education) ~ NA_real_,
    TRUE ~ NA_real_ # Handles any unexpected cases
))  



# creates unique identifier for each pair of players
data_experiment_complete <- data_experiment_complete %>% 
  group_by(day,groupId,exchange,inequality,base) %>% 
  mutate(pairId = if_else(bot_opponent==F,cur_group_id(),NA_integer_)) %>%
  ungroup()

data_experiment_complete <- data_experiment_complete %>% group_by(pairId) %>%
  mutate(count_pair = n()) %>%
  mutate(pairId = if_else(count_pair==2,pairId,NA_integer_)) %>%
  ungroup()

# computes payment of each player
data_experiment_complete <- data_experiment_complete %>% 
  mutate(payment = if_else(game_winner=="winner", inequality + base, base)) %>%
  ungroup()


# deviations from min_max strategy
data_experiment_complete <-   data_experiment_complete %>% 
   mutate(diff_r_1 = average_difference_round_1...15) %>% 
   mutate(diff_r_2 = average_difference_round_2...19) %>% 
   mutate(diff_r_3 = average_difference_round_3...23) %>% 
   mutate(diff_r_4 = average_difference_round_1...29) %>% 
   mutate(diff_r_5 = average_difference_round_2...32) %>% 
   mutate(diff_r_6 = average_difference_round_3...35) %>% 
   mutate(diff_r_7 = average_difference_round_4) %>% 
   mutate(diff_r_8 = average_difference_round_5) 
    

# hand in each round
data_experiment_complete <- data_experiment_complete %>% 
  mutate(powerhand_1 = power_player_round_1_game) %>% 
  mutate(powerhand_2 = power_player_round_2_game) %>% 
  mutate(powerhand_3 = power_player_round_3_game) %>% 
  mutate(powerhand_4 = power_player_round_4_game) %>% 
  mutate(powerhand_5 = power_player_round_5_game) 

# relative quality of hand in each round
data_experiment_complete <- data_experiment_complete %>% 
  group_by(pairId) %>%
  mutate(relpowerhand_1 = power_player_round_1_game/sum(power_player_round_1_game)) %>%
  mutate(relpowerhand_2 = power_player_round_2_game/sum(power_player_round_2_game)) %>%
  mutate(relpowerhand_3 = power_player_round_3_game/sum(power_player_round_3_game)) %>%
  mutate(relpowerhand_4 = power_player_round_4_game/sum(power_player_round_4_game)) %>%
  mutate(relpowerhand_5 = power_player_round_5_game/sum(power_player_round_5_game)) %>%
  ungroup()


# computes inequality of opportunity at group level
data_experiment_complete <- data_experiment_complete %>%
  rowwise() %>%
  mutate(relpowerhand_total = mean(relpowerhand_2,relpowerhand_3,relpowerhand_4,relpowerhand_5)) %>%
  ungroup() %>%
  group_by(pairId) %>%
  mutate(ineq_opp = Gini(relpowerhand_total, na.rm = T)) %>%
  ungroup()

# recode variables

data_experiment_complete <- 
  data_experiment_complete %>% 
  mutate(inequality    = factor(inequality))


data_experiment_complete <- 
  data_experiment_complete %>%  mutate(
    zip_code = str_trim(zip_code),  # remove extra spaces
    zip_prefix = str_sub(zip_code, 1, 1),  # extract first digit
    region = case_when(
      zip_prefix %in% c("0", "1")           ~ "Northeast",
      zip_prefix %in% c("2", "3")           ~ "South",
      zip_prefix %in% c("4", "5", "6")      ~ "Midwest",
      zip_prefix %in% c("7", "8", "9")      ~ "West",
      TRUE                                  ~ NA_character_
    )
  )

# Sample sociodemographic composition table

# Clean data
data_experiment_complete_clean <- data_experiment_complete %>%
  select(age, political_ideology, income, gender, race, education, religion, political_party, region) %>%
  mutate(
    age = as.numeric(age),
    political_ideology = as.numeric(political_ideology),
    across(where(is.character), ~ stringr::str_squish(.))
  )

# Specify variable types
continuous_vars <- c("age", "political_ideology")
categorical_vars <- setdiff(names(data_experiment_complete_clean), continuous_vars)

# Summarize continuous variables
cont_summary <- data_experiment_complete_clean %>%
  select(all_of(continuous_vars)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Value") %>%
  group_by(Variable) %>%
  summarise(
    N = sum(!is.na(Value)),
    Mean = mean(Value, na.rm = TRUE),
    SD = sd(Value, na.rm = TRUE),
    Min = min(Value, na.rm = TRUE),
    Max = max(Value, na.rm = TRUE)
  ) %>%
  mutate(across(where(is.numeric), ~round(., 2)))

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
  mutate(
    Mean = NA, SD = NA, Min = NA, Max = NA
  ) %>%
  select(Variable, Value, N, Percent, Mean, SD, Min, Max)

# For continuous, add Value = "" and Percent = NA for consistency
cont_summary <- cont_summary %>%
  mutate(Value = "", Percent = NA) %>%
  select(Variable, Value, N, Percent, Mean, SD, Min, Max)

# Combine
final_table <- bind_rows(cont_summary, cat_summary) %>%
  arrange(Variable, desc(N), Value)

# Output to LaTeX
file_table <- here("tables", "table_sociodemo_descriptives.tex")
modelsummary::datasummary_df(
  final_table,
  title = "Descriptive Statistics: Sociodemographic Variables",
  output = file_table
)
