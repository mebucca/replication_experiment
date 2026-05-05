
# Load data

file     <- paste0("data_pilot.csv")
path     <- here("study_2/data",file)
data_experiment <- read_delim(path, delim=",")

# drop Qualtrics metadata rows
data_experiment <- data_experiment %>%
  slice(-(1:2)) 

# dates

data_experiment <- data_experiment %>%
  mutate(
    year = year(EndDate),
    month = month(EndDate),
    day = day(EndDate) 
  )


# keep data from definite experiment only (exclude pilot data)
data_experiment <- data_experiment %>% filter(year==2026, month==2, day==12) 

# drop columns with all NA values 
data_experiment <- data_experiment %>%
  select(where(~ !all(is.na(.))))

data_experiment_complete <- data_experiment %>% 
  drop_na(ResponseId,conditions_DO) %>% # make sure that has complete information
  filter(check1==6)  # keep only individuals who passed attention check


