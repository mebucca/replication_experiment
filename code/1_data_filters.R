
# Load data

data_experiment <- NULL

for (i in c(12,13,14,20,21,22)) {
  
  file     <- paste0("info_",i,"-3-2024.csv")
  path     <- here("data",file)
  data_day <- read_delim(path, delim=";")
  data_experiment <- rbind(data_experiment,data_day)
  rm(data_day)
  }


data_experiment_complete <- data_experiment %>% 
  as_tibble() %>%
  drop_na(ConnectId,playerId,groupId,exchange,inequality,feelings) %>% # make sure that has complete information
  filter(attention_check==5)  # keep only individuals who passed attention check
