library(readr)
library(dplyr)
library(readxl)
library(tidyr)
install.packages("janitor")
library(janitor)

data <- read.csv("https://data.cityofnewyork.us/api/views/nc67-uf89/rows.csv?accessType=DOWNLOAD", header = TRUE, nrow = 4000000)

police_issued <- data %>% 
  filter(Issuing.Agency == "POLICE DEPARTMENT")


police_issued_g_ng <- data %>% 
  filter(Issuing.Agency == "POLICE DEPARTMENT") %>% 
  filter(Violation.Status == "HEARING HELD-NOT GUILTY" | Violation.Status == "HEARING HELD-GUILTY" | Violation.Status == "HEARING HELD-GUILTY REDUCTION")

status_by_precinct <- police_issued_g_ng %>%
  group_by(Precinct, Violation.Status) %>% 
  tally() %>% 
  spread(Violation.Status, n) %>% 
  clean_names() %>% 
  mutate(Total = hearing_held_guilty + hearing_held_guilty_reduction + hearing_held_not_guilty) %>% 
  mutate(NGPercent = (hearing_held_not_guilty/Total))

write.csv(status_by_precinct, "NotGuiltyPercent.csv")

status_by_violation_type <- police_issued_g_ng %>%
  group_by(Precinct, Violation, Violation.Status) %>% 
  filter(Precinct == '23') %>% 
  tally()

total_violations <- police_issued_g_ng %>% group_by(Precinct, Violation) %>% tally()
