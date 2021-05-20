library(tidyverse)
library(srvyr)
library(here)
library(corrr)
library(haven)
library(tidymetadata)
library(surveytoolbox)

### Load in EVS data 
### Create unique rowid 

evs_df <- read_sav(here("data/ZA7500_v4-0-0.sav"))%>% 
  rowid_to_column(var = 'rowid')

### Create meta data that shows different codes for different variables 
evs_df_meta <- create_metadata(evs_df)

### Merge in country names 
evs_country_name_df <- evs_df %>%
  extract_vallab("country") %>% 
  rename(country = id, 
         country_name_text = country) %>% 
  mutate(country_name_text = ifelse(country_name_text == "Great Britain", "United Kingdom", country_name_text))

evs_df <- left_join(evs_country_name_df, evs_df)



### DEMOS: gender, age, education
### Could not control for wealth, rural-urban due to high levels of missingness 
### Attitudinal: political interest, interpersonal trust, and political knowledge.

new_democracies <-  c("Slovenia", "Bulgaria", "Croatia",
                      "Czechia","Poland", "Slovakia",
                      "Hungary", "Lithuania", "Estonia", "Romania", "Albania")


evs_df_final <- evs_df %>% 
  ### Filter consolidated Democracies plus France 
  filter(country_name_text %in% c("Albania", "Austria", "Germany", "Iceland", 
                        "Netherlands", "Spain", "Switzerland",
                        "Slovenia", "Bulgaria", "Croatia", 
                        "Czechia", "Poland", "Slovakia",
                        "Denmark", "Finland", "France","Italy", "Sweden", 
                        "Hungary", "Lithuania", "Norway",
                        "Estonia", "Romania", "United Kingdom")) %>% 
  mutate_at(c("v85", "v95","v86", "v89", "v90"), ~ifelse(. == 2, 0, 1)) %>% 
  mutate(polImportant = if_else(as.numeric(v5) < 1, NaN, as.numeric(v5)),
         polImportant = (polImportant - 4) / (-3), #Standardized
         
         respect_auth = if_else(as.numeric(v114) < 1, NaN, as.numeric(v114)),
         respect_auth = (respect_auth - 2) * (-1), #Good thing at 1, bad thing at -1
         
         gen_trust = if_else(as.numeric(v31) < 1, NaN, as.numeric(v31)),
         gen_trust_rev = 2 - gen_trust,
         
         polint = if_else(as.numeric(v97) < 1, NaN, as.numeric(v97)),
         polint_rev = 5 - polint,
         
         
         system_democracy = if_else(as.numeric(v148) < 1, NaN, as.numeric(v148)),
         system_democracy_binary = if_else(system_democracy %in% c(1, 2), 1, 
                                           ifelse(is.na(system_democracy), NA, 0)),
         
         system_leader= if_else(as.numeric(v145) < 1, NaN, as.numeric(v145)),
         system_leader_binary = if_else(system_leader %in% c(1, 2), 1, 
                                        ifelse(is.na(system_leader), NA, 0)),
         
         system_experts = if_else(as.numeric(v146) < 1, NaN, as.numeric(v146)),
         system_experts_binary = if_else(system_experts %in% c(1, 2), 1, 
                                         ifelse(is.na(system_experts), NA, 0)),
         
         system_army = if_else(as.numeric(v147) < 1, NaN, as.numeric(v147)),
         system_army_binary = if_else(system_army %in% c(1, 2), 1, 
                                      ifelse(is.na(system_army), NA, 0)),
         
         non_dem_system = system_leader_binary + system_experts_binary + system_army_binary,
         
         commited_dem = ifelse(system_democracy_binary == 1 & non_dem_system == 0, "Commited Democrat", 
                               ifelse(system_democracy_binary == 1 & non_dem_system > 0, "Noncommited Democrat",
                                      ifelse(system_democracy_binary  == 0, "Non-Democrat", NA))),
         
         commited_dem = factor(commited_dem, levels = c("Non-Democrat", "Noncommited Democrat", "Commited Democrat")),
         
         commit_dem_binary = as.numeric(ifelse(commited_dem == "Commited Democrat", 1, 0)),
  
         auth_child_rearing = v85+v95,
         lib_child_rearing = v86+v89+v90,
         combined_child_rearing = auth_child_rearing - lib_child_rearing,
         combined_child_rearing_standard = (combined_child_rearing + 3) / (1),#Standardized
         
         democracy_freeElection = if_else(as.numeric(v135) < 1, NaN, as.numeric(v135)),
         democracy_freeElection = (democracy_freeElection - 1) / (9),#Standardized
         democracy_ArmyTakeOver = if_else(as.numeric(v137) < 1, NaN, as.numeric(v137)),
         democracy_ArmyTakeOver = (democracy_ArmyTakeOver - 1) / (9),#Standardized
         democracy_protectLiberty = if_else(as.numeric(v138) < 1, NaN, as.numeric(v138)),
         democracy_protectLiberty = (democracy_protectLiberty - 1) / (9),#Standardized
         DemImportant = if_else(as.numeric(v142) < 1, NaN, as.numeric(v142)),
         DemImportant = (DemImportant - 1) / (9),#Standardized,
         
         
         
         trustParliament = if_else(as.numeric(v121) < 1, NaN, as.numeric(v121)),
         trustParliament = (trustParliament - 4) / (-3),#Standardized
         trustJustice = if_else(as.numeric(v127) < 1, NaN, as.numeric(v127)),
         trustJustice = (trustJustice - 4) / (-3),#Standardized
         trustCivilService = if_else(as.numeric(v122) < 1, NaN, as.numeric(v122)),
         trustCivilService = (trustCivilService - 4) / (-3),#Standardized,
         
         trustInstSmall = (trustParliament + trustJustice + trustCivilService) / 3, #Additive Index
         
         sex = if_else(as.numeric(v225) < 1, NaN, as.numeric(v225)),
         sex = sex - 1,
         birthyear = if_else(as.numeric(v226) < 1880, NaN, as.numeric(v226)), #Remove extremely unreasonable respondent ages (older than 110?)
         period = 2017,
         period = as.numeric(period),
         age = 2017 - birthyear,
         age_grp = ifelse(age >= 15 & age <20, 1, ifelse(
             age >= 20 & age < 30,  2, ifelse(
               age >= 30 & age < 40, 3, ifelse(
                 age >= 40 & age < 50,  4, ifelse(
                   age >= 50 & age < 60,  5, ifelse(
                     age >= 60 & age < 70,  6, ifelse(
                       age >= 70 & age < 80,  7,  8))))))),
         educ = ifelse(as.numeric(v243_edulvlb_1) < 0, NaN, as.numeric(v243_edulvlb_1)),
         educ = ifelse(educ == 66, 0, educ),
         rural_urban = ifelse(as.numeric(v276_r) < 0, NaN, as.numeric(v276_r)),
         income = ifelse(as.numeric(v261_r) < 0, NaN, as.numeric(v261_r)),
         recent_dem = ifelse(country_name_text %in% new_democracies, 1, 0)) %>% 
  dplyr::select(country, country_name_text, gweight, pweight, polImportant, respect_auth, polint, polint_rev, gen_trust, gen_trust_rev, 
                system_democracy, system_democracy_binary,
                system_leader, system_leader_binary,
                system_experts, system_experts_binary, 
                system_army, system_army_binary,
                non_dem_system, commited_dem, commit_dem_binary,
                auth_child_rearing, lib_child_rearing, combined_child_rearing, combined_child_rearing_standard, 
                democracy_freeElection, democracy_ArmyTakeOver, democracy_protectLiberty, DemImportant,
                trustParliament, trustJustice, trustCivilService, trustInstSmall,
                sex, birthyear,age, age_grp, educ, rural_urban,income, recent_dem)




