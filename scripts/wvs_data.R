library(tidyverse)
library(srvyr)
library(here)
library(mirt)
library(corrr)
library(readxl)

### Load in WVS data 

load(here("data/WV6_Data_R_v20201117.rdata"))

### Rename WVS DF 

wvs_df <- WV6_Data_R_v20201117 %>% 
  rowid_to_column(var = 'rowid')

### Remove old DF

rm(WV6_Data_R_v20201117)


### Create countrylist

wvs_country_df <- select(wvs_df, cow, C_COW_ALPHA, B_COUNTRY_ALPHA) %>% 
  distinct()


### Read in LIED
### Create variable for year of democratization

lied <- read_xls((here("data/lied_v5.2.xls"))) %>%  
  group_by(countryn) %>% 
  mutate(dem_transition_year = ifelse(`democratic_transition` == 1,  year, 0),
         dem_breakdown_year = ifelse(`democratic breakdown` == 1, year, 0),
         final_dem_transition = ifelse(dem_transition_year > max(dem_breakdown_year), dem_transition_year, NA)) %>% 
  fill(final_dem_transition) %>% 
  mutate(years_democratized = year - final_dem_transition)

lied_dem_date <- filter(lied, year == 2017) %>% 
  select(countryn, cow, lexical_index, final_dem_transition,years_democratized)

#### join democeracy data

wvs_df_dem <- left_join(wvs_df,lied_dem_date)


  





