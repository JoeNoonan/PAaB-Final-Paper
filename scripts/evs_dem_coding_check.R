#### Check democratic coding

evs_df_final %>% 
  count(system_democracy, system_democracy_binary)

evs_df_final %>% 
  count(system_leader, system_leader_binary)

evs_df_final %>% 
  count(system_experts, system_experts_binary)

evs_df_final %>% 
  count(system_army, system_army_binary)

evs_df_final %>% 
  count(system_democracy_binary, non_dem_system)

evs_df_final %>% 
  count(system_democracy_binary, non_dem_system, commited_dem)


evs_df_final %>% 
  filter(!is.na(commit_dem_binary), !is.na(non_dem_system), commit_dem_binary == 0) %>% 
  count(commit_dem_binary, non_dem_system,system_army_binary,
        system_leader_binary,
        system_experts_binary,  wt = gweight)

### Check missingness by country for all
#age_grp, educ, rural_urban,income, recent_dem

evs_df_final %>% 
  group_by(country_name_text) %>% 
  summarize(age_grp_na = sum(is.na(age_grp)),
            educ_na = sum(is.na(age_grp)),
            )

