library(broom)
library(oddsratio)
library(moe)
library(extrafont)
library(gtsummary)
library(flextable)
extrafont::loadfonts("win")

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
#age_grp, educ, rural_urban,income

evs_df_final %>% 
  group_by(country_name_text) %>% 
  summarize(age_grp_na = sum(is.na(age_grp)),
            educ_na = sum(is.na(age_grp)),
            rural_urban_na = sum(is.na(rural_urban)),
            income_na = sum(is.na(income)),
            polint = sum(is.na(polint)),
            gen_trust = sum(is.na(gen_trust)))

### Examine Missinginess on IV and DV 

total_missing <- evs_df_final %>% 
  filter(is.na(commit_dem_binary) | is.na(combined_child_rearing)) %>%
  count()

total_missing_by_country <- evs_df_final %>% 
  filter(is.na(commit_dem_binary) | is.na(combined_child_rearing)) %>% 
  count(country_name_text)

total_by_country <- evs_df_final %>% count(country_name_text) %>% 
  rename(total = n)

missingness_count <- left_join(total_missing_by_country, total_by_country) %>% 
  mutate(percent = n/total)

### Count total number of cases per country

evs_df_final %>% 
  count(combined_child_rearing_standard, commit_dem_binary, country_name_text)

### Histogram of childrearing scale 

ggplot(evs_df_final, aes(x=combined_child_rearing)) + geom_histogram()


### Grouped Bar Plot
### Combined scale

combined_summary_df_unweighted <- evs_df_final %>% 
  filter(!is.na(commit_dem_binary),  !is.na(combined_child_rearing)) %>% 
  count(combined_child_rearing, commit_dem_binary) %>% 
  group_by(combined_child_rearing) %>% 
  mutate(total = sum(n),
         Percent = n/total, 
         commited_dem_name = ifelse(commit_dem_binary == 1, "Commited Democrat",
                                    ifelse(commit_dem_binary == 0, "Other", NA))) 

### Weighted with confidence intervals

combined_summary_df_weighted <- evs_df_final %>% 
  filter(!is.na(commit_dem_binary),  !is.na(combined_child_rearing)) %>% 
  count(combined_child_rearing, commit_dem_binary, wt = gweight) %>% 
  group_by(combined_child_rearing) %>% 
  mutate(total = sum(n),
         Percent = n/total, 
         commited_dem_name = ifelse(commit_dem_binary == 1, "Commited Democrat",
                                    ifelse(commit_dem_binary == 0, "Other", NA))) %>% 
  ungroup() %>%
  mutate(conf_lower = as.numeric(moe(Percent, total)$conf.lower)/100,
         conf_upper = as.numeric(moe(Percent, total)$conf.upper)/100) 




### Check with only authoritarian child rearing. 

auth_summary_df <- evs_df_final %>% 
  count(auth_child_rearing, commit_dem_binary) %>% 
  filter(!is.na(commit_dem_binary),  !is.na(auth_child_rearing)) %>% 
  group_by(auth_child_rearing) %>% 
  mutate(total = sum(n),
         Percent = n/total, 
         commited_dem_name = ifelse(commit_dem_binary == 1, "Commited Democrat",
                                    ifelse(commit_dem_binary == 0, "Other", NA))) 

### Summary commited dems overall

commit_dems_overall <- evs_df_final %>% 
  filter(!is.na(commit_dem_binary), !is.na(combined_child_rearing)) %>% 
  count(commit_dem_binary, wt = gweight) %>% 
  mutate(total = sum(n),
         Percent = n/total, 
         commited_dem_name = ifelse(commit_dem_binary == 1, "Commited Democrat",
                                    ifelse(commit_dem_binary == 0, "Other", NA))) %>% 
  ungroup() %>%
  mutate(conf_lower = as.numeric(moe(Percent, total)$conf.lower)/100,
         conf_upper = as.numeric(moe(Percent, total)$conf.upper)/100)

### Summary type of non-commited Dems

non_commits_type_df <- evs_df_final %>% 
  filter(!is.na(commit_dem_binary), !is.na(non_dem_system), commit_dem_binary == 0) %>% 
  count(commit_dem_binary, non_dem_system,system_army_binary,
        system_leader_binary,
        system_experts_binary,  wt = gweight) 

system_army_binary_df<- non_commits_type_df %>% 
  group_by(system_army_binary) %>% 
  summarize(system_army_binary_n = sum(n)) %>% 
  ungroup() %>% 
  mutate(total = sum(system_army_binary_n),
         Percent = system_army_binary_n/total)

system_leader_binary_df <- non_commits_type_df %>% 
  group_by(system_leader_binary) %>% 
  summarize(system_leader_binary_n = sum(n))%>% 
  ungroup() %>% 
  mutate(total = sum(system_leader_binary_n),
         Percent = system_leader_binary_n/total)

system_experts_binary_df <- non_commits_type_df %>% 
  group_by(system_experts_binary) %>% 
  summarize(system_experts_binary_n = sum(n))%>% 
  mutate(total = sum(system_experts_binary_n),
         Percent = system_experts_binary_n/total)


### Summary of commited dems by country

commited_dems_country_weighted_df <- evs_df_final %>% 
  filter(!is.na(commit_dem_binary), !is.na(combined_child_rearing)) %>% 
  count(country_name_text, commit_dem_binary, wt = gweight) %>% 
  group_by(country_name_text) %>% 
  mutate(total = sum(n),
         Percent = n/total, 
         commited_dem_name = ifelse(commit_dem_binary == 1, "Commited Democrat",
                                    ifelse(commit_dem_binary == 0, "Other", NA))) %>% 
  ungroup() %>%
  mutate(conf_lower = as.numeric(moe(Percent, total)$conf.lower)/100,
         conf_upper = as.numeric(moe(Percent, total)$conf.upper)/100)%>% 
  mutate(country_name_text  = fct_reorder(country_name_text, Percent)) 



### Plot % of commited democrats by country
color_pallete <- c("#2b5a6f", "#6a68a2", "#d06398", "#ff7d53")

df <- filter(commited_dems_country_weighted_df, commited_dem_name == "Commited Democrat") %>% 
  mutate(country_name_text  = fct_reorder(country_name_text, Percent)) 
  

eu_commited_dems_plot <- ggplot(df, 
       aes(x= country_name_text,
           y= Percent,
           ymin = conf_lower,
           ymax = conf_upper)) +
  geom_bar(stat="identity", fill =  "#2b5a6f",  position=position_dodge(preserve = "single", width = 0.9))+ 
  geom_errorbar(position=position_dodge(preserve = "single", width = 0.9), width = 0.5)+
  scale_y_continuous("% commited democrats", labels = scales::percent) +  
  coord_flip()+
  theme_minimal()+ 
  theme( text = element_text(size=12, family = "Meta"), 
         legend.title= element_blank(),
         legend.position="bottom", 
         legend.key.width = unit(.5,  unit = "cm"),
         legend.spacing.x = unit(.25, unit = "cm"),
         legend.box.margin=margin(-12,-12,-12,-12), 
         
         axis.title.x=element_text(size = rel(.9)),  
         axis.title.y=element_blank(),  
        
          axis.text.x = element_text(angle = 0, hjust = 0), 
         axis.text.y = element_text(size = rel(.9)),
         
         plot.title = element_text(size = rel(1), face = "bold", hjust = 0, margin = margin(0,0,5,0)),
         plot.subtitle = element_text(size = rel(.75), face = "plain", hjust = 0, margin = margin(0,0,5,0)),
         plot.caption = element_text(size = rel(0.5), face = "italic", hjust = 1, vjust = 1, margin = margin(12,0,0,0)),
         
         panel.grid.major = element_line(size = .25),
         panel.grid.minor = element_line(size = .25),
         panel.grid = element_line(colour = "grey70"))


ggsave("output/graphs/eu_commited_dems_plot.png", plot = eu_commited_dems_plot, width = 10.685, height = 8, units = "cm", scale = 1, dpi = 300)


### Plot combined_summary_df

grouped_bar_plot <- ggplot(combined_summary_df_weighted, 
       aes(x= combined_child_rearing,
           y= Percent,
           group = as.character(commited_dem_name),
           fill = as.character(commited_dem_name),
           ymin = conf_lower,
           ymax = conf_upper)) +
  geom_bar(stat="identity", position=position_dodge(preserve = "single", width = 0.9))+ 
  geom_errorbar(position=position_dodge(preserve = "single", width = 0.9), width = 0.5)+
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = seq(-3,2))+ 
  scale_fill_manual(values= color_pallete)+
  xlab("Authoritarian Childrearing Values")+
  theme_minimal()+ 
  theme( text = element_text(size=12, family = "Meta"), 
         legend.title= element_blank(),
         legend.position="bottom", 
         legend.key.width = unit(.5,  unit = "cm"),
         legend.spacing.x = unit(.25, unit = "cm"),
         legend.box.margin=margin(-12,-12,-12,-12), 
         
         axis.title.x=element_text(size = rel(.9)),  
         axis.text.x = element_text(angle = 0, hjust = 0), 
         axis.text.y = element_text(size = rel(.9)),
         
         plot.title = element_text(size = rel(1), face = "bold", hjust = 0, margin = margin(0,0,5,0)),
         plot.subtitle = element_text(size = rel(.75), face = "plain", hjust = 0, margin = margin(0,0,5,0)),
         plot.caption = element_text(size = rel(0.5), face = "italic", hjust = 1, vjust = 1, margin = margin(12,0,0,0)),
         
         panel.grid.major = element_line(size = .25),
         panel.grid.minor = element_line(size = .25),
         panel.grid = element_line(colour = "grey70"))+
  guides(colour = guide_legend(nrow = 1, byrow =  TRUE)) 


ggsave("output/graphs/grouped_bar_plot.png", plot = grouped_bar_plot, width = 10.685, height = 8, units = "cm", scale = 1, dpi = 300)


### Logit Regression

evs_df_final_old <-  filter(evs_df_final, recent_dem == 0)
evs_df_final_new <-  filter(evs_df_final, recent_dem == 1)


### Model 1. Simple bivarient with FE 

m1 <- glm(commit_dem_binary ~ combined_child_rearing + country_name_text, family = binomial, data = evs_df_final) 
m1_old <- glm(commit_dem_binary ~ combined_child_rearing + country_name_text, family = binomial, data = evs_df_final_old)
m1_new <- glm(commit_dem_binary ~ combined_child_rearing + country_name_text, family = binomial, data = evs_df_final_new)

anova(m1, test = "Chi")
anova(m1_fe, test = "Chi")
anova(m1_old, test = "Chi")
anova(m1_new, test = "Chi")


### Get Standard Errors
### https://www.andrewheiss.com/blog/2016/04/25/convert-logistic-regression-standard-errors-to-odds-ratios-with-r/

get.or.se <- function(model) {
  broom::tidy(model) %>%
    mutate(or = exp(estimate),
           var.diag = diag(vcov(model)),
           or.se = sqrt(or^2 * var.diag)) %>%
    select(or.se) %>% unlist %>% unname
}

get.or.se(m1)

tidy_m1 <- broom::tidy(m1)
tidy_m1_old <- broom::tidy(m1_old) 
tidy_m1_new <- broom::tidy(m1_new) 

### Exponiate to get odds ratios. 
tidy_m1_exp <- broom::tidy(m1, exponentiate = TRUE,  conf.int= TRUE) 
tidy_m1_old_exp <- broom::tidy(m1_old, exponentiate = TRUE,  conf.int= TRUE) 
tidy_m1_new_exp <- broom::tidy(m1_new, exponentiate = TRUE,  conf.int= TRUE)

### Calculate odds ratios based on longer intervals
#or_glm(data = evs_df_final, model = m1,incr = list(combined_child_rearing = 1))
#or_glm(data = evs_df_final, model = m1,incr = list(combined_child_rearing = 5))

### Publication ready tables
m1_reg_table <-tbl_regression(m1, exponentiate = TRUE) %>%  add_n() 
m1_old_table <- tbl_regression(m1_old, exponentiate = TRUE) %>%  add_n() 
m1_new_table <- tbl_regression(m1_new, exponentiate = TRUE) %>%  add_n()

model_1 <-
  tbl_merge(
    tbls = list(m1_reg_table, m1_old_table, m1_new_table),
    tab_spanner = c("**Full Sample**", "**Old Democracies**", "**New Democracies**"))

### Save Reg Table 
model_1 %>% 
as_flex_table() %>%
flextable::save_as_docx(path = paste0(getwd(),"/output/model_1.docx"))

### Model 2. Controls 
### Used in the final paper
### gender, age, education, wealth, 
### whether the person resides in an urban or rural area, 
# political interest, interpersonal trust, and political knowledge.


m2 <- glm(commit_dem_binary ~ combined_child_rearing + sex + age_grp + educ + polint_rev + gen_trust_rev +country_name_text, data = evs_df_final, family = "binomial")
m2_old <- glm(commit_dem_binary ~ combined_child_rearing + sex + age_grp + educ + polint_rev + gen_trust_rev + country_name_text, data = evs_df_final_old, family = "binomial")
m2_new <- glm(commit_dem_binary ~ combined_child_rearing + sex + age_grp + educ + polint_rev + gen_trust_rev + country_name_text, data = evs_df_final_new, family = "binomial")

#### Save as tidy dataset
tidy_m2 <- broom::tidy(m2) 
tidy_m2_old <- broom::tidy(m2_old) 
tidy_m2_new <- broom::tidy(m2_new) 

### Exponiate to get odds ratios. 
tidy_m2_exp <- broom::tidy(m2, exponentiate = TRUE,  conf.int= TRUE) 
tidy_m2_old_exp <- broom::tidy(m2_old, exponentiate = TRUE,  conf.int= TRUE) 
tidy_m2_new_exp <- broom::tidy(m2_new, exponentiate = TRUE,  conf.int= TRUE)

### Publication ready tables

m2_reg_table <-tbl_regression(m2, exponentiate = TRUE) %>%  add_n() 
m2_old_table <- tbl_regression(m2_old, exponentiate = TRUE) %>%  add_n() 
m2_new_table <- tbl_regression(m2_new, exponentiate = TRUE) %>%  add_n()

### Final model 2

model_2 <-
  tbl_merge(
    tbls = list(m2_reg_table, m2_old_table, m2_new_table),
    tab_spanner = c("**Full Sample**", "**Old Democracies**", "**New Democracies**"))

### Save Reg Table 
model_2 %>% 
  as_flex_table() %>%
  flextable::save_as_docx(path = paste0(getwd(),"/output/model_2.docx"))


####  Run Model 1 for every country

country_list <- unique(evs_df_final$country_name_text)

#m1_function <- function(x){
  df <- filter(evs_df_final, country_name_text %in% {{x}})
  m1 <- glm(commit_dem_binary ~ combined_child_rearing, family = binomial, data = df)
  tidy_m1 <- broom::tidy(m1, exponentiate = TRUE,  conf.int= TRUE) 
  tidy_m1$country_name_text <- {{x}}
  tidy_m1}

#m1_country_list <- lapply(country_list, m1_function)
#names(m1_country_list) <- country_list

#europe_m1 <- bind_rows(m1_country_list, .id = "country_name_text")

#### Run Model 2 for every country
#### Remove fixed effects 

m2_function <- function(x){
  df <- filter(evs_df_final, country_name_text %in% {{x}})
  m2 <- glm(commit_dem_binary ~ combined_child_rearing + sex + age_grp + educ + polint_rev + gen_trust_rev, data = df, family = "binomial")
  tidy_m2 <- broom::tidy(m2, exponentiate = TRUE,  conf.int= TRUE) 
  tidy_m2$country_name_text <- {{x}}
  tidy_m2}

m2_country_list <- lapply(country_list, m2_function)
names(m2_country_list) <- country_list
europe_m2 <- bind_rows(m2_country_list, .id = "country_name_text")


#### General Model 2 prediction plots
#### https://druedin.com/2016/01/16/predicted-probabilities-in-r/

m2_no_fe_old <- glm(commit_dem_binary ~ combined_child_rearing + sex + age_grp + educ + polint_rev + gen_trust_rev, data = evs_df_final_old, family = "binomial")
m2_no_fe_new <- glm(commit_dem_binary ~ combined_child_rearing + sex + age_grp + educ + polint_rev + gen_trust_rev, data = evs_df_final_new, family = "binomial")



m2_predict_df_old <- with(evs_df_final_old, data.frame(combined_child_rearing = -3:2,
                                            sex=mean(sex, na.rm=TRUE), 
                                            age_grp=mean(age_grp, na.rm=TRUE),
                                            educ = mean(educ, na.rm = TRUE),
                                            polint_rev = mean(polint_rev, na.rm = TRUE),
                                            gen_trust_rev = mean(gen_trust_rev, na.rm = TRUE)))


preds_old <- predict(m2_no_fe_old, m2_predict_df_old, type="response", se.fit=TRUE)
m2_predict_df_old$predf <- preds_old$fit # predicted
m2_predict_df_old$lower <- preds_old$fit - (1.96*preds_old$se.fit) # lower bounds
m2_predict_df_old$upper <- preds_old$fit + (1.96*preds_old$se.fit) # upper 
m2_predict_df_old$age <- "Old Democracies" 

### Create new prediction Set
m2_predict_df_new <- with(evs_df_final_new, data.frame(combined_child_rearing = -3:2,
                                                       sex=mean(sex, na.rm=TRUE), 
                                                       age_grp=mean(age_grp, na.rm=TRUE),
                                                       educ = mean(educ, na.rm = TRUE),
                                                       polint_rev = mean(polint_rev, na.rm = TRUE),
                                                       gen_trust_rev = mean(gen_trust_rev, na.rm = TRUE)))

preds_new<- predict(m2_no_fe_new, m2_predict_df_new, type="response", se.fit=TRUE)
m2_predict_df_new$predf <- preds_new$fit # predicted
m2_predict_df_new$lower <- preds_new$fit - (1.96*preds_new$se.fit) # lower bounds
m2_predict_df_new$upper <- preds_new$fit + (1.96*preds_new$se.fit) # upper 
m2_predict_df_new$age <- "New Democracies"

m2_final_predict_df <- rbind(m2_predict_df_new, m2_predict_df_old)


#### Create prediction plot with new and old sample  
m2_predict_plot <- ggplot(m2_final_predict_df, aes(x = combined_child_rearing  , y = predf,
                                                group=age, 
                                                color=age,
                                                fill = age,
                                                ymin = lower, ymax = upper)) + 
  geom_line()+
  geom_ribbon(alpha =  .25)+
  ylab("Predicted probability of \n being a commited democrat")+
  xlab("Authoritarian Childrearing Values") +
  theme_minimal()+ 
  theme( text = element_text(size=12, family = "Meta"), 
         legend.title= element_blank(),
         legend.position="bottom", 
         legend.key.width = unit(.5,  unit = "cm"),
         legend.spacing.x = unit(.25, unit = "cm"),
         legend.box.margin=margin(-12,-12,-12,-12), 
         
         axis.title.x=element_text(size = rel(.9)),  
         axis.text.x = element_text(angle = 0, hjust = 0), 
         axis.text.y = element_text(size = rel(.9)),
         
         plot.title = element_text(size = rel(1), face = "bold", hjust = 0, margin = margin(0,0,5,0)),
         plot.subtitle = element_text(size = rel(.75), face = "plain", hjust = 0, margin = margin(0,0,5,0)),
         plot.caption = element_text(size = rel(0.5), face = "italic", hjust = 1, vjust = 1, margin = margin(12,0,0,0)),
         
         panel.grid.major = element_line(size = .25),
         panel.grid.minor = element_line(size = .25),
         panel.grid = element_line(colour = "grey70"))+
  guides(colour = guide_legend(nrow = 1, byrow =  TRUE)) 

ggsave("output/graphs/m2_predict_new_old.png", plot = m2_predict_plot, width = 10.685, height = 8, units = "cm", scale = 1, dpi = 300)


### Create Eurpoean Prediction Dataset

#m1_predict_function <- function(x){
  df <- filter(evs_df_final, country_name_text %in% {{x}})
  m1 <- glm(commit_dem_binary ~ combined_child_rearing, family = binomial, data = df)
  
  m1_predict_eu <- with(evs_df_final, data.frame(combined_child_rearing = -3:2))
  
  preds <- predict(m1, m1_predict, type="response", se.fit=TRUE)
  m1_predict_eu$predf <- preds$fit # predicted
  m1_predict_eu$lower <- preds$fit - (1.96*preds$se.fit) # lower bounds
  m1_predict_eu$upper <- preds$fit + (1.96*preds$se.fit) # upper 
  m1_predict_eu$country_name_text <- {{x}}
  m1_predict_eu}

#m1_predict_country_list <- lapply(country_list, m1_predict_function)
#names(m1_predict_country_list) <- country_list

#m1_predict_europe <- bind_rows(m1_predict_country_list, .id = "country_name_text")


### 

#### M1: Create predict plot with facet_wrap for all countries 

#m1_predict_eu_plot <- ggplot(m1_predict_europe, aes(x = combined_child_rearing  , y = predf, ymin = lower, ymax = upper)) + 
  geom_line()+
  geom_ribbon(alpha =  .25, fill = "#111344" , colour=NA)+
  facet_wrap(~country_name_text)+
  ylab("Predicted probability of \n being a commited democrat")+
  xlab("Authoritarian Childrearing Values") +
  theme_minimal()+ 
  theme( text = element_text(size=12, family = "Meta"), 
         legend.title= element_blank(),
         legend.position="bottom", 
         legend.key.width = unit(.5,  unit = "cm"),
         legend.spacing.x = unit(.25, unit = "cm"),
         legend.box.margin=margin(-12,-12,-12,-12), 
         
         axis.title.x=element_text(size = rel(.9)),  
         axis.text.x = element_text(angle = 0, hjust = 0), 
         axis.text.y = element_text(size = rel(.9)),
         
         plot.title = element_text(size = rel(1), face = "bold", hjust = 0, margin = margin(0,0,5,0)),
         plot.subtitle = element_text(size = rel(.75), face = "plain", hjust = 0, margin = margin(0,0,5,0)),
         plot.caption = element_text(size = rel(0.5), face = "italic", hjust = 1, vjust = 1, margin = margin(12,0,0,0)),
         
         panel.grid.major = element_line(size = .25),
         panel.grid.minor = element_line(size = .25),
         panel.grid = element_line(colour = "grey70"))+
  guides(colour = guide_legend(nrow = 1, byrow =  TRUE)) 

ggsave("output/graphs/m1_predict_eu_plot.png", plot = m1_predict_eu_plot, width = 10.685, height = 8, units = "cm", scale = 2, dpi = 300)



#### Europe M1 Odds Chart 
#df <- filter(europe_m1, term == "combined_child_rearing") %>% 
  arrange(desc(estimate)) %>% 
  mutate(country_name_text  = fct_reorder(country_name_text, estimate))

europe_m1_plot <- ggplot(df, aes(x = estimate, y = country_name_text)) + 
  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed", color = "red") + 
  geom_errorbarh(aes(xmax = conf.high, xmin = conf.low), size =.5, height = .5, color = "black") +
  geom_point(size = 2, color = "orange")+
  theme_minimal()+ 
  theme( text = element_text(size=12, family = "Meta"), 
         legend.title= element_blank(),
         legend.position="bottom", 
         legend.key.width = unit(.5,  unit = "cm"),
         legend.spacing.x = unit(.25, unit = "cm"),
         legend.box.margin=margin(-12,-12,-12,-12), 
         
         axis.title.x=element_text(size = rel(.9)),  
         axis.text.x = element_text(angle = 0, hjust = 0), 
         axis.text.y = element_text(size = rel(.9)),
         
         plot.title = element_text(size = rel(1), face = "bold", hjust = 0, margin = margin(0,0,5,0)),
         plot.subtitle = element_text(size = rel(.75), face = "plain", hjust = 0, margin = margin(0,0,5,0)),
         plot.caption = element_text(size = rel(0.5), face = "italic", hjust = 1, vjust = 1, margin = margin(12,0,0,0)),
         
         panel.grid.major = element_line(size = .25),
         panel.grid.minor = element_line(size = .25),
         panel.grid = element_line(colour = "grey70"))+
  guides(colour = guide_legend(nrow = 1, byrow =  TRUE)) 

ggsave("output/graphs/europe_m1_plot.png", plot = europe_m1_plot, width = 10.685, height = 8, units = "cm", scale = 1, dpi = 300)



#### Europe M2 Odds Chart
df <- filter(europe_m2, term == "combined_child_rearing") %>% 
  arrange(desc(estimate)) %>% 
  mutate(country_name_text  = fct_reorder(country_name_text, estimate))

europe_m2_plot <- ggplot(df, aes(x = estimate, y = country_name_text)) + 
  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed", color = "red") + 
  geom_errorbarh(aes(xmax = conf.high, xmin = conf.low), size =.5, height = .5, color = "black") +
  geom_point(size = 2, color = "orange")+
  theme_minimal()+ 
  theme( text = element_text(size=12, family = "Meta"), 
         legend.title= element_blank(),
         legend.position="bottom", 
         legend.key.width = unit(.5,  unit = "cm"),
         legend.spacing.x = unit(.25, unit = "cm"),
         legend.box.margin=margin(-12,-12,-12,-12), 
         
         axis.title.x=element_text(size = rel(.9)),  
         axis.text.x = element_text(angle = 0, hjust = 0), 
         axis.text.y = element_text(size = rel(.9)),
         
         plot.title = element_text(size = rel(1), face = "bold", hjust = 0, margin = margin(0,0,5,0)),
         plot.subtitle = element_text(size = rel(.75), face = "plain", hjust = 0, margin = margin(0,0,5,0)),
         plot.caption = element_text(size = rel(0.5), face = "italic", hjust = 1, vjust = 1, margin = margin(12,0,0,0)),
         
         panel.grid.major = element_line(size = .25),
         panel.grid.minor = element_line(size = .25),
         panel.grid = element_line(colour = "grey70"))+
  guides(colour = guide_legend(nrow = 1, byrow =  TRUE)) 

ggsave("output/graphs/europe_m2_plot.png", plot = europe_m2_plot, width = 10.685, height = 8, units = "cm", scale = 1, dpi = 300)




#### Europe M2 Odds facet 
#### Used in final paper


df <- filter(europe_m2, term == "combined_child_rearing") %>% 
  arrange(desc(estimate)) %>% 
  mutate(country_name_text  = fct_reorder(country_name_text, estimate)) %>% 
  mutate(recent_dem = ifelse(country_name_text %in% new_democracies, "New Democracy", "Old Democracy"))

europe_m2_plot_facet <- ggplot(df, aes(x = estimate, y = country_name_text)) + 
  geom_vline(aes(xintercept = 1), size = .75, linetype = "dashed", color = "red") + 
  geom_errorbarh(aes(xmax = conf.high, xmin = conf.low), size =.5, height = .5, color = "black") +
  geom_point(size = 2, color = "#d06398")+
  xlab("Odds Ratio")+
  facet_wrap(~recent_dem, scales = "free_y")+
  scale_x_continuous(breaks = seq(-.5, 1.5, .25))+ 
  theme_minimal()+ 
  theme(text = element_text(size=12, family = "Meta"), 
         legend.title= element_blank(),
         legend.position="bottom", 
         legend.key.width = unit(.5,  unit = "cm"),
         legend.spacing.x = unit(.25, unit = "cm"),
         legend.box.margin=margin(-12,-12,-12,-12), 
         
         axis.title.y = element_blank(),
         axis.title.x=element_text(size = rel(.9)),  
         axis.text.x = element_text(size = rel(.9), hjust = .75), 
         axis.text.y = element_text(size = rel(.9)),
         
         plot.title = element_text(size = rel(1), face = "bold", hjust = 0, margin = margin(0,0,5,0)),
         plot.subtitle = element_text(size = rel(.75), face = "plain", hjust = 0, margin = margin(0,0,5,0)),
         plot.caption = element_text(size = rel(0.5), face = "italic", hjust = 1, vjust = 1, margin = margin(12,0,0,0)),
         
         panel.grid.major = element_line(size = .25),
         panel.grid.minor = element_line(size = .25),
         panel.grid = element_line(colour = "grey70"))+
  guides(colour = guide_legend(nrow = 1, byrow =  TRUE)) 

ggsave("output/graphs/europe_m2_plot_facet.png", plot = europe_m2_plot_facet, width = 12, height = 8, units = "cm", scale = 1, dpi = 300)

### summary statsitics 

df %>% 
  mutate(not_significant = ifelse(conf.low < 1 & conf.high > 1, 1, 0)) %>% 
  group_by(recent_dem) %>% 
  count(not_significant)

