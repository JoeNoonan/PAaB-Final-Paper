library(tidyverse)
library(srvyr)
library(here)
library(mirt)
library(corrr)

### WVS Democracy

wvs_df_con_dem <- filter(wvs_df_dem, years_democratized >= 20)


###  MIRT
all_child_rearing <- c("V12", "V13", "V14",
                       "V15", "V16", "V17",
                       "V18", "V19", "V20",
                       "V21", "V22")

auth_lib <- c("V13", "V14", "V21", "V12", "V15","V16")

### Recode so that 1 = Included and 0 = Not included

wvs_df_auth_lib <- select(wvs_df_con_dem, rowid, all_of(auth_lib)) %>%
  mutate_all(~ifelse(. == 2, 0, 1))

wvs_df_all_child_rearing <- select(wvs_df_con_dem, rowid, all_of(all_child_rearing)) %>%
  mutate_at(all_child_rearing, ~ifelse(. == 2, 0, 1))

wvs_df_all_child_rearing_na_omit <- na.omit(wvs_df_all_child_rearing)


#### Factor analysis and correlations 

factanal(na.omit(wvs_df_all_child_rearing), factors = 3)
correlate(wvs_df_all_child_rearing[2:12])

#### Democracy 

all_notions_democracy <- c("V133", "V136", "V139", "V132", "V135", "V138")

wvs_df_all_notions_democracy <-  select(wvs_df_con_dem, rowid, all_of(all_notions_democracy)) 

wvs_df_all_notions_democracy_na_omit <-  na.omit(wvs_df_all_notions_democracy)



factanal(wvs_df_all_notions_democracy_na_omit[2:7], factors = 2)
correlate(wvs_df_all_notions_democracy_na_omit[2:7])


#### mirt

range01 <- function(x){(x-min(x))/(max(x)-min(x))}


#### Child Rearing


results.grm <- mirt(data=wvs_df_all_child_rearing_na_omit[2:12], model=1, itemtype="graded", SE=TRUE, verbose=FALSE)
coef.grm <- coef(results.grm, IRTpars=TRUE, simplify=TRUE)
items.grm <- as.data.frame(coef.grm$items)
print(items.grm)

grm_all_child_rearing <- fscores(results.grm, method = 'MAP', full.scores = TRUE, full.scores.SE = TRUE)

wvs_child_rearing_mirt <- cbind(wvs_df_all_child_rearing_na_omit, grm_all_child_rearing)%>% 
  mutate(child_rearing_norm = range01(F1))

#### Democracy

results.grm <- mirt(data=wvs_df_all_notions_democracy_na_omit[2:7], model=1, itemtype="graded", SE=TRUE, verbose=FALSE)
coef.grm <- coef(results.grm, IRTpars=TRUE, simplify=TRUE)
items.grm <- as.data.frame(coef.grm$items)
print(items.grm)

grm_democracy <- fscores(results.grm, method = 'MAP', full.scores = TRUE, full.scores.SE = TRUE)


wvs_democracy_mirt <- cbind(wvs_df_all_notions_democracy_na_omit, grm_democracy)%>% 
  mutate(democracy_norm = range01(F1),
         lnds = V133 + V136 + V139,
         ands = V132  + V135 + V138,
         combined = lnds - ands)  

#### 

dem_scores_wvs <- left_join(wvs_democracy_mirt, wvs_df_con_dem) 

dem_scores_wvs_summary <- dem_scores_wvs%>% 
  group_by(countryn) %>% 
  summarize(mean_dem_nrom = mean(democracy_norm),
            mean_lnds = mean(lnds, na.rm = TRUE),
            mean_ands = mean(ands, na.rm = TRUE),
            mean_combined = mean(combined),
            median_combined = median(combined))

library(broom)

lm1 <- lm(combined ~ lnds + countryn - 1, data = dem_scores_wvs)

lm1_tidy <- tidy(lm1, conf.int = TRUE)

broom::glance(lm1)

library(jtools)

effect_plot(lm1, pred = lnds, interval = TRUE, plot.points = TRUE)



