
### Child Rearing Scale 

rescale <- function(x){(x-min(x))/(max(x)-min(x))}


all_child_rearing <- c("v85", "v86", "v87",
                       "v88", "v89", "v90",
                       "v91", "v92", "v93",
                       "v95")

auth_lib <- c("v85", "v95", "v86", "v89", "v90")

### Create DF to examine factor analysis of child rearing behavior. 

evs_df_auth_lib <- select(evs_df, rowid, all_of(auth_lib)) %>%
  mutate_at(auth_lib, ~as.numeric(.))

evs_df_auth_lib_na <-  na.omit(evs_df_auth_lib)

### 

factanal(na.omit(evs_df_auth_lib[2:6]), factors = 1)
correlate(evs_df_auth_lib[2:6])





### 

evs_df_child_rearing_mirt <- left_join(evs_child_rearing_mirt, evs_df_scales) 

evs_child_rearing_summary <- evs_df_child_rearing_mirt%>% 
  group_by(country_name_text) %>% 
  summarize(mean_child_rearing = mean(child_rearing_norm),
            mean_combined_child_rearing = mean(combined_child_rearing, na.rm = TRUE),
            mean_v135 = mean(v135, na.rm = TRUE))



evs_df_child_rearing_mirt %>% 
  select(child_rearing_norm, v135, v138, v140) %>% 
  correlate()

### Regression

evs_reg_df <- evs_df_child_rearing_mirt %>% 
  select(country_name_text, child_rearing_norm, v135, v225, age_r, v261, v51) %>%
  mutate_at(2:7, ~as.numeric(.))

library(broom)

lm1 <- lm(v140 ~ combined_child_rearing + v225 + age_r+ v261, data = evs_df_child_rearing_mirt)
lm1_fe <- lm(v135 ~ child_rearing_norm + v225 + age_r+ v261 + country_name_text - 1, data = evs_df_child_rearing_mirt)

plm1 <- plm(v51 ~ child_rearing_norm + v225 + age_r+ v261, data = evs_reg_df,
            model = 'within', index = c('country_name_text'))

lm1_tidy <- tidy(lm1, conf.int = TRUE)
lm1_tidy_fe <- tidy(lm1_fe, conf.int = TRUE)



broom::glance(lm1)
broom::glance(lm1_fe)


library(jtools)

effect_plot(lm1, pred = combined_child_rearing, interval = TRUE, plot.points = TRUE)






library(AER)
library(stargazer)

lm1 <- lm(commited_dem ~ combined_child_rearing + sex + birthyear + country, data = evs_df_final)



coeftest(lm1, vcov. = vcovHC, type = "HC1")

# plot the data
plot(x = evs_df_final$combined_child_rearing, 
     y = evs_df_final$commited_dem,
     main = "Scatterplot Commited Democrat and Authoritarian Predisposition",
     xlab = "Authoritarian Predisposition",
     ylab = "Commited Democrat",
     pch = 20,
     ylim = c(-0.4, 1.4),
     cex.main = 0.8)

# add horizontal dashed lines and text
abline(h = 1, lty = 2, col = "darkred")
abline(h = 0, lty = 2, col = "darkred")


# add the estimated regression line
abline(lm1, 
       lwd = 1.8, 
       col = "steelblue")


denyprobit <- glm(commited_dem ~ combined_child_rearing, 
                  family = binomial(link = "probit"), 
                  data = evs_df_final)

coeftest(denyprobit, vcov. = vcovHC, type = "HC1")

effect_plot(lm1, pred = combined_child_rearing, interval = TRUE, plot.points = TRUE)


## fit ordered logit model and store results 'm'
m <- polr(commited_dem ~ combined_child_rearing, data = evs_df_final, Hess=TRUE)

## view a summary of the model
summary(m)

## store table
(ctable <- coef(summary(m)))


## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

## combined table
(ctable <- cbind(ctable, "p value" = p))

(ci <- confint(m)) # default method gives profiled CIs

confint.default(m) # CIs assuming normality

## odds ratios
exp(coef(m))

exp(cbind(OR = coef(m), ci))

### Binary odds ratio

mylogit <- glm(commit_dem_binary ~ combined_child_rearing + country, data = evs_df_final, family = "binomial")
summary(mylogit)

tidy(mylogit)


effect_plot(mylogit, pred = combined_child_rearing, interval = TRUE, partial.residuals = TRUE)

effect_plot(mylogit, pred = combined_child_rearing, interval = TRUE, plot.points = TRUE,
            jitter = .2)


mylogit2 <- glm(respect_auth ~ combined_child_rearing + country, data = evs_df_final, family = "binomial")
summary(mylogit2)

tidy(mylogit)


#### scatter plot


ggplot(evs_df_final, aes(x=auth_child_rearing, y=non_dem_system, group = auth_child_rearing)) +
  geom_boxplot()

ggplot(evs_df_final, aes(x=combined_child_rearing, y=respect_auth, group = combined_child_rearing)) +
  geom_boxplot()


evs_df_final %>% 
  group_by(combined_child_rearing) %>% 
  summarize(mean_non_dem_system = mean(non_dem_system, na.rm = TRUE),
            mean_respect_auth = mean(respect_auth, na.rm = TRUE))



####https://druedin.com/2016/01/16/predicted-probabilities-in-r/

m <- glm(commit_dem_binary ~ combined_child_rearing + sex + age  + country_name_text, data = evs_df_final, family = "binomial")


newdata <- with(evs_df_final, data.frame(combined_child_rearing = c(-3, 2)))

predict(m, newdata, type="response")


newdata2 <- with(evs_df_final, data.frame(combined_child_rearing = -3:2))
newdata2$pree

preds <- predict(m, newdata2, type="response", se.fit=TRUE)
newdata2$predf <- preds$fit # predicted
newdata2$lower <- preds$fit - (1.96*preds$se.fit) # lower bounds
newdata2$upper <- preds$fit + (1.96*preds$se.fit) # upper bounds

ggplot(newdata2, aes(x = combined_child_rearing  , y = predf, ymin = lower, ymax = upper)) + 
  geom_line()+
  geom_ribbon(alpha =  .25, fill = "#111344" , colour=NA)+
  ylab("Predicted Probability of being a Commited Democrat")+
  xlab("Authoritarian Childrearing Values") +
  theme_minimal()

###


evs_df_final_denmark <- filter(evs_df_final, country_name_text == "Denmark")


m <- glm(commit_dem_binary ~ combined_child_rearing + sex + age, data = evs_df_final_denmark, family = "binomial")


newdata <- with(evs_df_final, data.frame(combined_child_rearing = c(-3, 2)))

predict(m, newdata, type="response")


newdata2 <- with(evs_df_final, data.frame(combined_child_rearing = -3:2))
newdata2$pree

preds <- predict(m, newdata2, type="response", se.fit=TRUE)
newdata2$predf <- preds$fit # predicted
newdata2$lower <- preds$fit - (1.96*preds$se.fit) # lower bounds
newdata2$upper <- preds$fit + (1.96*preds$se.fit) # upper bounds

ggplot(newdata2, aes(x = combined_child_rearing  , y = predf, ymin = lower, ymax = upper)) + 
  geom_line()+
  geom_ribbon(alpha =  .25, fill = "#111344" , colour=NA)+
  ylab("Predicted Probability of being a Commited Democrat")+
  xlab("Authoritarian Childrearing Values") +
  theme_minimal()



#### m1: Create plot for pooled 

m1_predict_plot <- ggplot(m1_predict, aes(x = combined_child_rearing  , y = predf, ymin = lower, ymax = upper)) + 
  geom_line()+
  geom_ribbon(alpha =  .25, fill = "#111344" , colour=NA)+
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

ggsave("output/graphs/m1_predict.png", plot = m1_predict_plot, width = 10.685, height = 8, units = "cm", scale = 1, dpi = 300)




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
