#housekeeping stuff
library(ggplot2)
library(flexplot)
library(tidyverse)
library(haven)
library(performance)
library(lmerTest)
library(lme4)
#P6003_A4 <- read_sav("P6003.A4.sav")#load in data
View(P6003_A4)

#visualize univariate distributions 
flexplot(swl~1, data= P6003_A4)#not very normally distributed at all. Quite negatively skewed and almost uniformly distributed.  
flexplot(tipm.E~1, data= P6003_A4)#more normally distributed than neuroticism. 
flexplot(tipm.N~1, data= P6003_A4)#roughly normally distributed. Quite positively skewed. 

#running a baseline model to get ICC 
baseline<- lme4::lmer(swl~1+(1|id), data = P6003_A4)#using lme4::lmer because estimates won't run properly without it.  
summary(baseline)
icc(baseline)
visualize(baseline, plot = "model")

################## Hypothesis Testing ##################

#model with only extraversion as a fixed effect 
ex_fixed <- lme4::lmer(swl~tipm.E+(1|id), data = P6003_A4)
#model with only extraversion as a fixed and random effect 
ex_random <- lme4::lmer(swl~tipm.E+(tipm.E|id), data = P6003_A4)
#comparing the two models to see which is a better fit of the data 
model.comparison(ex_fixed, ex_random)#adding extraversion and a fixed and random effect is a better fit.

#adding neuroticism to the model as a fixed effect only 
ex_random_ne_fixed <- lme4::lmer(swl~tipm.E+tipm.N+(tipm.E|id), data = P6003_A4)
#comparing to model with just extraversion as a fixed and random effect 
model.comparison(ex_random, ex_random_ne_fixed)#adding neuroticism as a fixed effect is a better fit. 

#adding neuroticism to the model as a fixed and random effect 
ex_ne_random <- lme4::lmer(swl~tipm.E+tipm.N+(tipm.E+tipm.N|id), data = P6003_A4)
#comparing the two models to see which is a better fit of the data
model.comparison(ex_random_ne_fixed, ex_ne_random)#adding neuroticism as a random and fixed effect is a better fit. 

#summary/estimates of final model 
summary(ex_ne_random)
flexplot::estimates(ex_ne_random)
r2(ex_ne_random)#getting conditional and marginal R2 for the model
icc(ex_ne_random)#getting model ICC 
coef(ex_ne_random)#visualizing slopes for each participant

#visualization of model with all slopes
visualize(ex_ne_random, plot = "model", sample = 263, replace = FALSE)

#plot model diagnostics
visualize(ex_ne_random, plot = "residuals")

################## Building tables for report ##################
#install.packages("sjPlot")
library(sjPlot)

tab_model(ex_ne_random, show.se = TRUE, show.r2 = TRUE, show.icc = TRUE, file = "A3_reg_table")#creating regression table for final model with R2, SE, and ICC

A3_corr <- P6003_A4[c("swl", "tipm.E", "tipm.N")]#creating new dataset with only variables of interest to create a less cluttered table
view(A3_corr)
apa.cor.table(A3_corr, filename = "A3_corr_table")#creating a correlation table 
