#housekeeping stuff
library(ggplot2)
library(flexplot)
library(tidyverse)
library(haven)
library(lme4)
library(performance)
library(lmerTest)
#P6003_A4 <- read_sav("P6003.A4.sav")#load in data
View(P6003_A4)

#visualize univariate distributions 
flexplot(swl~1, data= P6003_A4)#not very normally distributed at all. Quite negatively skewed and almost uniformly distributed.  
flexplot(tipm.E~1, data= P6003_A4)#more normally distributed than neuroticism. 
flexplot(tipm.N~1, data= P6003_A4)#roughly normally distributed. Quite positively skewed. 

#running a baseline model to get ICC 
baseline<- lmer(swl~1+(1|id), data = P6003_A4)
summary(baseline)
icc(baseline)

################## Hypothesis 1 & 2 ##################

#model with only extraversion as a fixed effect 
ex_fixed <- lmer(swl~tipm.E+(1|id), data = P6003_A4)
#model with only extraversion as a fixed and random effect 
ex_random <- lmer(swl~tipm.E+(tipm.E|id), data = P6003_A4)
#comparing the two models to see which is a better fit of the data 
model.comparison(ex_fixed, ex_random)#ex_random is a better fit of the data 

#adding neuroticism to the model as a fixed effect only 
ex_random_ne_fixed <- lmer(swl~tipm.E+tipm.N+(tipm.E|id), data = P6003_A4)
#adding neuroticism to the model as a fixed and random effect 
ex_ne_random <- lmer(swl~tipm.E+tipm.N+(tipm.E+tipm.N|id), data = P6003_A4)
#comparing the two models to see which is a better fit of the data
model.comparison(ex_random_ne_fixed, ex_ne_random)#ex_ne_random is a better fit of the data 

#summary/estimates of final model 
summary(ex_ne_random)
flexplot::estimates(ex_ne_random)#does not work on my machine for some reason
r2(ex_ne_random)
icc(ex_ne_random)
coef(ex_ne_random)
?estimates
#visualization of model 
visualize(ex_ne_random, plot = "model", sample = 200)

#plot model diagnostics
visualize(ex_ne_random, plot = "residuals")

################## Hypothesis 3 ##################


################## Building tables ##################
#install.packages("sjPlot")
library(sjPlot)
tab_model(ex_ne_random, show.se = TRUE, show.r2 = TRUE, show.icc = TRUE, file = "A3_reg_table")#creating regression table for final model with R2, SE, and ICC
A3_corr <- P6003_A4[c("swl", "tipm.E", "tipm.N")]#creating new dataset with only variables of interest to create a less cluttered table
view(A3_corr)
apa.cor.table(A3_corr, filename = "A3_corr_table")#creating a correlation table 
?tab_model
