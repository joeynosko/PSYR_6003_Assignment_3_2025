#housekeeping stuff
library(ggplot2)
library(flexplot)
library(tidyverse)
library(haven)
library(lme4)
library(performance)
library(apaTables)
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
#adding neuroticims to the model as a fixed and random effect 
ex_ne_random <- lmer(swl~tipm.E+tipm.N+(tipm.E+tipm.N|id), data = P6003_A4)
#comparing the two models to see which is a better fit of the data
model.comparison(ex_random_ne_fixed, ex_ne_random)#ex_ne_random is a better fit of the data 

#summary/estimates of final model 
summary(ex_ne_random)
estimates(ex_ne_random)
r2(ex_ne_random)

#visualization of model 
visualize(ex_ne_random, plot = "model")

#plot model diagnostics
visualize(ex_ne_random, plot = "residuals")

#Building tables for report
#install.packages("sjPlot")
library(sjPlot)
tab_model(ex_ne_random)

