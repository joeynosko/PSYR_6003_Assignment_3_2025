#housekeeping stuff
library(ggplot2)
library(flexplot)
library(tidyverse)
library(haven)
library(lme4)
#P6003_A4 <- read_sav("P6003.A4.sav")
View(P6003_A4)

#visualize univariate distributions 
flexplot(swl~1, data= P6003_A4)#not very normally distributed at all. Quite negatively skewed and almost uniformly distributed.  
flexplot(tipm.E~1, data= P6003_A4)#more normally distributed than neuroticism. 
flexplot(tipm.N~1, data= P6003_A4)#roughly normally distributed. Quite positively skewed. 

#running a baseline model to get ICC 
baseline<- lmer(swl~1+(1|id), data = P6003_A4)
summary(baseline)
icc(baseline)


