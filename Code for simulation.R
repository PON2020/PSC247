# simulation of an interaction between neuroticism and attachment type on first fixation duration FFD
# import package
library(tidyverse)
# the simulated data will be stored in a data frame with three columns and 100 rows.
# the first column is attachment type, an alternation between NP1 (0) and NP2 (1), each 50 times.
attachment_type <- rep(c(0,1),50)
# the second column is neuroticism score, simulated by randomly pickup 100 numbers from
# a distribution with the mean of 50 and sd of 15
neuroticism <- rnorm(100, 50, 15)
# sanity check: no value <0 and no value >100
table(neuroticism>100)
table(neuroticism<0)
# simulate FFD using the formula: 
# FFD = beta1 * attachment_type + beta2 * neuroticism + beta3 * attachment_type * neuroticism + error
error <- 200
beta1 <- -20
beta2 <- 0.05
beta3 <- -0.06
first_fixation_duration <- beta1 * attachment_type + beta2 * neuroticism + beta3 * attachment_type * neuroticism + error
# the simulated is stored in a tibble
data <- tibble(first_fixation_duration,attachment_type,neuroticism)
data$attachment_type <- as.factor(data$attachment_type)
# rename the attachment type 
data$attachment_type <- recode(data$attachment_type,
                       "0" = "NP1",
                       "1" = "NP2"
)
head(data)
# build a linear modal
lm(data=data,first_fixation_duration~attachment_type*neuroticism)
# plot the change of FFD with nueroticism by attachment style
ggplot(data=data,mapping = aes(x=neuroticism,y=first_fixation_duration, color=attachment_type))+
  geom_point(alpha=0.01)+
  geom_smooth(method="lm")+
  ylab("first fixation duration (ms)")
