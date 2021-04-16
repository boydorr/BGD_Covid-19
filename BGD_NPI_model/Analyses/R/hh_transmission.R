library(binom)

## attack rates
binom.confint(64+93,392+(449+93)) 
attack_rate <- glm(cbind(64+93,392-64+449)~1,family="binomial")
exp(attack_rate$coefficients)/(1+exp(attack_rate$coefficients))
exp(confint(attack_rate))/(1+exp(confint(attack_rate)))


