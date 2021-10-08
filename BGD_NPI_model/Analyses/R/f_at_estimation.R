#https://www.cebm.net/study/covid-19-epidemiological-characteristics-of-covid-19-close-contacts-in-ningbo-city/
library(binom)
set.seed(0)

## symptomatic cases
binom.confint(126,2001) #0.053 - 0.074
symptomatic <- glm(cbind(126,2001-126)~1,family="binomial")
symptomatic.sim <- matrix(unlist(simulate(symptomatic,100000)),ncol=2,byrow=T)
symptomatic.props <- symptomatic.sim[,1]/(symptomatic.sim[,1]+symptomatic.sim[,2])
hist(symptomatic.props)

## asymptomatic cases
binom.confint(6,146)
asymptomatic <- glm(cbind(6,146-6)~1,family="binomial")
asymptomatic.sim <- matrix(unlist(simulate(asymptomatic,100000)),ncol=2,byrow=T)
asymptomatic.props <- asymptomatic.sim[,1]/(asymptomatic.sim[,1]+asymptomatic.sim[,2])
hist(asymptomatic.props)

## asymptomatic/symptomatic
fa <- asymptomatic.props/symptomatic.props
hist(fa)
mean(fa)
quantile(fa,c(0.025,0.975))
