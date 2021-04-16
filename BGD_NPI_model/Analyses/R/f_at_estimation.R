library(binom)
set.seed(0)

## confirmed cases
binom.confint(126,2001) #0.053 - 0.074
confirmed <- glm(cbind(126,2001-126)~1,family="binomial")
confirmed.sim <- matrix(unlist(simulate(confirmed,100000)),ncol=2,byrow=T)
confirmed.props <- confirmed.sim[,1]/(confirmed.sim[,1]+confirmed.sim[,2])
hist(confirmed.props)

## asymptomatic cases
binom.confint(6,146)
asymptomatic <- glm(cbind(6,146-6)~1,family="binomial")
asymptomatic.sim <- matrix(unlist(simulate(asymptomatic,100000)),ncol=2,byrow=T)
asymptomatic.props <- asymptomatic.sim[,1]/(asymptomatic.sim[,1]+asymptomatic.sim[,2])
hist(asymptomatic.props)

## asymptomatic/confirmed
fa <- asymptomatic.props/confirmed.props
hist(fa)
mean(fa)
quantile(fa,c(0.025,0.975))
