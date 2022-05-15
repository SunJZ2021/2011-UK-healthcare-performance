library(MASS)

load("MAS223.RData")
who_exUK <- who[who$country != "United Kingdom",]
attach(who_exUK)

#cor(tobacco,life)
#cor(expenditure,life)
#cor(alcohol,life)
#cor(obesity,life)

lm_f <- lm(life~tobacco + expenditure + alcohol + obesity)
summary(lm_f)

par(mfrow=c(1,2))
evals <- stdres(lm_f)
hist(evals)
qqnorm(evals)
abline(0,1)
ks.test(evals,pnorm,0,1)

par(mfrow=c(1,5))
plot(fitted(lm_f),evals)
plot(tobacco,life,xlab = "Tobacco consumption",ylab = "Life expectancy")
plot(expenditure,life,xlab = "Expenditure",ylab = "Life expectancy")
plot(alcohol,life,xlab = "Alcohol consumption",ylab = "Life expectancy")
plot(obesity,life,xlab = "Obesity rates",ylab = "Life expectancy")

lm_t <- lm(life~tobacco + log(expenditure) + alcohol + obesity)
summary(lm_t)
evals1 <- stdres(lm_t)
plot(fitted(lm_t),evals1)
ks.test(evals1,pnorm,0,1)

lm1 <- lm(life~tobacco + log(expenditure) + alcohol)
anova(lm1,lm_t)

lm2 <- lm(life~tobacco + log(expenditure) + obesity)
anova(lm2,lm_t)

lm3 <- lm(life~tobacco + alcohol + obesity)
anova(lm3,lm_t)

lm4 <- lm(life~log(expenditure) + alcohol + obesity)
anova(lm4,lm_t)

lm4_1 <- lm(life~alcohol + obesity)
anova(lm4_1,lm4)
lm4_2 <- lm(life~log(expenditure) + obesity)
anova(lm4_2,lm4)
lm4_3 <- lm(life~log(expenditure) + alcohol)
anova(lm4_3,lm4)

summary(lm4)

uk_values <- who[who$country == "United Kingdom",]
predictions.PI <- data.frame(predict.lm(lm4,uk_values,
                                        interval = "prediction", 
                                        level = 0.95))
predictions.PI
uk_values$life