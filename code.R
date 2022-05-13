library(MASS)

load("MAS223.RData")
who_exUK <- who[who$country != "United Kingdom",]
attach(who_exUK)

#cor(tobacco,life)
#cor(expenditure,life)
#cor(alcohol,life)
#cor(obesity,life)

lm_f <- lm(life~tobacco + expenditure + alcohol + obesity)
lm1 <- lm(life~tobacco + expenditure + alcohol)
lm2 <- lm(life~tobacco + alcohol + obesity)
lm3 <- lm(life~expenditure + alcohol + obesity)
lm4 <- lm(life~tobacco + expenditure + obesity)
lm5 <- lm(life~expenditure + obesity)

par(mfrow=c(1,2))
evals <- stdres(lm_f)
hist(evals)
qqnorm(evals)
abline(0,1)
ks.test(evals,pnorm,0,1)

anova(lm_f,lm1)
anova(lm_f,lm2)
anova(lm_f,lm3)
anova(lm_f,lm4)
anova(lm3,lm5)

uk_values <- who[who$country == "United Kingdom",]
predictions.PI <- data.frame(predict.lm(lm3,uk_values,
                                        interval = "prediction", 
                                        level = 0.95))
predictions.PI
uk_values$life