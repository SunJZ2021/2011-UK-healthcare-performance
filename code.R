library(MASS)

load("who.RData")
who_exUK <- who[who$country != "United Kingdom",]
attach(who_exUK)

cor(tobacco,life)
cor(expenditure,life)
cor(alcohol,life)
cor(obesity,life)

lm0 <- lm(life~1)

lm1 <- lm(life~tobacco)
lm2 <- lm(life~expenditure)
lm3 <- lm(life~alcohol)
lm4 <- lm(life~obesity)

lm5 <- lm(life~tobacco + expenditure)
lm6 <- lm(life~tobacco + alcohol)
lm7 <- lm(life~tobacco + obesity)
lm8 <- lm(life~expenditure + alcohol)
lm9 <- lm(life~expenditure + obesity)
lm10 <- lm(life~alcohol + obesity)

lm11 <- lm(life~tobacco + expenditure + alcohol)
lm12 <- lm(life~tobacco + expenditure + obesity)
lm13 <- lm(life~tobacco + alcohol + obesity)
lm14 <- lm(life~expenditure + alcohol + obesity)

lm15 <- lm(life~tobacco + expenditure + alcohol + obesity)

anova(lm15,lm11)
anova(lm15,lm12)
anova(lm15,lm13)
anova(lm15,lm14)

anova(lm14,lm10)
anova(lm14,lm9)
anova(lm14,lm8)

evals <- stdres(lm14)
par(mfrow=c(1,2))
hist(evals)
qqnorm(evals)
abline(0,1)

ks.test(evals,pnorm,0,1)
uk_values <- who[who$country == "United Kingdom",]
predictions.PI <- data.frame(predict.lm(lm14,uk_values,
                                        interval = "prediction", level = 0.95))
predictions.PI
uk_values$life