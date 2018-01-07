setwd("C:/bujji/SEM2/MATH 261B/Data")

poker <- read.table("poker_num.txt",header=F,col.names=c("skill","hand","limit","cash"))

attach(poker)

skill <- factor(skill); levels(skill) <- c("Expert","Novice")
hand <- factor(hand); levels(hand) <- c("Bad","Neutral","Good")
limit <- factor(limit); levels(limit) <- c("Fixed","None")

mean(cash)
(tapply(cash,list(skill,hand,limit),mean)); (tapply(cash,list(skill,hand),mean))
(tapply(cash,list(skill,limit),mean)); (tapply(cash,list(hand,limit),mean))
(tapply(cash,skill,mean)); (tapply(cash,hand,mean)); (tapply(cash,limit,mean)) 

plot(cash~skill, main = "Boxplot of Balance by Skill type",xlab = " Skill", ylab= "Balance")
plot(cash~hand, main = "Boxplot of Balance by Hand type",xlab = " Hand", ylab= "Balance")
plot(cash~limit, main = "Boxplot of Balance by Limit type",xlab = " Limit", ylab= "Balance")

par(mfrow=c(2,2))


interaction.plot(hand[skill=="Expert"],limit[skill=="Expert"],cash[skill=="Expert"],
                 ylim=c(0,18),xlab="Hand", ylab="Cash Balance",main="Experts",trace.limit="lim")

interaction.plot(hand[skill=="Novice"],limit[skill=="Novice"],cash[skill=="Novice"],
                 ylim=c(0,18),xlab="hand", ylab="Cash Balance",main="Novices")

interaction.plot(hand[limit=="Fixed"],skill[limit=="Fixed"],cash[limit=="Fixed"],
                 ylim=c(0,18),xlab="hand", ylab="Cash Balance",main="Fixed Limit")

interaction.plot(hand[limit=="None"],skill[limit=="None"],cash[limit=="None"],
                 ylim=c(0,18),xlab="hand", ylab="Cash Balance",main="No Limit")

poker.aov1 <- aov(cash ~ as.factor(skill)*as.factor(hand)*as.factor(limit), data=poker.avg)
summary(poker.aov1)
#################Avg model###################

poker.avg = aggregate(cash ~ skill*hand*limit, poker, mean)
fit.avg <- lm(cash~-1 + skill*hand*limit, data=poker.avg)
anova(fit.avg)
coef(fit.avg)

plotavg = qqnorm(coef(fit.avg), main="Probability plot for factor effects")
qqline(coef(fit.avg))
text(plotavg$x, plotavg$y, names(plotavg$y))


fitavg.red1 <- lm(cash~hand+ skill*limit,data=poker.avg)
anova(fitavg.red1)
summary(fitavg.red1)

fitavg.red2 <- lm(cash~skill*hand,data=poker.avg)
anova(fitavg.red2)
summary(fitavg.red2)

fitavg.red3 <- lm(cash~hand,data=poker.avg)
anova(fitavg.red3)
summary(fitavg.red3)

qqnorm(rstandard(fitavg.red1))
abline(0,1)
plot(fitted(fitavg.red1),rstandard(fitavg.red1), main = " Residuals vs.Predicted values", xlab="fitted values", ylab="standardized residuals")
abline(0,0)

qqnorm(rstandard(fitavg.red2))
abline(0,1)
plot(fitted(fitavg.red2),rstandard(fitavg.red2), main = " Residuals vs.Predicted values", xlab="fitted values", ylab="standardized residuals")
abline(0,0)

qqnorm(rstandard(fitavg.red3))
abline(0,1)
plot(fitted(fitavg.red3),rstandard(fitavg.red3), main = " Residuals vs.Predicted values", xlab="fitted values", ylab="standardized residuals")
abline(0,0)

####sd##################
poker.sd = aggregate(cash ~ skill*hand*limit, poker, sd)
fit.sd <- lm(cash~-1 + skill*hand*limit, data=poker.sd)
anova(fit.sd)
coef(fit.sd)
summary(fit.sd)

plotsd = qqnorm(coef(fit.sd), main="Probability plot for factor effects")
qqline(coef(fit.sd))
text(plotsd$x, plotsd$y, names(plotsd$y))


fitred1.sd <- lm(cash~skill+hand+limit+skill*hand+skill*hand*limit+hand*limit, data=poker.sd)
anova(fitred1.sd)
summary(fitred1.sd)
coef(fitred1.sd)

fitred2.sd <- lm(cash~limit+hand*limit, data=poker.sd)
anova(fitred2.sd)
summary(fitred2.sd)
coef(fitred2.sd)


qqnorm(rstandard(fit.sd))
abline(0,1)
plot(fitted(fit.sd),rstandard(fit.sd), main = " Residuals vs.Predicted values", xlab="fitted values", ylab="standardized residuals")
abline(0,0)

qqnorm(rstandard(fitred2.sd))
abline(0,1)
plot(fitted(fitred2.sd),rstandard(fitred2.sd), main = " Residuals vs.Predicted values", xlab="fitted values", ylab="standardized residuals")
abline(0,0)

dev.off()