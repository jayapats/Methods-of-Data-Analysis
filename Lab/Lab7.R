library(Sleuth3)
library(ggplot2)

case0601_aov<-aov(Score~Handicap, data=case0601)
anova(case0601_aov)
with(case0601, unlist(lapply(split(Score,Handicap), mean)))
with(case0601, unlist(lapply(split(Score,Handicap), length)))

#Tukey Kramer
(SE <- sqrt(2.6665) * sqrt(1/14 + 1/14))

#qtukey(percentile,I,Res DF)
qtukey(0.95, 5, 65)

#Multiplier for Tukey
(M <- qtukey(0.95, 5, 65) / sqrt(2))

4.05 - 4.428571 - M*SE
4.05 - 4.428571 + M*SE

#To calculate for all of them
TukeyHSD(case0601_aov)

#R-Markdowm, table formatting for Dunnet's Procedure
library(xtable)
print(xtable(TukeyHSD(case0601_aov)$Handicap, 
             caption="95\\% Tukey Confidence Intervals"), 
      comment=FALSE, caption.placement="top")

library(multcomp)

#Dunnetts procedure - Control Group
summary(case0601$Handicap) # Check the original ordering.
case0601$Handicap <- relevel(case0601$Handicap, "None") # Put None first.
summary(case0601$Handicap) # Check to make sure of the order.

case0601_aov <- aov(Score~Handicap, data=case0601)

case0601_glht<- glht(case0601_aov, linfct=mcp(Handicap="Dunnett"))
confint(case0601_glht)

#Schafee's Procedure

#Getting Quantile from F-distribution
#qf(0.95 for 95%, I-1, DF of Residual)
qf(0.95, 4, 65)

#sqrt((I-1) * qf)
(M<-sqrt(4 * qf(0.95, 4, 65)))

SE <- sqrt(2.6665) * sqrt((0.5)^2/14 + (0.5)^2/14 + (0.5)^2/14 + (0.5)^2/14)
(5.921429+5.342857 )/2 - (4.428571+4.05)/2 - M*SE
(5.921429+5.342857 )/2 - (4.428571+4.05)/2 + M*SE

#Bonferroni Procedure
#3 pre-planned -same as in lecture notes
alpha <- 0.05/3 # Set Bonferroni alpha to nominal alpha divided by k.


(M <- qt(1-alpha/2, 65))

pt_est <- 4.9 - (4.428571+5.921429+4.05+5.342857)/4
SE <- sqrt(2.6665)*sqrt(1/14 + 4*(0.25)^2/14)
pt_est - M*SE
pt_est + M*SE

#To compare all 10 pair wise comparisons
qt(1-(0.05/10)/2,65)
