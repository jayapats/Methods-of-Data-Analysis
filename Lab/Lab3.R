library(Sleuth3)
library(ggplot2)

names(case0301)

qplot(Treatment, Rainfall, data=case0301, geom="boxplot")

log_rainfall<-log(case0301$Rainfall)

qplot(Treatment, log_rainfall, data=case0301, geom="boxplot")

with(case0301, summary(Treatment)) # Check to see which group R puts first.
t.test(log_rainfall~Treatment, data=case0301, var.equal=TRUE, 
       alternative="greater")

t.test(log_rainfall~Treatment, data=case0301, var.equal=TRUE)

exp(0.2408651)
exp(2.0466973)

C_dalli <- read.csv("C_dalli.csv")

View(C_dalli)

qplot(as.factor(time_point), pct_cover, data=C_dalli, geom="boxplot")

logit.pct <- with(C_dalli, log((pct_cover)/(100-pct_cover)))

qplot(as.factor(time_point), logit.pct, data=C_dalli, geom="boxplot")

head(case0302)
qplot(Veteran, Dioxin, data=case0302, geom="boxplot")

plot(Dioxin~Veteran, data=case0302)
with(case0302, identify(x=Veteran, y=Dioxin))

summary(case0302$Veteran) # Check R's ordering of the groups.
t.test(Dioxin~Veteran, data=case0302, var.equal=TRUE, alternative="less")

t.test(Dioxin~Veteran, data=case0302, var.equal=TRUE, alternative="less",
       subset=-646)

t.test(Dioxin~Veteran, data=case0302, var.equal=TRUE, alternative="less",
       subset=-c(646,645))
