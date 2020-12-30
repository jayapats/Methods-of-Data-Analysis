library(Sleuth3)
library(ggplot2)

View(case0202)
qplot(Unaffected-Affected, data=case0202, geom="histogram")

diffs <- case0202$Unaffected-case0202$Affected
diffs <- with(case0202, Unaffected-Affected)

diffs

qplot(diffs, geom="histogram")

t.test(diffs)

with(case0202, t.test(Unaffected, Affected, paired=TRUE))

#This gives two sample t-test which is not right for twin studies
#It assumes that the 2 samples are independent
with(case0202, t.test(Unaffected, Affected))

(Ybar <- mean(diffs)) #Calculate the sample mean of the differences
(s <- sd(diffs))     #Calculate the sample standard deviation
(n <- length(diffs))         #Find the sample size

(se_Ybar <- s/sqrt(n))    #Calculate the SE of the sample mean

Ybar/se_Ybar

pt(3.228928, 14)

1-pt(3.228928, 14)

2*(1-pt(3.228928, 14))

2*pt(3.228928, 14, lower.tail=FALSE)

qt(.975, 14)

Ybar - qt(.975, 14)*se_Ybar #Lower end point of CI
Ybar + qt(.975, 14)*se_Ybar #Upper endpoint of CI

qplot(factor(Year), Depth, data=case0201, geom="boxplot")

t.test(Depth~Year, data=case0201, var.equal=TRUE)

t.test(Depth~Year, data=case0201, var.equal=TRUE, alternative="less")

HW2Dat <- read.csv("NavDat.csv")

HW2Dat <- read.csv("D:/D drive contents/Fall 2020/Stats 511/Class/HW/HW2/NavDat.csv")

head(HW2Dat)
