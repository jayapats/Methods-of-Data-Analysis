library(Sleuth3)
library(ggplot2)

View(case0402)

cbind(case0402, rank(case0402$Time))

qplot(Treatment,Time,data=case0402,geom="boxplot")

qplot(Time,data=case0402,geom="histogram",
        facets=Treatment~.)

summary(case0402$Treatment)

wilcox.test(Time~Treatment, data=case0402, alternative="greater",
            exact=FALSE, correct=FALSE)

with(case0402,wilcox.test(x=Time[Treatment=="Modified"],
                          y=Time[Treatment=="Conventional"],
                          alternative="less",
                          correct=FALSE,
                          exact=FALSE))

wilcox.test(Time~Treatment, data=case0402, exact=FALSE, correct=FALSE,
            conf.int=TRUE)

t.test(Depth~Year, data=case0201) # Not assuming equal variance

diffs <- with(case0202, Unaffected-Affected)

length(diffs)
length(which(diffs>0))

binom.test(14, 15, alternative="greater")
