library(Sleuth3)
library(ggplot2)

View(case0501)

summary(case0501$Diet)

qplot(Diet, Lifetime, data=case0501, geom="boxplot")

case0501_aov <- aov(Lifetime~Diet, data=case0501)

case0501_aov

anova(case0501_aov)

#T test of beak depth(here we run again for comparison)
#To say that Anova is a generalization test.
# Earlier-> we did one sided, but as Anova is 2 sided, we do a 2 sided here
t.test(Depth~Year, data=case0201, var.equal=TRUE)

case0201_aov <- aov(Depth~Year, data=case0201)
anova(case0201_aov)

#If we square the t-statistics, we get the F-statistics
(-4.5833)^2

#Pooled standard deviation which was calculated before=9730406
#from text book-> estimate of population SD
#Therefore sigma sq is given in Anova-> Residual mean sq
0.9730406^2 #Estimate of pop variance, sigma sq

166.638/176 

#Mean sq = Sum sq/DF

#Comparison betw groups
#To calculate point estimates

#To get sample means for the 6 Diets
with(case0501, unlist(lapply(split(Lifetime, Diet), mean)))

#Sample Sizes, can also be got from summary
with(case0501, unlist(lapply(split(Lifetime, Diet), length)))

#Fstat-57, gives the comparison between two models
# two models described by H0(Reduced/Null model) and HA
#Comparing the equal means model with the complex separate means model
#H0 -> All pop are equal
#HA -> negation of H0,atleast one is different
anova(case0501_aov)

#total no of rows
nrow(case0501) # Find total sample size
length(unique(case0501$Diet)) # How many different groups?

#Therefore Residual DF = 349-6 = 343
# DF of diet - called as extra DF.It is not same as Residual DF which is samole size-No of param
#Its a comparison betw the model means of equal and separate models
#Simple mean model has just 1 mean parameter whereas Separate mean model has 6 mean parameters
#therefore 6-1 = 5

#Plus sign located at the sample mean of each group
ggplot(data=case0501, aes(x=Diet, y=Lifetime)) +
  geom_boxplot() +
  stat_summary(fun=mean, geom="point", shape=3, size=3)

#Fstatistics -> Ratio of Diet Mean sq to the Residual Mean Sq
2546.8/44.6

#Fstat- quite large
#Pval and F-stats tells that the full model has lot of variations.
#Pval< smal -> less evidence that the means are all same.