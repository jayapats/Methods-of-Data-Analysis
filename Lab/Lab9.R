library(Sleuth3)
library(ggplot2)

case0701_lm <- lm(Distance~Velocity, data=case0701)

summary(case0701_lm)

#Cal t-stat
0.3991704/0.1186662

#pt gives area to the left
2*(1-pt(3.363809,22))

#for 95% CI
qt(0.975, 22)

0.3991704 - qt(0.975, 22)*0.1186662
0.3991704 + qt(0.975, 22)*0.1186662

confint(case0701_lm)

#gives 90% CI
confint(case0701_lm, level=0.9)

0.0013724 - qt(0.975, 22)*0.0002278
0.0013724 + qt(0.975, 22)*0.0002278

0.0013724/0.0002278

2*(1-pt(6.024583,22))

#-1 says no intercept
case0701_noint <- lm(Distance~Velocity-1, data=case0701)
summary(case0701_noint)

0.0019214 - qt(.975, 23)*0.0001913
0.0019214 + qt(.975, 23)*0.0001913

confint(case0701_noint)

plot(case0701_lm, which=2)
