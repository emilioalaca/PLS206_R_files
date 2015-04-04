library(faraway)
data(gala)
str(gala)
m1 <- lm(Species ~ Area + Nearest, data = gala)
summary(m1)
str(summary(m1))
obs.F <- summary(m1)$fstatistic["value"]
fstats <- numeric(50000)
for(i in 1:50000){ 
  fstats[i] <- summary(lm(sample(Species) ~ Area + Nearest, data = gala))$fstatistic["value"]
}

(critical.F <- qf(p = 0.95, df1 = summary(m1)$fstatistic["numdf"], df2 = summary(m1)$fstatistic["dendf"]))

length(fstats[fstats > obs.F])/50000
sum(fstats > obs.F)/50000