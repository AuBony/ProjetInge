load("~/2020-2021/PROJET-INGE/Mars.RData")

library(JADE)
library(tuneR)
library(BBmisc)

son <- son2[sample(1:194,50,replace = FALSE),seq(1,1102500, by = 200)]
  
S <- as.data.frame(t(as.matrix(son)))
S <- scale(S, center = FALSE, scale = apply(S, 2, sd))
St <- ts(son, start = 0, end = 25, frequency = 22050)

jade <- JADE(S)
sobi <- SOBI(St)



l1 <- as.integer(son2[1,])
l1ts <- ts(l1, start = 0, end = 25, frequency = 44100)
plot.ts(l1ts)
dev.off()
