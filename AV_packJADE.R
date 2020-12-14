load("~/2020-2021/PROJET-INGE/Mars.RData")

library(JADE)
library(tuneR)
library(BBmisc)

cat1 <- which(chat2$Cat_name == 'Cat1')
ech <- seq(1, 1102500, by = 200)
son <- son2[cat1,ech]
  
X <- as.data.frame(t(as.matrix(son)))
X <- scale(X, center = FALSE, scale = apply(X, 2, sd))

jade <- JADE(X)

plot(jade$S[,1], type = 'l')

S <- matrix(ts(jade$S[,1], start = 0, end = 25, frequency = 220), ncol = 1)
for (i in 2:21){
  S <- cbind(S, ts(jade$S[,i], start = 0, end = 25, frequency = 220))
}
colnames(S) <- seq(1,ncol(S))
plot(S[,c(1:7)], plot.type = 'multiple', main = 'Sources 1 à 7')
plot(S[,c(8:14)], plot.type = 'multiple', main = 'Sources 8 à 14')
plot(S[,c(15:21)], plot.type = 'multiple', main = 'Sources 15 à 21')

Z <- bss.components(jade)
Zwave1 <- tuneR:::normalize(Wave(left = as.numeric(Z[, 1]), samp.rate = 220, bit = 16), unit = '16')
play(Zwave1)
Zwave13 <- tuneR:::normalize(Wave(left = as.numeric(Z[, 13]), samp.rate = 220, bit = 16), unit = '16')
play(Zwave13)
Zwave3 <- normalize(Wave(left = as.numeric(Z[, 3]), samp.rate = 44100, bit = 16), unit = "16")
play(NSSTDJDwave3) 
NSSTDJDwave4 <- normalize(Wave(left = as.numeric(Z[, 4]), samp.rate = 44100, bit = 16), unit = "16")
play(NSSTDJDwave4) 
NSSTDJDwave5 <- normalize(Wave(left = as.numeric(Z[, 5]), samp.rate = 44100, bit = 16), unit = "16")
play(NSSTDJDwave5) 

####
# SOBI et ?? 
# fastICA
