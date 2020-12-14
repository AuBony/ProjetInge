# ProJET INGENIEUR
# FILTRER : exploration de la méthode ICA
# Audrey Bony
# 10/12/2020

load("../data/Mars.RData")
test <- son2[1:20, 1:1000000]

# JADE ----
library(JADE)
colnames(test) <- seq(1, 1000)
colnames(test) <- as.Date((colnames(test)), "%Y")
str(test)
AMUSE(test, k = 1)

# fastICA ----
library(fastICA)
mat <- as.matrix(son2[1:20, 1:1000])
head(mat) ; class(mat)

res <- fastICA::fastICA(mat, n.comp = 2)
res$A
res$X
res$K
dim(res$S)
res$S

plot(res$X)
text(res$X, labels = rownames(mat), pos = 2) 

plot(res$X %*% res$K, main = "PCA components")
text(res$X %*% res$K, labels = rownames(mat), pos = 2) 

plot(res$S, main = "ICA components")
text(res$S, labels = rownames(mat), pos = 2)


plot(1:1000, mat[1,], type = "l",
     xlab = "", ylab = "")
plot(1:1000, res$S[,1])
plot(1:20, res$S[,2])

res$W

# retest
son <- as.data.frame(t(as.matrix(test)))
son_scale <- scale(son, center = FALSE, scale = apply(son, 2, sd))
St <- ts(son_scale, start = 0, frequency = 44100)
 
jade <- JADE(son_scale)
sobi <- SOBI(St)
nsstdjd <- NSS.TD.JD(St)

dim(coef(jade))
jade$A

Z <- bss.components(jade)
library(BBmisc)
library(tuneR)
NSSTDJDwave1 <- normalize(Wave(left = as.numeric(Z[, 1]), samp.rate = 44100, bit = 16), unit = "16")
play(NSSTDJDwave1) #voix
NSSTDJDwave2 <- normalize(Wave(left = as.numeric(Z[, 2]), samp.rate = 44100, bit = 16), unit = "16")
play(NSSTDJDwave2) #frottement
NSSTDJDwave3 <- normalize(Wave(left = as.numeric(Z[, 3]), samp.rate = 44100, bit = 16), unit = "16")
play(NSSTDJDwave3) 
NSSTDJDwave4 <- normalize(Wave(left = as.numeric(Z[, 4]), samp.rate = 44100, bit = 16), unit = "16")
play(NSSTDJDwave4) 
NSSTDJDwave5 <- normalize(Wave(left = as.numeric(Z[, 5]), samp.rate = 44100, bit = 16), unit = "16")
play(NSSTDJDwave5) 

play(readWave("../data/audio_brute/chat_4.wav"))

plot.ts(Z[,1:5], nc = 1)
