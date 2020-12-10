# ProJET INGENIEUR
# FILTRER : exploration de la méthode ICA
# Audrey Bony
# 10/12/2020

load("../data/Mars.RData")
test <- son2[1:20, 1:1000]

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

