load("../data/data_agrocampus.RData")
#load("~/2020-2021/PROJET-INGE/data_agrocampus.RData")

son <- model_data[, 7:1102506]
chat <- model_data[,1:6]

toy_chat <- chat[1:10,]
f <- 25/1102500

summary(chat)
table(chat$Cat_name, chat$Session)
table(chat$Cat_name, chat$Diet)

# INTERVALLE DE TEMPS CHOISI
dep <- 1
fin <- 1102500
pas <- 500
temps <- seq(from = dep, to = fin, by = pas)

# courbes 1eres lignes
plot(temps,son[1,temps], type = 'l', xlab = "temps", ylab = "son",
     main = "Courbe sonore")
plot(temps,son[2,temps], type = 'l', xlab = "temps", ylab = "son",
     main = "Courbe sonore")
plot(temps,son[3,temps], type = 'l', xlab = "temps", ylab = "son",
     main = "Courbe sonore")

# lignes interessantes (on peut bien entendre crunching sounds)
chat['32',1:6]
plot(temps,son['32',temps], type = 'l', xlab = "temps", ylab = "son",
     main = "Courbe sonore")
chat['34',1:6]
plot(temps,son['34',temps], type = 'l', xlab = "temps", ylab = "son",
     main = "Courbe sonore")
chat['39',1:6]
plot(temps,son['39',temps], type = 'l', xlab = "temps", ylab = "son",
     main = "Courbe sonore")
chat['40',1:6]
plot(temps,son['40',temps], type = 'l', xlab = "temps", ylab = "son",
     main = "Courbe sonore")


# Seewave with Notchi
library(seewave)
library(tuneR)
?seewave
a <- readWave("../data/Notchi 2.wav")
savewav(a)
seewave(a)
listen(a)
acoustat(a)
ama(a)
drawenv(a)
env(a)
phaseplot(a)

acoustat(a)
afilter(a@left, f = f)

class(a@left)

drawenv(rmnoise(a), f = a@samp.rate)

spec <- meanspec(a@left, f=a@samp.rate)
res3 <- fpeaks(spec = spec, f = a@samp.rate, nmax = 10, freq = 1 ) 

localpeaks(spec, bands=5)
#localpeaks(a@left, f = a@samp.rate, bands = 1) très long

w <- colorRampPalette(c("white","white"))
spectro(a@left, f=a@samp.rate, ovlp=95, palette=w, cont=TRUE, colcont=temp.colors(8), contlevels=seq(-5,0,5), scale=FALSE)

#Noise
