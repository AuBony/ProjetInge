load("data/data_agrocampus.RData")
#load("~/2020-2021/PROJET-INGE/data_agrocampus.RData")

son <- model_data[, 7:1102506]
chat <- model_data[,1:6]

toy_chat <- chat[1:10,]
dt <- 25/1102500
f <- 1/dt

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
a <- readWave("data/Notchi 2.wav")
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
library(signal)
require(tuneR)

  #Notchi

    #Avant filtre    
play(a)
    #Filtre 1
h1 <- c(0.5, 0.5)
a_h1 <- conv(h1, a@left)
a_h1_wav <- Wave(a_h1)
play(a_h1_wav)
    #Filtre 2
h2 <- rep(0.1, 10)
a_h2 <- conv(h2, a@left)
a_h2_wav <- Wave(a_h2)
play(a_h2_wav)

h2_padded <- c(h2, rep(0, 1014))
h2freq <- fft(h2_padded)

    #rmnoise
a_rmnoise <- rmnoise(a, f = f, output = "Wave")
play(a_rmnoise)
class(a_rmnoise)

a_test <- a
a_test@left <- a_rmnoise@left
play(a_test)

  #Données chats
chat_32 <- a
chat_34 <- a
chat_39 <- a
chat_40 <- a

chat_32@left <- as.integer(son[32,400000:1102500])
chat_34@left <- as.integer(son[34,400000:1102500])
chat_39@left <- as.integer(son[39,400000:1102500])
chat_40@left <- as.integer(son[40,])

play(chat_32)
play(chat_34)
play(chat_39)
play(chat_40)


chat_1 <- son[1,]
play(chat_1)
class(a_h1_wav)
chat_1@left <- son[1,]
b <- a
b@samp.rate <- f
b@left <- as.integer(son[32,400000:1102500])
c <- a 
c@left <- as.integer(son[34,400000:1102500])


class(a@left)
class(as.integer(son[1, 1:400000]))
