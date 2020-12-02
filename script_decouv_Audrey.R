load("../data/data_agrocampus.RData")
#load("~/2020-2021/PROJET-INGE/data_agrocampus.RData")

son <- model_data[, 7:1102506]
chat <- model_data[,1:6]

toy_chat <- chat[1:10,]
dt <- 25/1102500
f <- 1/dt

summary(chat)
table(chat$Cat_name, chat$Session)
table(chat$Cat_name, chat$Diet)

############################
# Visualisation exp?rience #
############################
str(chat)
chat$row <- seq(from = 1, to = 192, by = 1)
hist(chat$nb_bit)
hist(chat$nb_bk)
plot(nb_bk ~ Diet, data = chat)
plot(nb_bit ~ Diet, data = chat)

library(ggplot2)
library(plotly)
p <- ggplot(chat, aes(x = nb_bit, y = nb_bk, color = Cat_name)) +
  geom_point() 
ggplotly(p, labels = ~row)

plot_ly(chat, x = ~nb_bit, y = ~nb_bk, text = rownames(chat), text = ~paste('Row : ', row))

#################
# Visualisation #
#################

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
#localpeaks(a@left, f = a@samp.rate, bands = 1) tr?s long

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

  #Donn?es chats
chat_10 <- a
chat_32 <- a
chat_34 <- a
chat_39 <- a
chat_40 <- a

chat_10@left <- as.integer(son[10,])
chat_32@left <- as.integer(son[32,])
chat_34@left <- as.integer(son[34,])
chat_39@left <- as.integer(son[39,])
chat_40@left <- as.integer(son[40,])

play(chat_10)
play(chat_32)
play(chat_34)
play(chat_39)
play(chat_40)

writeWave(chat_10, "data/chat_10.wav")
writeWave(chat_32, "data/chat_l32.wav")
writeWave(chat_34, "data/chat_l34.wav")
writeWave(chat_39, "data/chat_l39.wav")
writeWave(chat_40, "data/chat_l40.wav")



chat_32_read <- readWave(filename = "data/chat_32.wav")
listen(chat_32_read)

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



# Cours R
library(tuneR)
notchi1 <- readWave("data/Notchi 1.wav")
notchi2 <- readWave('data/Notchi 2.wav')

notchi1
notchi2

play(notchi1)
listen(notchi1)
play(notchi2)

plot(notchi1)
plot(notchi2)

plot.ts(notchi1@left)

plot(chat_32_read)

par(mfrow = c(2,1))
notchi1.p <- periodogram(mono(notchi1, 'left'))
plot(notchi1.p, ylim = c(0,0.004))
notchi2.p <- periodogram(mono(notchi2, 'left'))
plot(notchi2.p, ylim = c(0,0.004))
par(mfrow = c(1,1))

notchi1.p1 <- spec(notchi1, type='h')

notchi2_filtered <- ffilter(notchi2, channel = 1, from = 100, to = 5000, output = "Wave", bandpass = FALSE)
listen(notchi2_filtered)
par(mfrow = c(2,1))
plot(notchi2_filtered)
plot(notchi2)
listen(notchi2)

p.notchi2_filetered <- periodogram(mono(notchi2_filtered, 'left'))
plot(p.notchi2_filetered)

ffilter(wave, f, channel = 1, from = NULL, to = NULL, bandpass = TRUE,
        custom = NULL, wl = 1024, ovlp = 75, wn = "hanning", fftw = FALSE,
        rescale=FALSE, listen=FALSE, output="matrix")


###########"
#
############
draw_plot <- function(line,pas,dep,fin){
  dt <- 25/1102500
  dep <- dep
  fin <- fin/dt
  pas <- pas
  temps <- seq(from = dep, to = fin, by = pas)
  indice <- temps + 1
  dta <- data.frame(time = temps[-length(temps)]*dt, 
                    amplitude = as.integer(son[line,temps]))
  
  # graphe
  p1 <- dta %>% ggplot() +
    aes(x = time, y = amplitude) +
    geom_line(size = 0.2) +
    theme_minimal()
  
  p1 <- p1 + annotate("text", x = 23, y = max(dta$amplitude), 
                      label = paste('nb_bk: ',
                                    chat[line,'nb_bk'],'\n',
                                    'nb_bit: ',
                                    chat[line,'nb_bit']),
                      colour = 'orange', size = 5)
  ggplotly(p1)
}

draw_plot('39', 200, 0, 25)

