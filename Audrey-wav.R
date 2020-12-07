# ProJET INGENIEUR
# Création des audio Mars
# Audrey Bony
# 4/12/2020


library(seewave)
library(tuneR)

a <- readWave("data/Notchi 2.wav")
#Ecoute tt les chats bro
#n <- 1
chat_t <- a

for (i in 1:99){
  chat_t@left <- as.integer(son2[n,])
  #play(chat_t)
  writeWave(chat_t, paste0("data/audio/chat_", n, ".wav"))
  n <- n+1
}

for (i in 1:5){
  chat_t@left <- as.integer(son2[n,])
  play(chat_t)
  writeWave(chat_t, paste0("data/audio/chat_", n, ".wav"))
  n <- n+1
}

#Noise 
load("../data/noise_data_agrocampus.RData")
a <-  readWave("../data/Notchi 2.wav")

noise1_wav <- a
noise1_wav@left <- as.integer(noise1[1,])
writeWave(noise1_wav, paste0("../data/audio/noise1.wav"))

noise2_wav <- a
noise2_wav@left <- as.integer(noise2[1,])
writeWave(noise2_wav, paste0("../data/audio/noise2.wav"))

noise3_wav <- a
noise3_wav@left <- as.integer(noise3[1,])
writeWave(noise3_wav, paste0("../data/audio/noise3.wav"))

noise5_wav <- a
noise5_wav@left <- as.integer(noise5[1,])
writeWave(noise5_wav, paste0("../data/audio/noise5.wav"))