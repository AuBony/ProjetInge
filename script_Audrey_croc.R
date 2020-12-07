# ProJET INGENIEUR
# Etude du son des chats : Notchi qui croque
# Audrey Bony
# 2/12/2020

#Import des données
library(tuneR)
library(seewave)
notchi2_wav <- readWave("../data/audio_brute/chat_194.wav")

#Ecoute du son
play(notchi2_wav)
  #Notchi croque autour de la seconde 4

#Sélection du son de croquage
croc_wav <- cutw(notchi2_wav, from = 4, to = 4.8, f = notchi2_wav@samp.rate, output = "Wave")
  #On vérifie que le son obtenu est bien le son de croc
listen(croc_wav)

#On enregistre le son de croc sous la forme d'un data.frame
croc <- as.integer(cutw(notchi2_wav, from = 4, to = 4.8, f = notchi2_wav@samp.rate))

#Spectrogramme
spectro(croc_wav, osc = TRUE)

#MFCCs
size <- 0.01
mfcc_croc <- tuneR::melfcc(croc_wav, wintime = size, hoptime = size)
class(mfcc_croc)

heatmap(t(mfcc_croc), Colv = NA, Rowv = NA, scale = "column",
        main = "MFCCs Croc",
        labCol = seq(0, 0.8, size), 
        labRow = paste("Coef ", seq(1,12,1)))


