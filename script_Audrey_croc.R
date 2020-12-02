# ProJET INGENIEUR
# Etude du son des chats : Notchi qui croque
# Audrey Bony
# 2/12/2020

#Import des données
library(tuneR)
notchi2_wav <- readWave("data/Notchi 2.wav")

#Ecoute du son
listen(notchi2_wav)
  #Notchi croque autour de la seconde 4

#Sélection du son de croquage
croc_wav <- cutw(notchi2_wav, from = 4, to = 4.8, f = notchi2_wav@samp.rate, output = "Wave")
  #On vérifie que le son obtenu est bien le son de croc
listen(croc_wav)

#On enregistre le son de croc sous la forme d'un data.frame
croc <- as.integer(cutw(notchi2_wav, from = 4, to = 4.8, f = notchi2_wav@samp.rate))

#

acoustat(croc_wav)
fpeaks(meanspec(croc_wav, wl = 1000), nmax = 1)
spec(croc_wav)
spectro(croc_wav)
