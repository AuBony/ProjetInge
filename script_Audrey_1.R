# ProJET INGENIEUR
# Création du jeu de données complet avec les son Mars et les sons Notchi
# Audrey Bony
# 2/12/2020


#Import des données Notchi
library(tuneR)
notchi1_wav <- readWave("data/Notchi 1.wav")
notchi2_wav <- readWave("data/Notchi 2.wav")

notchi1_left <- notchi1_wav@left
notchi2_left <- notchi2_wav@left

#Les vecteurs doivent avoir la taille du jdd final
len_max <- length(son)
length(notchi1_left) <- len_max
length(notchi2_left) <- len_max
notchi1_left[is.na(notchi1_left)] <- 0
notchi2_left[is.na(notchi2_left)] <- 0

#bind des deux vecteurs
notchi_left <- rbind(notchi1_left, notchi2_left)
str(notchi_left)

son2 <- as.data.frame(rbind(as.matrix(son), notchi_left))

#
notchi_chat_1 <- c("Notchi", NA, NA, NA, 0, 12)
notchi_chat_2 <- c("Notchi", NA, NA, NA, 1, 8)

levels(chat$Cat_name) <- c(levels(chat$Cat_name), "Notchi")


chat2 <- as.data.frame(rbind(chat, notchi_chat_1, notchi_chat_2))

#Enregistrement
save(chat2, son2, file = "data/Mars.RData")

