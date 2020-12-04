# ProJET INGENIEUR
# Script initial à lancer (import de l'espace de travail)
# Audrey Bony
# 2/12/2020

load(file = "data/Mars.RData")
#load(file = "data/noise_data_agrocampus.RData")

chat2$nb_bk <- as.integer(as.character(chat2$nb_bk))
chat2$nb_bit <- as.integer(as.character(chat2$nb_bit))

dt <- 25/1102500
f <- 1/dt