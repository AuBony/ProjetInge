
############# TESTS #############
### NE PAS LANCER CETTE PARTIE POUR REFAIRE LE JDD ###

library(tuneR)

setwd("~/2020-2021/PROJET-INGE")
sound <- readWave('cleanwav/cathy_A_3.wav')
play(sound)

plot(sound)

sound@samp.rate # 44100
sound@bit # 16
str(sound@left) # int [1:706541]

plot(sound@left, type = 'l')

## -> choisir left ou right channel?
## on prend left pour l'instant


############# CODE #############

# chargement du package

library(tuneR)

# importation des donnees

setwd("~/GitHub/ProjetInge")

res <- read.table('cleandata.csv', sep = ';', header = TRUE)

fich <- list.files('cleanwav')
fichmat <-  matrix(fich, nrow = length(fich))
wavlist <- sapply(paste0('cleanwav/',fichmat), imp_left)

# transformation de la matrice en dataframe, les noms des rows sont les noms 
# des fichiers

df <- data.frame(t(wavlist))
dim(df) # 48 882000
summary(df[,1:10])

# ajout des colonnes resultats (nb_bk et nb_bit)

res <- data.frame(res[,-1], row.names = paste0('cleanwav/',res$filename,'.wav'))
dta <- merge(df, res, by = "row.names")
dta <- data.frame(dta[,-1], row.names = dta[,1])

# plus rapide mais pas à l'abri d'un mélange d'ordre donc mieux d'être sûr que les
# rownames correspondent
#df$nb_bk <- res$nb_bk
#df$nb_bit <- res$nb_bit

# resultat : df de la a autant de lignes que de vidéos, avec 882000 colonnes de 
# données quanti d'amplitude sonore, et 2 colonnes quanti de nb de break et bite

# exportation du dataframe en csv

save.image("~/2020-2021/PROJET-INGE/complete_clean_data.RData")
#write.csv(dta, 'complete_clean_data.csv', col.names = TRUE, row.names = TRUE)

########### FONCTIONS ###########

imp_left <- function(filename){
  # prend en entrée le nom d'un fichier pour renvoyer les valeurs de sa left channel
  # dans un vecteur de 882000 éléments (20 sec) complété avec des 0 à la fin
  vec <- rep(0,882000)
  file <- readWave(filename)
  left <- file@left
  vec[1:length(left)] <- left
  return(vec)
}

# df = tableau avec les intervalles start et end des evenements
# pour calculer le nb de crocs par enregistrement
# df %>% group_by(filename) %>% summarise(nb_bk = n())