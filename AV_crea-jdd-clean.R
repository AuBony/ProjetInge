library(tuneR)

setwd("~/2020-2021/PROJET-INGE")
sound <- readWave('cathy_C_4.wav')
play(sound)

plot(sound)

sound@samp.rate # 48000
sound@bit # 16
str(sound@left) # int [1:290816]

plot(sound@left, type = 'l')

## -> choisir left ou right channel?
## on prend left pour l'instant

fich <- list.files('cleanwav')
fichmat <-  matrix(fich, nrow = length(fich))
wavlist <- sapply(paste0('cleanwav/',fichmat), imp_left)

# transformer la liste de vecteurs en dataframe, les noms des rows sont les noms 
# des elements de la liste

df <- data.frame(matrix(unlist(wavlist), nrow=length(wavlist), byrow=T))

# PROBLEME QUE PAS LA MEME LONGEUR !!!

########### FONCTIONS ###########

imp_left <- function(filename){
  file <- readWave(filename)
  return(file@left)
}

