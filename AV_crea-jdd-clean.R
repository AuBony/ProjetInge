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
wavlist <- apply(fichmat, 1, imp_left)
dta <- matrix(wavlist)


########### FONCTIONS ###########

imp_left <- function(filename){
  file <- readWave(filename)
  return(file@left)
}

