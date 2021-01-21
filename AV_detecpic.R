##### detection de pic a partir des donnees brutes

load("C:/Users/HP/Documents/GitHub/ProjetInge/complete_clean_data.RData")

test <- dta[,c(seq(1,882000, by = 200), 882001, 882002)]
n <- ncol(test)
plot(1:(n-2), test[2,-c(n-1, n)], type = 'l')

View(test[1:10,c(1:5,4411,4412)])

testcr <- as.data.frame(t(apply(t(test),2,scale)))

l <- 2
plot(1:(n-2), testcr[l,-c(n-1, n)], type = 'l')
print(testcr[l,c(n-1, n)])

# importation avec normalisation des donnees

library(tuneR)

path <- '~/GitHub/ProjetInge/cleanwav'

ech1 <- readWave(paste0(path,'/cathy_A_1.wav'))
ech1norm <- normalize(ech1, center = TRUE)
plot(ech1norm)

ech2 <- readWave(paste0(path,'/cathy_A_2.wav'))
ech2norm <- normalize(ech2, center = TRUE)
plot(ech2norm)

ech3 <- readWave(paste0(path,'/cathy_A_3.wav'))
ech3norm <- normalize(ech3, center = TRUE)
plot(ech3norm)

l <- list(ech1norm,ech2norm,ech3norm)
count <- rep(0,length(l))
n <- 1
for (i in 1:n){
  vec <- l[[i]]@left
  vecsel <- (vec > 0.6 | vec < -0.6)
  which(vecsel)
  
}

# ecart minimal entre 2 crocs? on va prendre 0.1 sec pour l'instant, apres on pourra
# regarder la vraie valeur
# 0.1*44100 = 4410 donc on va prendre un ecart minimal de 4400 unites entre 2 pics

# marche pas si pas de croc car dilate echelle, mais peut-etre a utiliser apres 
# le tri croc/pas croc pour compter sur le signal entier