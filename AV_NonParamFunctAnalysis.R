####
# Non Parametric Functional Data Analysis
# https://www.math.univ-toulouse.fr/~ferraty/SOFTWARES/NPFDA/
####

library(FactoMineR)


######################################
##### Data importation
######################################

#### Spectrometric Data

spec <- read.table("~/2020-2021/PROJET-INGE/npfda-spectrometric.txt")
View(spec)

plot(x = seq(850,1050,length.out=100), y = spec[1,c(1:100)], 
     ylim = c(2,5.5), type = 'l',
     ylab = 'Absorbance', xlab = 'wavelengths', main = 'Spectrometric curves')
for (i in 2:215){
  lines(x = seq(850,1050,length.out=100), y = spec[i,c(1:100)])
}

#### Phoneme Data

pho <- read.table("~/2020-2021/PROJET-INGE/npfda-phoneme.txt")
View(pho)
pho[,151] <- as.factor(pho[,151])

col <- c('blue','red','green','orange','pink')
plot(x = seq(1,150,length.out=150), y = pho[1,c(1:150)], 
     type = 'l', col = col[as.integer(pho[1,151])],
     ylim = c(0,25), ylab = 'magnitude',
     xlab = 'frequencies', main = 'Phonemes')
for (i in 2:2000){
  lines(x = seq(1,150,length.out=150), y = pho[i,c(1:150)],
        col = col[as.integer(pho[i,151])])
}

#### Cat data

load("~/2020-2021/PROJET-INGE/Mars.RData")

test <- son2[,seq(1, 1102500, by = 100)]


######################################
##### PCA
######################################

#### Spectrometric Data

res.PCA1 <- PCA(spec[,c(1:100)], graph = FALSE)
plot.PCA(res.PCA1, choix = "var", label = "none")
plot.PCA(res.PCA1, choix = "ind", label = "none")

#### Phoneme Data

res.PCA2 <- PCA(pho, quali.sup = 151, graph = FALSE)
plot.PCA(res.PCA2, choix = "var", label = "none")
plot.PCA(res.PCA2, choix = "ind", label = "none", habillage = 151)

#### Cat data

res.PCA3 <- PCA(test, graph = FALSE)
plot.PCA(res.PCA3, choix = "var", label = "none")
plot.PCA(res.PCA3, choix = "ind")


