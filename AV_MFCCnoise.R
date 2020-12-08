# ProJET INGENIEUR
# Etude du son des bruits entre croquettes 
# Anne-Victoire de CROUTTE
# 7/12/2020

#Import des données
library(tuneR)
library(seewave)
library(wesanderson)
noise1_wav <- readWave("C:/Users/HP/Documents/2020-2021/PROJET-INGE/noise_wav/noise1.wav")
noise2_wav <- readWave("C:/Users/HP/Documents/2020-2021/PROJET-INGE/noise_wav/noise2.wav")
noise3_wav <- readWave("C:/Users/HP/Documents/2020-2021/PROJET-INGE/noise_wav/noise3.wav")
noise5_wav <- readWave("C:/Users/HP/Documents/2020-2021/PROJET-INGE/noise_wav/noise5.wav")

#Ecoute du son
play(noise2_wav)

#Spectrogramme
spectro(noise1_wav, 
        palette = reverse.heat.colors,
        main = 'Noise1')
spectro(noise2_wav, contlevels = seq(-30,0), 
        palette = reverse.heat.colors,
        main = 'Noise2')
spectro(noise3_wav, 
        palette = reverse.heat.colors,
        main = 'Noise3')
spectro(noise5_wav, 
        palette = reverse.heat.colors,
        main = 'Noise5')

dev.off()

#MFCCs
size <- 0.01
mfcc_noise1 <- tuneR::melfcc(noise1_wav, wintime = size, hoptime = size, numcep = 13)
mfcc_noise2 <- tuneR::melfcc(noise2_wav, wintime = size, hoptime = size, numcep = 13)
mfcc_noise3 <- tuneR::melfcc(noise3_wav, wintime = size, hoptime = size, numcep = 13)
mfcc_noise5 <- tuneR::melfcc(noise5_wav, wintime = size, hoptime = size, numcep = 13)

heatmap(t(mfcc_noise1), Colv = NA, Rowv = NA, scale = "column",
        main = "MFCCs Noise1",
        labCol = seq(0, 68, size), 
        labRow = paste("Coef ", seq(1,12,1)))
heatmap(t(mfcc_noise2), Colv = NA, Rowv = NA, scale = "column",
        main = "MFCCs Noise2",
        labCol = seq(0, 40, size), 
        labRow = paste("Coef ", seq(1,12,1)))
heatmap(t(mfcc_noise3), Colv = NA, Rowv = NA, scale = "column",
        main = "MFCCs Noise3",
        labCol = seq(0, 40, size), 
        labRow = paste("Coef ", seq(1,12,1)))
heatmap(t(mfcc_noise5), Colv = NA, Rowv = NA, scale = "column",
        main = "MFCCs Noise5",
        labCol = seq(0, 20, size), 
        labRow = paste("Coef ", seq(1,12,1)))

library(ggplot2)
library(hrbrthemes)
library(reshape2)

# Dummy data
x <- LETTERS[1:20]
y <- paste0("var", seq(1,20))
data <- expand.grid(X=x, Y=y)
data$Z <- runif(400, 0, 5)

a = melt(mfcc_noise5)
colnames(a) = c('Time', 'Coef', 'Value')

ggplot(a, aes(x = Time, y = Coef, fill = Value)) + 
  geom_tile() +
  scale_fill_gradient(low="blue", high="red") +
  theme_ipsum()



