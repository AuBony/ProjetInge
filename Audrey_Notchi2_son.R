# ProJET INGENIEUR
# Etude du son des chats : Comparaison des sons Notchi qui mache, Notchi qui croque, Enregistrement complet de Notchi2
# Audrey Bony
# 3/12/2020

#Import des données
library(tuneR)
library(seewave)
notchi2_wav <- readWave("data/Notchi 2.wav")

listen(notchi2_wav)
#Selection croc, machage, total sous forme d'un son
croc_wav <- cutw(notchi2_wav, from = 4, to = 4.8, f = notchi2_wav@samp.rate, output = "Wave")
mach1_wav <- cutw(notchi2_wav, from = 0, to = 3, f = notchi2_wav@samp.rate, output = "Wave")
mach2_wav <- cutw(notchi2_wav, from = 5, to = 8, f = notchi2_wav@samp.rate, output = "Wave")

listen(mach1_wav)
listen(mach2_wav)
#Spectrogramme
spectro(notchi2_wav, wl = 2048, collevels = seq(-70, 0, 1), ovlp = 25, flog = 1, fastdisp = TRUE)
spectro(croc_wav)
spectro(mach1_wav)
spectro(mach2_wav)

#MFCCs
size <- 0.01

mfcc_notchi2 <- tuneR::melfcc(notchi2_wav, nband = 80)
mfcc_notchi2
heatmap(t(mfcc_notchi2), Colv = NA, Rowv = NA, scale = "column",
        main = "MFCCs Notchi2")


mfcc_croc <- tuneR::melfcc(croc_wav, wintime = 0.2)
class(mfcc_croc)

heatmap(t(mfcc_croc), Colv = NA, Rowv = NA, scale = "column",
        main = "MFCCs Croc",
        labCol = seq(0, 0.8, size), 
        labRow = paste("Coef ", seq(1,12,1)))

wintime = size, hoptime = size