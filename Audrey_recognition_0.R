# PROJET INGENIEUR
# MACHINE LEARNING ET CLASSIFICATION : Create train audio (croc and mach)
# Audrey Bony
# 7/12/2020
# Source : https://blogs.rstudio.com/ai/posts/2018-06-06-simple-audio-classification-keras/

library(tuneR)
library(seewave)

# Import ----
notchi1_wav <- readWave("../data/audio_brute/chat_193.wav")
notchi2_wav <- readWave("../data/audio_brute/chat_194.wav")

# Croc ----
## Notchi2 
croc1_wav <- cutw(notchi2_wav, from = 4, to = 4.8, f = notchi2_wav@samp.rate, output = "Wave")
writeWave(croc1_wav, paste0("../data/audio_train/croc_notchi2.wav"))


# Mach ----
##Notchi1

mach_Notchi1_1_wav <- cutw(notchi1_wav, from = , to = ,  f = notchi2_wav@samp.rate, output = "Wave")

## Notchi2 
mach_Notchi2_1_wav <- cutw(notchi2_wav, from = , to = ,  f = notchi2_wav@samp.rate, output = "Wave")
writeWave(mach2_1_wav, paste0("../data/audio_train/", "mach", "mach_Notchi2_", 1, ".wav"))

