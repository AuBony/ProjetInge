# PROJET INGENIEUR
# Etude Bruit
# Audrey Bony
# 7/12/2020

require(seewave)
require(tuneR)

par(mfrow = c(1,1))
# Filtrer les interferences
chat_22_wav <- readWave("../data/audio_brute/chat_22.wav")
play(chat_22_wav)

chat_22_wav_cut_interference <- cutw(chat_22_wav, from = 20, to = 22, f = chat_22_wav@samp.rate, output = "Wave")
listen(chat_22_wav_cut_interference)

spec(chat_22_wav_cut_interference, PSD = TRUE, col = 'gray', identify = TRUE)
  #Frequence interférence = 3160 HZ -> entre 3100 et 3500Hz

chat_22_wav_cut_interference_filtrer <- ffilter(chat_22_wav_cut_interference, from = 3000, to = 3200, bandpass = FALSE, output = "Wave")
listen(chat_22_wav_cut_interference_filtrer)
spec(chat_22_wav_cut_interference_filtrer)

#Comparaison avec l'enregistrement original
chat_22_wav_filter <- ffilter(chat_22_wav, from = 3000, to = 3500, bandpass = FALSE, output = "Wave")
spec(chat_22_wav_filter)
listen(chat_22_wav_filter)


freq_interference <- spec(chat_22_wav_cut_interference, flim = c(0,5), alim = c(-2, 150))
freq_interference 

spectro(chat_22_wav_cut_interference, osc = TRUE)
