#######################################################################
# PROJECT : CAT ACCOUSTIC DATA PROJECT
# TITLE : IA3 - Visualisation
# AUTHOR : BONY Audrey & de CROUTTE Anne-Victoire (AGROCAMPUS OUEST)
# DATE : DECEMBER 2020 TO FEBRURARY 2021
#######################################################################

#Library
require(seewave)
require(tuneR)

#Audio
wav <- readWave(filename = "data/wav/cathy_A_1.wav")

#Oscillogram
seewave::oscillo(wav, f = wav@samp.rate, title = TRUE)

#Spectrogram
seewave::spectro(wav, f = wav@samp.rate)
