###########################################################################
# PROJECT : CAT ACCOUSTIC
# TITLE : I-B-3-MelspectroMFCC
# AUTHOR : BONY & de CROUTTE (AGROCAMPUS OUEST)
# DATE : DECEMBER 2020 TO FEBRUARY 2021
###########################################################################

## Packages

import librosa
import librosa.display
import IPython.display as ipd
import matplotlib.pyplot as plt
import numpy as np
import os

## Loading Audio file with Librosa

'''
/!\ IMPORTANT /!\
change path to set the working directory in the 'Cat_accoustic' file
'''
# change working directory
os.chdir('C:/Users/HP/Documents/GitHub/ProjetInge/Cat_accoustic')

# path to wav file
scale_file = "data/wav/cathy_A_3.wav"

# take a time interval of a break
with open("data/labels/labels_1/cathy_A_3.txt", 'r') as lab:
    lignes = lab.readlines()
    l1 = lignes[0].split()
    start = float(l1[0])
    end = float(l1[1])
    print(l1)

# import this break
scale, sr = librosa.load(scale_file, sr = 44100,   offset = start, duration = end-start)

# display this break
plt.figure(figsize = (5, 10))
librosa.display.waveplot(scale, sr = 44100)
plt.show()

## Fixing parameters

n_fft = 2048 # frame size
n_mels = 20 # nb of mel bands
hop_length =  512

## Mel filter banks (triangular filter)

filter_banks = librosa.filters.mel(n_fft = n_fft, sr = sr, n_mels = n_mels)

print(filter_banks.shape)
# (20,1025) (nb of mel bands, framesize/2 +1)

plt.figure(figsize=(25, 10))
librosa.display.specshow(filter_banks,
                         sr = sr,
                         x_axis = "linear",
                         y_axis = "hz")
# can use specshow to display any matrix
plt.ylabel("Mel filter")
plt.colorbar()
plt.show()

## Extracting Mel Spectrogram

mel_spectrogram = librosa.feature.melspectrogram(scale, sr = sr, n_fft = n_fft,
                                                 n_mels = n_mels, hop_length = hop_length)

print(mel_spectrogram.shape)
# (10, 1077) (nb mel bands, nb of frame/bins we extract from the signal)

log_mel_spectrogram = librosa.power_to_db(mel_spectrogram)
# way we actually percive amplitude is logarithmic not linear

print(log_mel_spectrogram.shape)
# (10, 1077) (same)

plt.figure(figsize=(5, 10))
librosa.display.specshow(log_mel_spectrogram,
                         x_axis = "time",
                         y_axis = "mel",
                         sr = sr, cmap='magma')
plt.colorbar(format="%+2.0f dB")
plt.show()

## Extracting MFCCs

n_mfcc = 13

# extract 13 MFCCs
MFCCs = librosa.feature.mfcc(scale, sr = sr, n_fft = n_fft, hop_length = hop_length, n_mfcc = n_mfcc)

# display MFCCs
plt.figure(figsize = (5, 10))
librosa.display.specshow(MFCCs,
                         sr=sr,
                         hop_length=hop_length,
                         x_axis = "time",
                         y_axis = "frames")
plt.ylabel("MFCC coefficients")
plt.colorbar()
plt.title("MFCCs")
plt.show()
