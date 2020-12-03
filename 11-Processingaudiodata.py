# 11- Processing audio data for Deep Learning
# Date : 3/12/2020
# Audrey
# lien : https://www.youtube.com/watch?v=Oa_d-zaUti8&t=488s
os.chdir("E:\\Cours\\M2_Stat\\Projet_inge")

import librosa, librosa.display
import matplotlib.pyplot as plt
import numpy as np

file = "E:\\Cours\\M2_Stat\\Projet_inge\\data\\Notchi 2.wav"
#waveform
signal,  sr = librosa.load(file, sr = 22050) # sr * T = 22050 * 8

librosa.display.waveplot(signal, sr = sr)
plt.xlabel("Time")
plt.ylabel("Amplitude")
plt.show()

#fft -> spectrum - Power spectrum Plot
fft = np.fft.fft(signal)

magnitude = np.abs(fft)
frequency = np.linspace(0, sr, len(magnitude))

left_frequency = frequency[:int(len(frequency)/2)]
left_magnitude = magnitude[:int(len(magnitude)/2)]

plt.plot(left_frequency, left_magnitude)
plt.xlabel("Frequency")
plt.ylabel("Magnitude")
plt.show()

# stft -> spectogram
n_fft = 2048 #number sample per fft
hop_length = 512
stft = librosa.core.stft(signal)