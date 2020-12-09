
import librosa
import librosa.display
import IPython.display as ipd
import matplotlib.pyplot as plt

## Loading Audio file with Librosa

scale_file = "E:/ProjetInge/audio_brute/chat_1.wav"

# play audio
# ipd.Audio(scale_file)

scale, sr = librosa.load(scale_file)

## Mel filter banks (triangular filter)

filter_banks = librosa.filters.mel(n_fft = 2048, sr = 22050, n_mels = 20)
# n_fft = frame size
# sr = sample rate
# n_mels = nb of mel bands

print(filter_banks.shape)
# (10,1025) (nb of mel bands, framesize/2 +1)

plt.figure(figsize=(25, 10))
librosa.display.specshow(filter_banks,
                         sr = sr,
                         x_axis = "linear")
# can use specshow to display any matrix
plt.colorbar(format="%+2.f")
plt.show()

## Extracting Mel Spectrogram

mel_spectrogram = librosa.feature.melspectrogram(scale, sr = sr, n_fft = 2048,
                                                hop_length = 512, n_mels = 20)

print(mel_spectrogram.shape)
# (10, 1077) (nb mel bands, nb of frame/bins we extract from the signal)

log_mel_spectrogram = librosa.power_to_db(mel_spectrogram)
# way we actually percive amplitude is logarithmic not linear

print(log_mel_spectrogram.shape)
# (10, 1077) (same)

plt.figure(figsize=(25, 10))
librosa.display.specshow(log_mel_spectrogram,
                         x_axis = "time",
                         y_axis = "mel",
                         sr = sr)
plt.colorbar(format="%+2.f")
plt.show()

