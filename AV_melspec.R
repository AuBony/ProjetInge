library(tensorflow)
library(keras)
library(tuneR)
library(seewave)


dataset_path = "E:/ProjetInge/audio_brute"
json_path = "data_melspec.json"
chat_path = "E:/ProjetInge"
n_fft = 2048 # interval we consider to apply FFT
sr = 22050 # sample rate
n_mels = 20 # nb of mel bands
hop_length = 512 # sliding window for FFT

inputs <- list()
outputs <- list()

chat <- read.table(paste0(chat_path,"/chat.csv"), 
                   sep = ";", header = TRUE)

for (i in 1:nrow(chat)){}
  
  # chargement des outputs (nb_bk et nb_bit)
  outputs[1] <- c(as.integer(chat[1,5]), as.integer(chat[1,6]))
  
  # chargement des audio
  audio <- readWave(paste0(dataset_path,"/chat_",1,".wav"))
  
  freq <- fft(audio@left, inverse = FALSE)
  
  mel_freq <- mel(freq, inverse = FALSE)
  
  spectro(wave = audio, f = 22050)
  
  # conversion FFT freq bins en Mel bins
  filter <- tuneR:::fft2melmx(n_fft, sr = sr, nfilts = n_mels, width = 1,
                      htkmel = FALSE, constamp = FALSE)
  
  weights <- filter$wts
  
  mel_spec <- tuneR:::fft2melmx(n_fft, sr = sr, nfilts = n_mels)*fft(n_fft)
  plot(weights)
  
  keras:::layer_mel_spectrogram(audio, num_mel_bins = n_mels, sample_rate = sr)
                    
}


  # charge audio file
  scale_file = dataset_path+"/chat_"+str(i-1)+".wav"
  scale, sr = librosa.load(scale_file)
  
  # Mel filter banks
  filter_banks = librosa.filters.mel(n_fft = n_fft, sr = sr, n_mels = n_mels)
  
  # Mel spectro
  mel_spectrogram = librosa.feature.melspectrogram(scale, sr = sr, n_fft = n_fft,
                                                   hop_length = hop_length, n_mels = n_mels)
  log_mel_spectrogram = librosa.power_to_db(mel_spectrogram)
  
  # adding to items
  data["inputs"].append(log_mel_spectrogram)

###################################################
###################################################

x<-seq(0,10000,by=50)
y<-mel(x)
plot(x,y,type="l",xlab = "f (hertz)", ylab = "f (mel)",
     main = "Mel scale", col="red")



