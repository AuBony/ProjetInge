# Import des données ----
library(tuneR)
chat1_wav <- readWave("../data/audio_brute/chat_1.wav")

# Processing ----
s1 <- chat1_wav@left

#Convert integer to floating point values form -1 to 1
s1 <- s1/ 2^(chat1_wav@bit -1)

# Plotting the tone ----
timeArray <- (0:(length(chat1_wav@left) - 1)) / chat1_wav@samp.rate
timeArray <- timeArray * 1000 #scale to milliseconde

plot(timeArray, s1, type='l', col='black', xlab='Time (ms)', ylab='Amplitude') 

#Plotting frequency ----
n <- length(s1)
p <- fft(s1)

nUniquePts <- ceiling((n+1)/2)
p <- p[1:nUniquePts] #select just the first half since the second half 
# is a mirror image of the first
p <- abs(p)  #take the absolute value, or the magnitude 

p <- p / n #scale by the number of points so that the magnitude does not depend on the length of the signal or on its sampling frequency  
p <- p^2  # square it to get the power 

# multiply by two (see technical document for details)
# odd nfft excludes Nyquist point
if (n %% 2 > 0){
  p[2:length(p)] <- p[2:length(p)]*2 # we've got odd number of points fft
} else {
  p[2: (length(p) -1)] <- p[2: (length(p) -1)]*2 # we've got even number of points fft
}

freqArray <- (0:(nUniquePts-1)) * (chat1_wav@samp.rate / n) #  create the frequency array 

plot(freqArray/1000, 10*log10(p), type='l', col='black', xlab='Frequency (kHz)', ylab='Power (dB)')
