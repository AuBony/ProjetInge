###########################################################################
## PROJECT : CAT ACCOUSTIC
## TITLE : II-C-countpeaks
## AUTHOR : BONY & de CROUTTE (AGROCAMPUS OUEST)
## DATE : DECEMBER 2020 TO FEBRUARY 2021
###########################################################################

# PACKAGES ----

library(tuneR)
library(dplyr)
library(ggplot2)
library(plotly)
library(randomForest) # for the random forest model importation and use

# FUNCTIONS ----

imp_norm <- function(file_name, path_wav){
  # INPUTS : 
  # file_name : character, name of the wave file to import and normalize
  # path_wav : character, path to the file
  # OUTPUT : 
  # wave file imported and normalized
  return(normalize(readWave(paste0(path_wav, file_name)), center = TRUE))
}

count_peaks <- function(wav, amp_lim, time_lim){
  # INPUTS:
  # wav: Wave, an element of wavlist (a recording as wave)
  # amp_lim : numeric, amplitude limit of detection (1st threshold)
  # time_lim : numeric, minimal distance between 2 peaks (2nd threshold)
  # OUTPUT:
  # c : numeric, count of peaks in the wav
  c <- 1
  vec <- wav@left
  vecsel <- which(abs(vec) > amp_lim)
  if (length(vecsel) == 0){
    c <- 0
  } else if (length(vecsel) > 1) {
    for (k in 2:length(vecsel)){
      if (abs(vecsel[k]-vecsel[k-1]) > time_lim){
        c <- c + 1
      }
    }}
  return(c)
}

mse <- function(pred, act){
  # INPUTS
  # pred : vector of predictions
  # act : vector of actual values
  # OUPUT
  # MSE
  return(sum((pred - act)^2) / length(pred))
}

# II-C-2) METHOD ON AMPLITUDE CURVES WITH ORIGINAL RECORDINGS ----


### Data importation

# names of the files
fich <- list.files('wav')

# weal labels for the 79 samples
actual <- read.table('nb_bk_79samples.csv', 
                     sep = ';', dec = '.', header = TRUE)
actual$filename <- as.factor(actual$filename)

# wav files importation in a list (79 wav = 79 elements of the list)
wavlist <- lapply(fich, FUN = imp_norm, path_wav = 'wav/')


### Find the optimal couple of parameters (2 thresholds : amplitude (amp_lim) and 
# time (time_lim))

# put the parameters to test in a dataframe, with a  3rd column MSE
param <- expand.grid(amp_lim = seq(0.1, 1, by = 0.1), 
                     time_lim = seq(1000, 60000, by = 2000))
param$mse <- rep(0, nrow(param))

# calculate the MSE for each couple of parameters
for (i in 1:nrow(param)){
  count <- c()
  for (k in 1:nrow(actual)){
    count <- c(count, count_peaks(wav = wavlist[[k]],
                                  amp_lim = param$amp_lim[i],
                                  time_lim = param$time_lim[i]))
  }
  param$mse[i] <- mse(count, actual$nb_bk)
}

# plot the MSE depending on the parameters values as a heatmap
p <- ggplot(param, aes(x = amp_lim, y = time_lim, fill = log(mse))) +
  geom_tile() +
  scale_fill_gradient(low = 'springgreen3', high = 'blue') +
  scale_x_continuous(breaks = seq(0.1, 1, by = 0.1)) +
  scale_y_continuous(breaks = seq(1000, 60000, by = 2000)) +
  theme_minimal()
ggplotly(p)


### Predict with the optimal couple of parameters

# fix the parameters
amp_lim <- param$amp_lim[which.min(param$mse)]
time_lim <- param$time_lim[which.min(param$mse)]

# count the peaks with the 2 optimal thresholds
count1 <- c()
for (k in 1:nrow(actual)){
  count1 <- c(count1, count_peaks(wav = wavlist[[k]],
                                  amp_lim = amp_lim,
                                  time_lim = time_lim))
}
count1
mse(count1, actual$nb_bk)
length(which(count1 == actual$nb_bk))

# draw the plot of prediction vs actual
df1 <- data.frame(actual = actual$nb_bk, prediction = count1)
df1 <- df1 %>% add_count(actual, prediction)
m <- max(actual$nb_bk)+1
title1 <- paste0('Number of breaks, predicted vs actual \n for amp_lim = ', amp_lim,
                 ', time_lim = ', time_lim)
ggplot(df1, aes(x = actual, y = prediction)) +
  stat_density2d(geom='tile', aes(fill=..density..), contour = FALSE) +
  geom_segment(aes(x = -1, y = -1, xend = m, yend = m), 
               colour = 'lightgrey', alpha = 0.5) +
  geom_point(colour='lightgrey', size = 1) +
  scale_x_continuous(breaks = seq(-1, m, by = 1)) +
  scale_y_continuous(breaks = seq(-1, m, by = 1)) +
  theme(plot.title = element_text()) +
  labs(title = title1) +
  theme_light()

# display the results
cat('MSE : ', round(min(param$mse), 2),
    '\namp_opt: ', param$amp_lim[which.min(param$mse)],
    '\ntime_opt: ', param$time_lim[which.min(param$mse)],
    '\nnumber of breaks mean : ', round(mean(actual$nb_bk), 2))
