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
  return(normalize(readWave(paste0(path_wav, file_name)), center = TRUE)@left)
}

frame_pts <- function(file_name, step, pred){
  # INPUTS:
  # audio : character, name of the treated wav file
  # step : integer, step used for the transformation frame->points for the predictions
  # pred : dataframe, prediction probabilities per frame, 
  # filename | start | end | prob_no_event | prob_event
  # OUTPUTS:
  # df : dataframe, pts | time | prob_no_event | prob_event, pts is the point index,
  # time is the corresponding time in seconds
  print(file_name)
  df1 <- pred %>% filter(filename == audio)
  n <- df1$end[nrow(df1)] * 44100
  pts <- seq(0, n, by = step)
  df <- data.frame(pts = pts, 
                   time = pts/44100, 
                   prob_no_event = rep(0,length(pts)),
                   prob_event = rep(0,length(pts)))
  a <- 1
  for (k in df$time){
    window_index <- which(k >= df1$start & k <= df1$end)
    df$prob_no_event[a] <- round(max(df1$prob_no_event[window_index]), 3)
    df$prob_event[a] <- round(max(df1$prob_event[window_index]), 3)
    a <- a+1
  }
  return(df)
}

count_peaks_amp <- function(wav_left, amp_lim, time_lim){
  # INPUTS:
  # wav_left : vector, wav@left of the treated sample
  # amp_lim : numeric, amplitude limit of detection
  # time_lim : numeric, minimal distance between 2 peaks
  # OUTPUT:
  # c : numeric, count of peaks in the wav
  c <- 1
  vecsel <- which(abs(wav_left) > amp_lim)
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

count_peaks_ML <- function(pred_rf, prob_lim, time_lim){
  # INPUTS:
  # pred_rf : dataframe, pts | time | prob_no_event | prob_event,
  #           predicted by the ML for the corresponding wav
  # prob_lim : numeric, minimal probability to define an event
  # time_lim : numeric, minimal distance between 2 peaks
  # OUTPUT:
  # c : numeric, count of peaks in the wav
  vecsel <- pred_rf$pts[which(pred_rf$prob_event > prob_lim)]
  if (length(vecsel) == 0){
    c <- 0
  } else if (length(vecsel) == 1) {
    c <- 1
  } else {
    c <- 1
    for (k in 2:length(vecsel)){
      if (abs(vecsel[k]-vecsel[k-1]) > time_lim){
        c <- c + 1
      }
    }
  }
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
actual <- read.table('nb_bk_79samples.csv', sep = ';', dec = '.', header = TRUE)
actual$filename <- as.factor(actual$filename)

# wav files importation in a list (79 wav = 79 elements of the list)
wavleft <- lapply(fich, FUN = imp_norm, path_wav = 'wav/')


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
    count <- c(count, count_peaks_amp(wav_left = wavleft[[k]],
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
  xlab('amp_lim (dB)') +
  ylab('time_lim (pts)') +
  theme_minimal()
ggplotly(p)


### Predict with the optimal couple of parameters

# fix the parameters
amp_lim <- param$amp_lim[which.min(param$mse)]
time_lim <- param$time_lim[which.min(param$mse)]

# count the peaks with the 2 optimal thresholds
count1 <- c()
for (k in 1:nrow(actual)){
  count1 <- c(count1, count_peaks_amp(wav_left = wavleft[[k]],
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
                 ' dB, time_lim = ', time_lim, ' (', round(time_lim/44100, 2), ' sec)')
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


# II-C-3) METHOD ON PROBABILITY CURVES WITH ORIGINAL RECORDINGS ----

### Data importation

# names of the files
fich <- list.files(paste0(path,'wav'))

# # cut initial recordings into frames 
# dta <- lapply(fich, FUN = frame_cut, path = paste0(path,'wav/'),
#               window_length = 0.02, overlap = 0.5)
# data <- ldply(dta, rbind)
# df_feature <- calc_features(data, path = paste0(path,'wav/'))
df_feature1 <- read.table('C:/Users/HP/Documents/GitHub/ProjetInge/features/29_01_0.02-0.5.csv',
                          sep = ',', dec = '.', header = TRUE)
df_feature2 <- read.table('C:/Users/HP/Documents/GitHub/ProjetInge/features/29_01_0.02-0.5_nocrocs.csv',
                          sep = ',', dec = '.', header = TRUE)
df_feature <- rbind(df_feature1, df_feature2)

# dataframe with file names and number of breaks
actual <- read.table(paste0(path, 'nb_bk_withnobreak.csv'), 
                     sep = ';', dec = '.', header = TRUE)
actual$filename <- as.factor(actual$filename)
summary(actual)
# list of the wav files
wavlist <- lapply(fich, FUN = imp_norm, path_wav = paste0(path,'wav/'))

model <- readRDS("~/GitHub/ProjetInge/model_29_01_1.rds")
pred <- predict(model, newdata = df_feature, type = 'prob') # 17227 | 2
pred <- data.frame(filename = df_feature$filename,
                   start = df_feature$start,
                   end = df_feature$end,
                   no_event = pred[,1],
                   event = pred[,2])

pred_pts <- lapply(fich, FUN = frame_pts, step = 20, pred = pred) 

# finding optimal couple

param <- expand.grid(prob_lim = seq(0.1, 1, by = 0.1), 
                     diff_lim = seq(1000, 50000, by = 1000))
param$mse <- rep(0, nrow(param))
for (i in 1:nrow(param)){
  count <- c()
  print(paste('prob: ',param$prob_lim[i], 'diff: ', param$diff_lim[i]))
  for (k in 1:nrow(actual)){
    count <- c(count, count_peaks_ML(diff_lim = param$diff_lim[i], 
                                     prob_lim = param$prob_lim[i],
                                     pred_rf = pred_pts[[k]]))
  }
  param$mse[i] <- mse(count, actual$nb_bk)
}
p <- ggplot(param, aes(x = prob_lim, y = diff_lim, fill = log(mse))) +
  geom_tile() +
  scale_fill_gradient(low = 'springgreen3', high = 'blue') +
  scale_x_continuous(breaks = seq(0.1, 1, by = 0.1)) +
  scale_y_continuous(breaks = seq(1000, 50000, by = 2000)) +
  theme_minimal()
ggplotly(p)
min(param$mse)
prob_opt <- param$prob_lim[which.min(param$mse)]
diff_opt <- param$diff_lim[which.min(param$mse)]

# using optimal parameters

diff_lim <- diff_opt
prob_lim <- prob_opt
count5 <- c()
for (k in 1:nrow(actual)){
  print(k)
  count5 <- c(count5, count_peaks_ML(diff_lim = diff_lim, 
                                     prob_lim = prob_lim,
                                     pred_rf = pred_pts[[k]]))
}
count5
mse(count5, actual$nb_bk)
length(which(count5 == actual$nb_bk))

df5 <- data.frame(actual = actual$nb_bk, prediction = count5)
df5 <- df5 %>% add_count(actual, prediction)
m <- max(actual$nb_bk)+1

title5 <- paste0('Number of breaks, predicted vs actual \n for prob_lim = ', prob_lim,
                 ', diff_lim = ', diff_lim)
ggplot(df5, aes(x = actual, y = prediction)) +
  stat_density2d(geom='tile', aes(fill=..density..), contour = FALSE) +
  geom_segment(aes(x = -1, y = -1, xend = m, yend = m), 
               colour = 'lightgrey', alpha = 0.5) +
  geom_point(colour='lightgrey', size = 1) +
  scale_x_continuous(breaks = seq(-1, m, by = 1)) +
  scale_y_continuous(breaks = seq(-1, m, by = 1)) +
  theme(plot.title = element_text()) +
  labs(title = title5) +
  theme_light()