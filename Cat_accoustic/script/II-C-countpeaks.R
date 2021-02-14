###########################################################################
## PROJECT : CAT ACCOUSTIC
## TITLE : II-C-countpeaks
## AUTHOR : BONY & de CROUTTE (AGROCAMPUS OUEST)
## DATE : DECEMBER 2020 TO FEBRUARY 2021
###########################################################################

# PACKAGES ----

library(tuneR)
library(seewave)
library(dplyr)
library(ggplot2)
library(plotly)
library(plyr)
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

frame_cut <- function(path, file_name, window_length, overlap){
  # INPUTS
  # path : character, path to the file of wav
  # file_name : character, name of the file
  # window_length : numeric
  # overlap : numeric, percentage of the frame taken again in the next frame
  # OUTPUT
  # ech : data frame, table with filename | start | end
  n <- length(readWave(paste0(path, file_name)))
  dec <- window_length * (1 - overlap)
  ech <- data.frame(filename = file_name, start = 0, end = window_length)
  a <- 1
  st <- round(dec, 2)
  end <- st + window_length
  while(st < (n - window_length*44100) / 44100){
    a <- a + 1
    ech[a,] <- c(file_name, st, end)
    st <- round(st + dec, 2)
    end <- st + window_length
  }
  ech$start <- as.numeric(ech$start)
  ech$end <- as.numeric(ech$end)
  ech$filename <- as.factor(ech$filename)
  return(ech)
}

calc_features <- function(data, path){
  # INPUT
  # data : dataframe with filename | start | end -> recordings already cut in frames
  # OUTPUT
  # tibble dataframe with filename | start | end | features
  
  # df initialisation (empty)
  df_feature <- tibble(filename = character(),
                       start = numeric(),
                       end = numeric(),
                       
                       th = numeric(),
                       maxdfreq =  numeric(),
                       meandfreq =  numeric(),
                       
                       smean =  numeric(),
                       ssd =  numeric(),
                       ssem =  numeric(),
                       smedian =  numeric(),
                       smode =  numeric(),
                       sQ25 =  numeric(),
                       sQ75 =  numeric(),
                       sIQR =  numeric(),
                       scent =  numeric(),
                       sskewness =  numeric(),
                       skurtosis =  numeric(),
                       ssfm =  numeric(),
                       ssh =  numeric())
  
  # for each different audio 
  for (audio in unique(data$filename)){
    
    print(audio)
    tab <- data %>% filter(filename == audio)
    
    # moment is the start of each frame
    for (moment in 1:nrow(tab)){
      
      # charge the wav in the frame
      wav_file <- readWave(paste0(path, audio),
                           from = tab$start[moment],
                           to = tab$end[moment],
                           units = "seconds")
      
      # features of the frame
      sp <- seewave::specprop(seewave::spec(wav_file@left, 
                                            f = wav_file@samp.rate, 
                                            plot = FALSE, scaled = TRUE, 
                                            norm = FALSE))
      
      df_feature <- df_feature %>% add_row(
        filename = audio,
        start = tab$start[moment],
        end = tab$end[moment],
        
        th = seewave::th(env(wav_file, plot = FALSE)),
        maxdfreq = max(dfreq(wav_file, plot = FALSE)[,2]),
        meandfreq = mean(dfreq(wav_file, plot = FALSE)[,2]),
        
        smean = sp$mean,
        ssd = sp$sd,
        ssem = sp$sem,
        smedian = sp$median,
        smode = sp$mode,
        sQ25 = sp$Q25,
        sQ75 = sp$Q75,
        sIQR = sp$IQR,
        scent = sp$cent,
        sskewness = sp$skewness,
        skurtosis = sp$kurtosis,
        ssfm = sp$sfm,
        ssh = sp$sh)
      
    }
  } 
  return(df_feature)
}

frame_pts <- function(file_name, step, pred){
  # INPUTS:
  # audio : character, name of the treated wav file
  # step : integer, step used for the transformation frame->points for the predictions
  # pred : dataframe, prediction probabilities per frame, 
  # filename | start | end | no_event | event
  # OUTPUTS:
  # df : dataframe, pts | time | prob_no_event | prob_event, pts is the point index,
  # time is the corresponding time in seconds
  print(file_name)
  df1 <- pred %>% filter(filename == file_name)
  n <- df1$end[nrow(df1)] * 44100
  pts <- seq(0, n, by = step)
  df <- data.frame(pts = pts, 
                   time = pts/44100, 
                   prob_no_event = rep(0,length(pts)),
                   prob_event = rep(0,length(pts)))
  a <- 1
  for (k in df$time){
    window_index <- which(k >= df1$start & k <= df1$end)
    df$prob_no_event[a] <- round(mean(df1$no_event[window_index]), 3)
    df$prob_event[a] <- round(mean(df1$event[window_index]), 3)
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
  c <- 1
  vecsel <- pred_rf$pts[which(pred_rf$prob_event > prob_lim)]
  if (length(vecsel) == 0){
    c <- 0
  } else if (length(vecsel) > 1) {
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


# II-C-1) METHOD ON AMPLITUDE CURVES WITH ORIGINAL RECORDINGS ----


### Data importation

# names of the files
fich <- list.files('data/wav')

# weal labels for the 79 samples
actual <- read.table('data/nb_bk_79samples.csv', sep = ';', dec = '.', header = TRUE)
actual$filename <- as.factor(actual$filename)

# import left channel of wav files in a list (79 wav = 79 elements of the list)
wavleft <- lapply(fich, FUN = imp_norm, path_wav = 'data/wav/')


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


# II-C-2) METHOD ON PROBABILITY CURVES WITH ORIGINAL RECORDINGS ----

### Data importation

# names of the files
fich <- list.files('data/wav')

# uncomment these next 9 lines to recalculate the features, if you want to change the
# window length or overlap for instance

# # cut initial recordings into frames (choose the frames according to the model you
# # will use afterwards)
# dta <- lapply(fich, FUN = frame_cut, path = 'data/wav/',
#               window_length = 0.02, overlap = 0.5)
# data <- ldply(dta, rbind)
# 
# # calculate features for each frame
# df_feature <- calc_features(data, path = 'data/wav/')
# write.table(df_feature, 'data/features/IIC3-df_features-0.02-0.5.csv')

# import already calculated features
df_feature <- read.table('data/features/IIC3-df_features-0.02-0.5.csv',
                         sep = ' ', dec = '.', header = TRUE)

# dataframe with file names and number of breaks
actual <- read.table('data/nb_bk_79samples.csv', sep = ';', dec = '.', header = TRUE)
actual$filename <- as.factor(actual$filename)
summary(actual)


### Prediction with the random forest model

# import the model (choose the model according to the frame cut you did)
model <- readRDS("data/RFmodels/model_label_2.rds")

# predict probability of being an event and not being an event per frame
pred <- predict(model, newdata = df_feature, type = 'prob')
pred <- data.frame(filename = df_feature$filename,
                   start = df_feature$start,
                   end = df_feature$end,
                   no_event = pred[,1],
                   event = pred[,2])

# transform these probabilities per frame to probabilities per point (with a step)
pred_pts <- lapply(fich, FUN = frame_pts, step = 20, pred = pred) 


### Find the optimal couple of parameters (2 thresholds : probability of being an 
### event (prob_lim) and time (time_lim))

# put the parameters to test in a dataframe, with a  3rd column MSE
param <- expand.grid(prob_lim = seq(0.1, 1, by = 0.1), 
                     time_lim = seq(1000, 60000, by = 2000))
param$mse <- rep(0, nrow(param))

# calculate the MSE for each couple of parameters
for (i in 1:nrow(param)){
  count2 <- c()
  print(paste('prob: ',param$prob_lim[i], 'time: ', param$time_lim[i]))
  for (k in 1:nrow(actual)){
    count2 <- c(count2, count_peaks_ML(pred_rf = pred_pts[[k]],
                                       prob_lim = param$prob_lim[i],
                                       time_lim = param$time_lim[i]))
  }
  param$mse[i] <- mse(count2, actual$nb_bk)
}

# plot the MSE depending on the parameters values as a heatmap
p <- ggplot(param, aes(x = prob_lim, y = time_lim, fill = log(mse))) +
  geom_tile() +
  scale_fill_gradient(low = 'springgreen3', high = 'blue') +
  scale_x_continuous(breaks = seq(0.1, 1, by = 0.1)) +
  scale_y_continuous(breaks = seq(1000, 60000, by = 2000)) +
  xlab('prob_lim') +
  ylab('time_lim (pts)') +
  theme_minimal()
ggplotly(p)


### Predict with the optimal couple of parameters

# fix the parameters
prob_lim <- param$prob_lim[which.min(param$mse)]
time_lim <- param$time_lim[which.min(param$mse)]

# count the peaks with the 2 optimal thresholds
count3 <- c()
for (k in 1:nrow(actual)){
  print(k)
  count3 <- c(count3, count_peaks_ML(pred_rf = pred_pts[[k]],
                                     prob_lim = prob_lim,
                                     time_lim = time_lim))
}
count3
mse(count3, actual$nb_bk)
length(which(count3 == actual$nb_bk))

# draw the plot of prediction vs actual
df3 <- data.frame(actual = actual$nb_bk, prediction = count3)
df3 <- df3 %>% add_count(actual, prediction)
m <- max(actual$nb_bk)+1
title3 <- paste0('Number of breaks, predicted vs actual \n for prob_lim = ', prob_lim,
                 ', time_lim = ', time_lim, ' (', round(time_lim/44100, 2), ' sec)')
ggplot(df3, aes(x = actual, y = prediction)) +
  stat_density2d(geom='tile', aes(fill=..density..), contour = FALSE) +
  geom_segment(aes(x = -1, y = -1, xend = m, yend = m), 
               colour = 'lightgrey', alpha = 0.5) +
  geom_point(colour='lightgrey', size = 1) +
  scale_x_continuous(breaks = seq(-1, m, by = 1)) +
  scale_y_continuous(breaks = seq(-1, m, by = 1)) +
  theme(plot.title = element_text()) +
  labs(title = title3) +
  theme_light()

# display the results
cat('MSE : ', round(min(param$mse), 2),
    '\nprob_opt: ', param$prob_lim[which.min(param$mse)],
    '\ntime_opt: ', param$time_lim[which.min(param$mse)],
    '\nnumber of breaks mean : ', round(mean(actual$nb_bk), 2))

# II-C-3) METHODS ON AMPLITUDE AND PROBABILITY CURVES WITH 100 MIX OF RECORDINGS ----

### Data importation

# names of the files
fich <- list.files('data/wav')

# uncomment these next 9 lines to recalculate the features, if you want to change the
# window length or overlap for instance

# # cut initial recordings into frames (choose the frames according to the model you
# # will use afterwards)
# dta <- lapply(fich, FUN = frame_cut, path = 'data/wav/',
#               window_length = 0.02, overlap = 0.5)
# data <- ldply(dta, rbind)
# 
# # calculate features for each frame
# df_feature <- calc_features(data, path = 'data/wav/')
# write.table(df_feature, 'data/features/IIC3-df_features-0.02-0.5.csv')

# import already calculated features
df_feature <- read.table('data/features/IIC3-df_features-0.02-0.5.csv',
                         sep = ' ', dec = '.', header = TRUE)

# dataframe with file names and number of breaks
actual <- read.table('data/nb_bk_79samples.csv', sep = ';', dec = '.', header = TRUE)
actual$filename <- as.factor(actual$filename)
summary(actual)

# import left channel of wav files in a list (79 wav = 79 elements of the list)
wavleft <- lapply(fich, FUN = imp_norm, path_wav = 'data/wav/')


### Prediction with the random forest model

# import the model (choose the model according to the frame cut you did)
model <- readRDS("data/RFmodels/model_label_2.rds")

# predict probability of being an event and not being an event per frame
pred <- predict(model, newdata = df_feature, type = 'prob')
pred <- data.frame(filename = df_feature$filename,
                   start = df_feature$start,
                   end = df_feature$end,
                   no_event = pred[,1],
                   event = pred[,2])

# transform these probabilities per frame to probabilities per point (with a step)
pred_pts <- lapply(fich, FUN = frame_pts, step = 20, pred = pred) 


### Building the new dataset 

# fix the parameters
nb_samples <- 100 # number of samples in the new dataset
max_samples <- 5 # maximum number of recordings computed together

# create the dataset
actual_rand <- c()
pr <- list() # initialise a list of dataframes pred_pts for each new mixed sample
left_l <- list() # initialise a list of vectors of left channel for each new sample
for (k in 1:nb_samples){
  nb <- sample(1:max_samples, 1)
  lines <- sample(1:nrow(actual), nb, replace = TRUE)
  p <- data.frame(pts = pred_pts[[lines[1]]]$pts, 
                  time = pred_pts[[lines[1]]]$time,
                  prob_no_event = pred_pts[[lines[1]]]$prob_no_event, 
                  prob_event = pred_pts[[lines[1]]]$prob_event)
  l <- c(wavleft[[lines[1]]])
  if (nb > 1){
    for (j in lines[2:nb]){
      points <- pred_pts[[j]]$pts + p$pts[nrow(p)] + 20 # time needs to change
      p_eff <- data.frame(pts = points,
                          time = points / 44100,
                          prob_no_event = pred_pts[[j]]$prob_no_event,
                          prob_event = pred_pts[[j]]$prob_event)
      p <- rbind(p, p_eff)
      l <- c(l, wavleft[[j]])
    }
  }
  pr[[k]] <- p
  left_l[[k]] <- l
  actual_rand <- c(actual_rand, sum(actual$nb_bk[lines]))
}

### Find the optimal couple of parameters for amplitude method (2 thresholds : 
### amplitude (amp_lim, dB) and time (time_lim, pts))

# put the parameters to test in a dataframe, with a  3rd column MSE
param <- expand.grid(amp_lim = seq(0.1, 1, by = 0.1), 
                     time_lim = seq(1000, 60000, by = 2000))
param$mse <- rep(0, nrow(param))

# calculate the MSE for each couple of parameters
for (i in 1:nrow(param)){
  count4 <- c()
  for (k in 1:nb_samples){
    count4 <- c(count4, count_peaks_amp(wav_left = left_l[[k]],
                                       amp_lim = param$amp_lim[i],
                                       time_lim = param$time_lim[i]))
  }
  param$mse[i] <- mse(count4, actual_rand)
}

# plot the MSE depending on the parameters values as a heatmap
p <- ggplot(param, aes(x = amp_lim, y = time_lim, fill = log(mse))) +
  geom_tile() +
  scale_fill_gradient(low = 'springgreen3', high = 'blue') +
  scale_x_continuous(breaks = seq(0.1, 1, by = 0.1)) +
  scale_y_continuous(breaks = seq(1000, 60000, by = 2000)) +
  theme_minimal()
ggplotly(p)


### Find the optimal couple of parameters for ML method (2 thresholds : probability 
### of being an event (prob_lim) and time (time_lim))

# put the parameters to test in a dataframe, with a  3rd column MSE
param2 <- expand.grid(prob_lim = seq(0.1, 1, by = 0.1), 
                     time_lim = seq(1000, 60000, by = 2000))
param2$mse <- rep(0, nrow(param2))

# calculate the MSE for each couple of parameters
for (i in 1:nrow(param2)){
  count5 <- c()
  for (k in 1:nb_samples){
    count5 <- c(count5, count_peaks_ML(pred_rf = pr[[k]],
                                       prob_lim = param2$prob_lim[i],
                                       time_lim = param2$time_lim[i]))
  }
  param2$mse[i] <- mse(count5, actual_rand)
}

# plot the MSE depending on the parameters values as a heatmap
p <- ggplot(param2, aes(x = prob_lim, y = time_lim, fill = log(mse))) +
  geom_tile() +
  scale_fill_gradient(low = 'springgreen3', high = 'blue') +
  scale_x_continuous(breaks = seq(0.1, 1, by = 0.1)) +
  scale_y_continuous(breaks = seq(1000, 60000, by = 2000)) +
  theme_minimal()
ggplotly(p)


### Display the results
cat('mse : ', min(param$mse),
    '\namp_opt: ', param$amp_lim[which.min(param$mse)],
    '\ntime_opt: ', param$time_lim[which.min(param$mse)])
cat('mse : ', min(param2$mse),
    '\nprob_opt: ', param2$prob_lim[which.min(param2$mse)],
    '\ntime_opt: ', param2$time_lim[which.min(param2$mse)])


### Predict with the optimal couple of parameters

# fix the parameters
time_lim <- 49000
amp_lim <- 0.2
prob_lim <- 0.7

# count the peaks with the 2 optimal thresholds
count6 <- c()
count7 <- c()
for (k in 1:nb_samples){
  count6 <- c(count6, count_peaks_amp(wav_left = left_l[[k]], 
                                      amp_lim = amp_lim,
                                      time_lim = time_lim))
  count7 <- c(count7, count_peaks_ML(pred_rf = pr[[k]],
                                     prob_lim = prob_lim,
                                     time_lim = time_lim))
}
count6
mse(count6, actual_rand)
length(which(count6 == actual_rand))
count7
mse(count7, actual_rand)
length(which(count7 == actual_rand))

# draw the plot of prediction vs actual for amplitude
df6 <- data.frame(actual = actual_rand, prediction = count6)
df6 <- df6 %>% add_count(actual, prediction)
m <- max(actual_rand)+1
title6 <- paste0('Number of breaks, predicted vs actual \n for amp_lim = ', amp_lim,
                 ' (dB), time_lim = ', time_lim, ' (', round(time_lim/44100, 2), ' sec)')
ggplot(df6, aes(x = actual, y = prediction)) +
  stat_density2d(geom='tile', aes(fill=..density..), contour = FALSE) +
  geom_segment(aes(x = -1, y = -1, xend = m, yend = m), 
               colour = 'lightgrey', alpha = 0.5) +
  geom_point(colour='lightgrey', size = 1) +
  scale_x_continuous(breaks = seq(-1, m, by = 1)) +
  scale_y_continuous(breaks = seq(-1, m, by = 1)) +
  theme(plot.title = element_text()) +
  labs(title = title6) +
  theme_light()

# display the results
cat('MSE : ', round(mse(count6, actual_rand), 2),
    '\namp_lim: ', amp_lim,
    '\ntime_lim: ', time_lim,
    '\nnumber of breaks mean : ', round(mean(actual_rand), 2))

# draw the plot of prediction vs actual for amplitude
df7 <- data.frame(actual = actual_rand, prediction = count7)
df7 <- df7 %>% add_count(actual, prediction)
m <- max(actual_rand)+1
title7 <- paste0('Number of breaks, predicted vs actual \n for prob_lim = ', prob_lim,
                 ', time_lim = ', time_lim, ' (', round(time_lim/44100, 2), ' sec)')
ggplot(df7, aes(x = actual, y = prediction)) +
  stat_density2d(geom ='tile', aes(fill=..density..), contour = FALSE) +
  geom_segment(aes(x = -1, y = -1, xend = m, yend = m), 
               colour = 'lightgrey', alpha = 0.5) +
  geom_point(colour='lightgrey', size = 1) +
  scale_x_continuous(breaks = seq(-1, m, by = 1)) +
  scale_y_continuous(breaks = seq(-1, m, by = 1)) +
  theme(plot.title = element_text()) +
  labs(title = title7) +
  theme_light()

# display the results
cat('MSE : ', round(mse(count7, actual_rand), 2),
    '\nprob_lim: ', prob_lim,
    '\ntime_lim: ', time_lim,
    '\nnumber of breaks mean : ', round(mean(actual_rand), 2))


### Visualise the distribution of initial dataset vs fictive one

ggplot() +
  geom_histogram(mapping = aes(x = actual_rand), col = 'lightblue', 
                 fill = 'lightblue', alpha = 0.7, binwidth = 1) +
  geom_histogram(mapping = aes(x = actual$nb_bk), col = 'lightpink', 
                 fill = 'lightpink', alpha = 0.7, binwidth = 1) +
  annotate(geom = "text", x = 2.35, y = 33, label = "Initial recordings",
           color= 'lightpink2') +
  annotate(geom = "text", x = 6, y = 15, label = "Mixed recordings",
           color= 'lightblue3') +
  scale_x_continuous(breaks = seq(0, max(actual_rand)+1, by = 1)) +
  xlab('number of breaks') +
  theme_light() +
  theme(plot.title = element_text(),
        panel.grid.minor = element_blank()) +
  labs(title = 'Distribution of the number of breaks for the 2 datasets')


