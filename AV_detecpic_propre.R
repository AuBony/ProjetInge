
path <- '~/GitHub/ProjetInge/'

# PACKAGES ----

library(tuneR)
require(readr)  # for read_csv()
require(dplyr)  # for mutate()
require(tidyr)  # for unnest()
require(purrr)  # for map(), reduce()
library(stringr) # for str_replace()
library(ggplot2)
library(plotly)

# FUNCTIONS ----

frame_cut <- function(path, file, window_length, overlap){
  # INPUTS
  # path : character, path to the file of wav
  # file : character, name of the file
  # window_length : numeric
  # overlap : numeric, percentage of the frame taken again in the next frame
  # OUTPUT
  # ech : data frame, table with filename | start | end
  n <- length(readWave(paste0(path, file)))
  dec <- window_length * (1 - overlap)
  ech <- data.frame(filename = file, start = 0, end = window_length)
  a <- 1
  st <- round(dec, 2)
  end <- st + window_length
  while(st < (n - window_length*44100) / 44100){
    a <- a + 1
    ech[a,] <- c(file, st, end)
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
  # data : dataframe with filename, start, end -> recordings already cut in frames
  # OUTPUT
  # tibble dataframe with filename, start, end and features
  
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

frame_pts <- function(audio, step, pred){
  print(audio)
  df1 <- pred %>% filter(filename == audio)
  n <- df1$end[nrow(df1)] * 44100
  pts <- seq(0, n, by = step)
  df <- data.frame(pts = pts, 
                   time = pts/44100, 
                   prob_0 = rep(0,length(pts)),
                   prob_1 = rep(0,length(pts)))
  a <- 1
  for (k in df$time){
    window_index <- which(k >= df1$start & k <= df1$end)
    df$prob_0[a] <- round(max(df1$no_event[window_index]), 3)
    df$prob_1[a] <- round(max(df1$event[window_index]), 3)
    a <- a+1
  }
  return(df)
}

imp_norm <- function(file_name, path_wav){
  # INPUTS : 
  # file_name : character, name of the wave file to import and normalize
  # path_wav : character, path to the file
  # OUTPUT : 
  # wave file imported and normalized
  return(normalize(readWave(paste0(path_wav, file_name)), center = TRUE))
}

count_peaks <- function(wav, amp_lim, diff_lim){
  # INPUTS:
  # wav: Wave, an element of wavlist (a recording as wave)
  # amp_lim : numeric, amplitude limit of detection
  # diff_lim : numeric, minimal distance between 2 peaks
  # OUTPUT:
  # c : numeric, count of peaks in the wav
  c <- 1
  vec <- wav@left
  vecsel <- which(abs(vec) > amp_lim)
  if (length(vecsel) == 0){
    c <- 0
  } else if (length(vecsel) > 1) {
    for (k in 2:length(vecsel)){
      if (abs(vecsel[k]-vecsel[k-1]) > diff_lim){
        c <- c + 1
      }
    }}
  return(c)
}

count_peaks_m <- function(left_m, amp_lim, diff_lim){
  # INPUTS:
  # left_m : vector, wav@left of multiple elements of wavlist (a recording as wave)
  # amp_lim : numeric, amplitude limit of detection
  # diff_lim : numeric, minimal distance between 2 peaks
  # OUTPUT:
  # c : numeric, count of peaks in the wav
  c <- 1
  vecsel <- which(abs(left_m) > amp_lim)
  if (length(vecsel) == 0){
    c <- 0
  } else if (length(vecsel) > 1) {
    for (k in 2:length(vecsel)){
      if (abs(vecsel[k]-vecsel[k-1]) > diff_lim){
        c <- c + 1
      }
    }}
  return(c)
}

count_peaks_m0 <- function(left_m, amp_lim, diff_lim){ # FALSE
  # INPUTS:
  # left_m: vector, wav@left of multiple elements of wavlist
  # amp_lim : numeric, amplitude limit of detection
  # diff_lim : numeric, minimal distance between 2 peaks
  # OUTPUT:
  # c : numeric, count of peaks in the wav
  c <- 1
  vecselamp <- which(abs(vec) > amp_lim)
  vecselprob <- pred_rf$pts[which(pred_rf$prob_1 > prob_lim)]
  vecsel <- union(vecselamp, vecselprob)
  if (length(vecsel) == 0){
    c <- 0
  } else if (length(vecsel) > 1) {
    for (k in 2:length(vecsel)){
      if (abs(vecsel[k]-vecsel[k-1]) > diff_lim){
        c <- c + 1
      }
    }}
  return(c)
}

count_peaks_ML <- function(diff_lim, prob_lim, pred_rf){
  # INPUTS:
  # diff_lim : numeric, minimal distance between 2 peaks
  # prob_lim : numeric, minimal probability to define an event
  # pred_rf : dataframe, pts | time | prob_0 | prob_1 predicted by random forest
  #           for the corresponding wav
  # OUTPUT:
  # c : numeric, count of peaks in the wav
  vecsel <- pred_rf$pts[which(pred_rf$prob_1 > prob_lim)]
  if (length(vecsel) == 0){
    c <- 0
  } else if (length(vecsel) == 1) {
    c <- 1
  } else {
    c <- 1
    for (k in 2:length(vecsel)){
      if (abs(vecsel[k]-vecsel[k-1]) > diff_lim){
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

# WITH ALL DATA, ONLY MANUAL PREDICTION ----

# Functions
imp_norm <- function(file_name, path_wav){
  # INPUTS : 
  # file_name : character, name of the wave file to import and normalize
  # path_wav : character, path to the file
  # OUTPUT : 
  # wave file imported and normalized
  return(normalize(readWave(paste0(path_wav, file_name)), center = TRUE))
}

count_peaks <- function(wav, amp_lim, diff_lim){
  # INPUTS:
  # wav: Wave, an element of wavlist (a recording as wave)
  # amp_lim : numeric, amplitude limit of detection
  # diff_lim : numeric, minimal distance between 2 peaks
  # OUTPUT:
  # c : numeric, count of peaks in the wav
  c <- 1
  vec <- wav@left
  vecsel <- which(abs(vec) > amp_lim)
  if (length(vecsel) == 0){
    c <- 0
  } else if (length(vecsel) > 1) {
    for (k in 2:length(vecsel)){
      if (abs(vecsel[k]-vecsel[k-1]) > diff_lim){
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

# Data
fich <- list.files(paste0(path,'wav'))

actual <- read.table(paste0(path, 'nb_bk_withnobreak.csv'), 
                     sep = ';', dec = '.', header = TRUE)
actual$filename <- as.factor(actual$filename)

wavlist <- lapply(fich, FUN = imp_norm, path_wav = paste0(path,'wav/'))

# finding optimal couple

param <- expand.grid(amp_lim = seq(0.1, 1, by = 0.1), 
                     diff_lim = seq(1000, 50000, by = 1000))
param$mse <- rep(0, nrow(param))
for (i in 1:nrow(param)){
  count <- c()
  print(paste('amp: ',param$amp_lim[i], 'diff: ', param$diff_lim[i]))
  for (k in 1:nrow(actual)){
    count <- c(count, count_peaks(wav = wavlist[[k]],
                                  amp_lim = param$amp_lim[i],
                                  diff_lim = param$diff_lim[i]))
  }
  param$mse[i] <- mse(count, actual$nb_bk)
}
p <- ggplot(param, aes(x = amp_lim, y = diff_lim, fill = log(mse))) +
  geom_tile() +
  scale_fill_gradient(low = 'springgreen3', high = 'blue') +
  scale_x_continuous(breaks = seq(0.1, 1, by = 0.1)) +
  scale_y_continuous(breaks = seq(1000, 50000, by = 2000)) +
  theme_minimal()
ggplotly(p)
min(param$mse)
amp_opt <- param$amp_lim[which.min(param$mse)]
diff_opt <- param$diff_lim[which.min(param$mse)]

# with optimal couple

diff_lim <- diff_opt
amp_lim <- amp_opt
count5 <- c()
for (k in 1:nrow(actual)){
  print(k)
  count5 <- c(count5, count_peaks(wav = wavlist[[k]],
                                  amp_lim = amp_lim,
                                  diff_lim = diff_lim))
}
count5
mse(count5, actual$nb_bk)
length(which(count5 == actual$nb_bk))

df5 <- data.frame(actual = actual$nb_bk, prediction = count5)
df5 <- df5 %>% add_count(actual, prediction)
m <- max(actual$nb_bk)+1

title5 <- paste0('Number of breaks, predicted vs actual \n for amp_lim = ', amp_lim,
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

# WITH ALL DATA, ONLY ML PREDICTION (ML 2nd lab) ----

# Fonctions

frame_pts <- function(audio, step, pred){
  print(audio)
  df1 <- pred %>% filter(filename == audio)
  n <- df1$end[nrow(df1)] * 44100
  pts <- seq(0, n, by = step)
  df <- data.frame(pts = pts, 
                   time = pts/44100, 
                   prob_0 = rep(0,length(pts)),
                   prob_1 = rep(0,length(pts)))
  a <- 1
  for (k in df$time){
    window_index <- which(k >= df1$start & k <= df1$end)
    df$prob_0[a] <- round(max(df1$no_event[window_index]), 3)
    df$prob_1[a] <- round(max(df1$event[window_index]), 3)
    a <- a+1
  }
  return(df)
}

imp_norm <- function(file_name, path_wav){
  # INPUTS : 
  # file_name : character, name of the wave file to import and normalize
  # path_wav : character, path to the file
  # OUTPUT : 
  # wave file imported and normalized
  return(normalize(readWave(paste0(path_wav, file_name)), center = TRUE))
}

count_peaks_ML <- function(diff_lim, prob_lim, pred_rf){
  # INPUTS:
  # diff_lim : numeric, minimal distance between 2 peaks
  # prob_lim : numeric, minimal probability to define an event
  # pred_rf : dataframe, pts | time | prob_0 | prob_1 predicted by random forest
  #           for the corresponding wav
  # OUTPUT:
  # c : numeric, count of peaks in the wav
  vecsel <- pred_rf$pts[which(pred_rf$prob_1 > prob_lim)]
  if (length(vecsel) == 0){
    c <- 0
  } else if (length(vecsel) == 1) {
    c <- 1
  } else {
    c <- 1
    for (k in 2:length(vecsel)){
      if (abs(vecsel[k]-vecsel[k-1]) > diff_lim){
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

# Data

fich <- list.files(paste0(path,'wav'))
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


# WITH MIXED DATA, ONLY MANUAL PREDICTION ----

# functions

imp_norm <- function(file_name, path_wav){
  # INPUTS : 
  # file_name : character, name of the wave file to import and normalize
  # path_wav : character, path to the file
  # OUTPUT : 
  # wave file imported and normalized
  return(normalize(readWave(paste0(path_wav, file_name)), center = TRUE))
}

count_peaks_m <- function(left_m, amp_lim, diff_lim){
  # INPUTS:
  # left_m : vector, wav@left of multiple elements of wavlist (a recording as wave)
  # amp_lim : numeric, amplitude limit of detection
  # diff_lim : numeric, minimal distance between 2 peaks
  # OUTPUT:
  # c : numeric, count of peaks in the wav
  c <- 1
  vecsel <- which(abs(left_m) > amp_lim)
  if (length(vecsel) == 0){
    c <- 0
  } else if (length(vecsel) > 1) {
    for (k in 2:length(vecsel)){
      if (abs(vecsel[k]-vecsel[k-1]) > diff_lim){
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

# data

fich <- list.files(paste0(path,'wav'))

actual <- read.table(paste0(path, 'nb_bk_withnobreak.csv'), 
                     sep = ';', dec = '.', header = TRUE)
actual$filename <- as.factor(actual$filename)

wavlist <- lapply(fich, FUN = imp_norm, path_wav = paste0(path,'wav/'))

# building the new dataset 

nb_samples <- 100 # number of samples in the new dataset
max_samples <- 5 # maximum number of recordings computed together
actual_rand <- c()
left <- list()
for (k in 1:nb_samples){
  nb <- sample(1:max_samples, 1)
  lines <- sample(1:length(wavlist), nb, replace = TRUE)
  l <- c()
  for (j in lines){
    l <- c(l, wavlist[[j]]@left)
  }
  left[[k]] <- l
  actual_rand <- c(actual_rand, sum(actual$nb_bk[lines]))
}

# finding optimal couple

param <- expand.grid(amp_lim = seq(0.1, 1, by = 0.1), 
                     diff_lim = seq(20000, 50000, by = 1000))
param$mse <- rep(0, nrow(param))
for (i in 1:nrow(param)){
  count <- c()
  print(paste('amp: ',param$amp_lim[i], 'diff: ', param$diff_lim[i]))
  for (k in 1:nb_samples){
    count <- c(count, count_peaks_m(left_m = left[[k]],
                                    amp_lim = param$amp_lim[i],
                                    diff_lim = param$diff_lim[i]))
  }
  param$mse[i] <- mse(count, actual_rand)
}
p <- ggplot(param, aes(x = amp_lim, y = diff_lim, fill = log(mse))) +
  geom_tile() +
  scale_fill_gradient(low = 'springgreen3', high = 'blue') +
  scale_x_continuous(breaks = seq(0.1, 1, by = 0.1)) +
  scale_y_continuous(breaks = seq(20000, 50000, by = 2000)) +
  theme_minimal()
ggplotly(p)
min(param$mse)
amp_opt <- param$amp_lim[which.min(param$mse)]
diff_opt <- param$diff_lim[which.min(param$mse)]

# using optimal parameters

diff_lim <- diff_opt
amp_lim <- amp_opt
count5 <- c()
for (k in 1:nb_samples){
  print(k)
  count5 <- c(count5, count_peaks_m(left_m = left[[k]], 
                                    diff_lim = diff_lim, 
                                    amp_lim = amp_opt))
}
count5
mse(count5, actual_rand)
length(which(count5 == actual_rand))

df5 <- data.frame(actual = actual_rand, prediction = count5)
df5 <- df5 %>% add_count(actual, prediction)
m <- max(actual_rand)+1

title5 <- paste0('Number of breaks, predicted vs actual \n for amp_lim = ', amp_lim,
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

# WITH MIXED DATA, ONLY ML PREDICTION ----

# Fonctions

imp_norm <- function(file_name, path_wav){
  # INPUTS : 
  # file_name : character, name of the wave file to import and normalize
  # path_wav : character, path to the file
  # OUTPUT : 
  # wave file imported and normalized
  return(normalize(readWave(paste0(path_wav, file_name)), center = TRUE))
}

frame_pts <- function(audio, step, pred){
  print(audio)
  df1 <- pred %>% filter(filename == audio)
  n <- df1$end[nrow(df1)] * 44100
  pts <- seq(0, n, by = step)
  df <- data.frame(pts = pts, 
                   time = pts/44100, 
                   prob_0 = rep(0,length(pts)),
                   prob_1 = rep(0,length(pts)))
  a <- 1
  for (k in df$time){
    window_index <- which(k >= df1$start & k <= df1$end)
    df$prob_0[a] <- round(max(df1$no_event[window_index]), 3)
    df$prob_1[a] <- round(max(df1$event[window_index]), 3)
    a <- a+1
  }
  return(df)
}

count_peaks_ML_m <- function(diff_lim, prob_lim, pred_rf){
  # INPUTS:
  # diff_lim : numeric, minimal distance between 2 peaks
  # prob_lim : numeric, minimal probability to define an event
  # pred_rf : dataframe, pts | time | prob_0 | prob_1 predicted by random forest
  #           for the corresponding wav
  # OUTPUT:
  # c : numeric, count of peaks in the wav
  vecsel <- pred_rf$pts[which(pred_rf$prob_1 > prob_lim)]
  if (length(vecsel) == 0){
    c <- 0
  } else if (length(vecsel) == 1) {
    c <- 1
  } else {
    c <- 1
    for (k in 2:length(vecsel)){
      if (abs(vecsel[k]-vecsel[k-1]) > diff_lim){
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

# Data

fich <- list.files(paste0(path,'wav'))
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

# building the new dataset 

nb_samples <- 100 # number of samples in the new dataset
max_samples <- 5 # maximum number of recordings computed together
actual_rand <- c()
pr <- list()
for (k in 1:nb_samples){
  nb <- sample(1:max_samples, 1)
  print(nb)
  lines <- sample(1:nrow(actual), nb, replace = TRUE)
  p <- data.frame(pts = pred_pts[[lines[1]]]$pts, 
                  time = pred_pts[[lines[1]]]$time,
                  prob_0 = pred_pts[[lines[1]]]$prob_0, 
                  prob_1 = pred_pts[[lines[1]]]$prob_1)
  for (j in lines[2:nb]){
    points <- pred_pts[[j]]$pts + p$pts[nrow(p)] + 20
    p_eff <- data.frame(pts = points,
                        time = points / 44100,
                        prob_0 = pred_pts[[j]]$prob_0,
                        prob_1 = pred_pts[[j]]$prob_1)
    p <- rbind(p, p_eff)
  }
  pr[[k]] <- p
  actual_rand <- c(actual_rand, sum(actual$nb_bk[lines]))
}

# finding optimal couple

param <- expand.grid(prob_lim = seq(0.1, 1, by = 0.1), 
                     diff_lim = seq(1000, 50000, by = 1000))
param$mse <- rep(0, nrow(param))
for (i in 1:nrow(param)){
  count <- c()
  print(paste('prob: ',param$prob_lim[i], 'diff: ', param$diff_lim[i]))
  for (k in 1:nb_samples){
    count <- c(count, count_peaks_ML_m(prob_lim = param$prob_lim[i],
                                       diff_lim = param$diff_lim[i],
                                       pred_rf = pr[[k]]))
  }
  param$mse[i] <- mse(count, actual_rand)
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
for (k in 1:nb_samples){
  print(k)
  count5 <- c(count5, count_peaks_ML_m(prob_lim = prob_lim,
                                       diff_lim = diff_lim,
                                       pred_rf = pr[[k]]))
}
count5
mse(count5, actual_rand)
length(which(count5 == actual_rand))

df5 <- data.frame(actual = actual_rand, prediction = count5)
df5 <- df5 %>% add_count(actual, prediction)
m <- max(actual_rand)+1

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

# plot of prob_1 compared to amplitude for the wav number a

a <- 3
title4 <- paste0('Probability for a point to belong to an event\ndepending on the time for recording ', fich[a])
pred_pts[[a]] %>% ggplot(aes(x = time, y = prob_1)) +
  geom_line() +
  xlab('time (s)') +
  ylab('probability to be a break') +
  theme(plot.title = element_text()) +
  labs(title = title4) +
  theme_minimal()

plot(wavlist[[a]])
dev.off()
