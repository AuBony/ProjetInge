
setwd('~/GitHub/ProjetInge/')

# PACKAGES ----

library(tuneR)
library(seewave)
require(readr)  # for read_csv()
require(dplyr)  # for mutate()
require(tidyr)  # for unnest()
require(purrr)  # for map(), reduce()
library(stringr) # for str_replace()
library(ggplot2)
library(plotly)
library(vioplot)
library(randomForest)
library(plyr) # ldply

#---- FUNCTIONS ----

frame_cut <- function(path = 'cleanwav/', file, window_length, overlap){
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

calc_features <- function(data){
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
      wav_file <- readWave(paste0('cleanwav/', audio),
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
    df$prob_0[a] <- round(mean(df1$no_event[window_index]), 3)
    df$prob_1[a] <- round(mean(df1$event[window_index]), 3)
    a <- a+1
  }
  return(df)
}

imp_norm <- function(file_name, path = 'cleanwav/'){
  # INPUTS : 
  # file_name : character, name of the wave file to import and normalize
  # path : character, path to the file
  # OUTPUT : 
  # wave file imported and normalized
  return(normalize(readWave(paste0(path, file_name)), center = TRUE))
}

count_peaks2 <- function(wav, diff_lim, prob_lim, pred_rf){
  # INPUTS:
  # wav: Wave, an element of wavlist (a recording as wave)
  # diff_lim : numeric, minimal distance between 2 peaks
  # prob_lim : numeric, minimal probability to define an event
  # pred_rf : dataframe, pts | time | prob_0 | prob_1 predicted by random forest
  #           for the corresponding wav
  # OUTPUT:
  # c : numeric, count of peaks in the wav
  c <- 1
  vec <- wav@left
  vecsel <- pred_rf$pts[which(pred_rf$prob_1 > prob_lim)]
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

count_peaks3 <- function(wav, amp_lim, diff_lim, prob_lim, pred_rf){
  # INPUTS:
  # wav: Wave, an element of wavlist (a recording as wave)
  # amp_lim : numeric, amplitude limit of detection
  # diff_lim : numeric, minimal distance between 2 peaks
  # prob_lim : numeric, minimal probability to define an event
  # pred_rf : dataframe, pts | time | prob_0 | prob_1 predicted by random forest
  #           for the corresponding wav
  # OUTPUT:
  # c : numeric, count of peaks in the wav
  c <- 1
  vec <- wav@left
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

mse <- function(pred, act){
  # INPUTS
  # pred : vector of predictions
  # act : vector of actual values
  # OUPUT
  # MSE
  return(sum((pred - act)^2) / length(pred))
}

# DETECTION ON COMPLETE SIGNAL ----

# features calculation on complete samples (per frames)
fich <- list.files('cleanwav')
# dta <- lapply(fich, FUN = frame_cut, path = 'cleanwav/', 
#               window_length = 0.02, overlap = 0.5)
# data <- ldply(dta, rbind)
# df_feature <- calc_features(data)
# write.csv(df_feature, 
#           'C:/Users/HP/Documents/GitHub/ProjetInge/features/29_01_0.02-0.5.csv',
#           row.names = FALSE)
df_feature <- read.table('C:/Users/HP/Documents/GitHub/ProjetInge/features/29_01_0.02-0.5.csv',
                         sep = ',', dec = '.', header = TRUE)

# counting events on total samples
actual <- read.table('nb_bk.csv', sep = ';', dec = '.', header = TRUE)
actual$filename <- as.factor(actual$filename)
summary(actual)
wavlist <- lapply(fich, FUN = imp_norm)
diff_lim <- 8820 # 0.2 sec

# probabilies calculation of belonging to event or no-event class with RF model
model <- readRDS("~/GitHub/ProjetInge/model_29_01_1.rds")
pred <- predict(model, newdata = df_feature, type = 'prob')
pred <- data.frame(filename = df_feature$filename,
                   start = df_feature$start,
                   end = df_feature$end,
                   no_event = pred[,1],
                   event = pred[,2])

pred_pts <- lapply(fich, FUN = frame_pts, step = 20, pred = pred) 

# visualization
i <- 3
plot(wavlist[[i]],
     main = fich[i])
plot(pred_pts[[i]]$time, pred_pts[[i]]$prob_1, type = 'l',
     xlab = 'time', ylab = 'probabily of being an event',
     main = fich[i], ylim = c(0,1))
dev.off()

# FINDING AN OPTIMAL COUPLE (AMP_LIM, PROB_LIM) ----

param <- expand.grid(amp_lim = seq(0.1,1,by=0.1), prob_lim = seq(0.1,1,by = 0.1))
param$mse <- rep(0, nrow(param))
for (i in 1:nrow(param)){
  count <- c()
  print(paste('amp: ',param$amp_lim[i], 'prob: ', param$prob_lim[i]))
  for (k in 1:length(wavlist)){
    count <- c(count, count_peaks3(wav = wavlist[[k]], 
                                   diff_lim = diff_lim, 
                                   amp_lim = param$amp_lim[i],
                                   prob_lim = param$prob_lim[i],
                                   pred_rf = pred_pts[[k]]))
  }
  param$mse[i] <- mse(count, actual$nb_bk)
}
p <- ggplot(param, aes(x = amp_lim, y = prob_lim, fill = log(mse))) +
  geom_tile() +
  scale_fill_gradient(low = 'springgreen3', high = 'blue') +
  scale_x_continuous(breaks = seq(0.1, 1, by = 0.1)) +
  scale_y_continuous(breaks = seq(0.1, 1, by = 0.1)) +
  theme_minimal()
ggplotly(p)
min(param$mse)
amp_opt <- param$amp_lim[which.min(param$mse)]
prob_opt <- param$prob_lim[which.min(param$mse)]


# with optimal parameters
prob_lim <- prob_opt
amp_lim <- amp_opt
count4 <- c()
for (k in 1:length(wavlist)){
  print(k)
  count4 <- c(count4, count_peaks3(wav = wavlist[[k]], diff_lim = diff_lim, 
                                   amp_lim = amp_lim, prob_lim = prob_lim,
                                   pred_rf = pred_pts[[k]]))
}
count4
mse(count4, actual$nb_bk)
length(which(count4 == actual$nb_bk))

df4 <- data.frame(actual = actual$nb_bk, prediction = count4)
df4 <- df4 %>% add_count(actual, prediction)
m <- max(actual$nb_bk)+1

title4 <- paste0('Number of breaks, predicted vs actual \n for amp_lim = ', amp_lim,
                 ', diff_lim = ', diff_lim, ' and prob_lim = ', prob_lim)
ggplot(df4, aes(x = actual, y = prediction)) +
  stat_density2d(geom='tile', aes(fill=..density..), contour = FALSE) +
  geom_segment(aes(x = 0, y = 0, xend = m, yend = m), 
               colour = 'lightgrey', alpha = 0.5) +
  geom_point(colour='lightgrey', size = 1) +
  scale_x_continuous(breaks = seq(0, m, by = 1)) +
  scale_y_continuous(breaks = seq(0, m, by = 1)) +
  theme(plot.title = element_text()) +
  labs(title = title4) +
  theme_light()

