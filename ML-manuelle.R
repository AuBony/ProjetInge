
path <- '~/GitHub/ProjetInge/'

#---- PACKAGES ----

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

# prediction (apres fonction predict(modele, newdata = X, type = prob (ou =response)) 
# = df avec 2 colonnes: proba d'appartenir a classe 0, proba d'appartenir a classe 1
# ligne = dans l'ordre des intervals donnes dans X

#---- FUNCTIONS ----

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
    
    cat("_")
    print(audio)
    # moment is the start of each frame
    for (moment in 1:nrow(data %>% filter(filename == audio))){
      
      # charge the wav in the frame
      wav_file <- readWave(paste0(path, 'cleanwav/', audio),
                           from = data$start[moment],
                           to = data$end[moment],
                           units = "seconds")
      
      # features of the frame
      sp <- seewave::specprop(seewave::spec(wav_file@left, 
                                            f = wav_file@samp.rate, 
                                            plot = FALSE, scaled = TRUE, 
                                            norm = FALSE))
      
      df_feature <- df_feature %>% add_row(
        filename = audio,
        start = data$start[moment],
        end = data$end[moment],
        
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

count_peaks2 <- function(wav, amp_lim, diff_lim, prob){
  # INPUTS:
  # wav: Wave, an element of wavlist (a recording as wave)
  # amp_lim : numeric, amplitude limit of detection
  # diff_lim : numeric, minimal distance between 2 peaks
  # probs : vector, probability of event and as many lines as pts
  # OUTPUT:
  # c : numeric, count of peaks in the wav
  c <- 1
  vec <- wav@left
  vecsel <- which((abs(vec) > amp_lim) & prob > 0.4)
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

#---- DETECTION ON COMPLETE SIGNAL ----

fich <- list.files(paste0(path,'cleanwav'))
dta <- lapply(fich, FUN = frame_cut, path = paste0(path,'cleanwav/'), 
              window_length = 0.1, overlap = 0.7)
data <- ldply(dta, rbind)
df_feature <- calc_features(data)

model <- readRDS("~/GitHub/ProjetInge/final_model_26_01.rds")
pred <- predict(model, newdata = df_feature, type = 'prob') # 17227 | 2
pred_frame <- data.frame(filename = df_feature$filename,
                         start = df_feature$start,
                         end = df_feature$end,
                         no_event = pred[,1],
                         event = pred[,2])

pred_pts <- as.list(fich)
a <- 1
for (audio in unique(data$filename)){
  df1 <- pred_frame %>% filter(filename == audio)
  n <- length(readWave(paste0(path, 'cleanwav/', audio)))
  df <- data.frame(pts = seq(0, n, by = 1), 
                   time = seq(0, n/44100, length.out = n+1), 
                   prob_0 = rep(0,n+1),
                   prob_1 = rep(0,n+1))
  for (k in 1:n){
    cat('_')
    t <- k/44100
    window_index <- which(t >= df1$start & t<= df1$end)
    df$prob_0[k] <- mean(df1$no_event[window_index])
    df$prob_1[k] <- mean(df1$event[window_index])
  }
  pred_pts[[a]] <- df
  a <- a+1
}





