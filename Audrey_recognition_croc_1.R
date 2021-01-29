# PROJET INGENIEUR
# ALGORITHME MACHINE LEARNING CROC DETECTION : METHODE DECOMPOSITION
# Audrey Bony
# 28/01/2021

# DATA WAV IMPORTATION ----

require(readr)  # for read_csv()
require(dplyr)  # for mutate()
require(tidyr)  # for unnest()
require(purrr)  # for map(), reduce()
library(stringr) # for str_replace()

setwd('~/GitHub/')

data_path <- "ProjetInge/labs/"
files <- dir(data_path, pattern = "*.txt")

data <- tibble(filename = files) %>%
  mutate(file_contents = map(filename,         
                             ~ read_delim(file.path(data_path, .),
                                          delim="\t",
                                          escape_double = FALSE,
                                          col_names = c("start", "end", "annotation"),
                                          trim_ws = TRUE)))


vec_noms <- c()
start <- c()
end <- c()
annotation <- c()
for (k in 1:nrow(data)){
  vec_noms <- c(vec_noms, rep(data[[1]][[k]], nrow(data[[2]][[k]])))
  start <- c(start, data[[2]][[k]][[1]])
  end <- c(end, data[[2]][[k]][[2]])
  annotation <- c(annotation, data[[2]][[k]][[3]])
}

data_modif <- data.frame(filename = vec_noms, start = start, 
                         end = end, annotation = annotation)

data_modif_chat_kibble_duration <- data_modif %>% 
  mutate(chat = as.character(map(strsplit(data_modif$filename, "_"), 1)), 
         kibble = as.character(map(strsplit(data_modif$filename, "_"), 2)),
         duration = end-start)
df_txt <- cbind.data.frame(data_frame(id = seq(1, nrow(data_modif_chat_kibble_duration))), data_modif_chat_kibble_duration)
df_wav <- df_txt
df_wav$filename <- str_replace(df_txt$filename, ".txt", ".wav")
df_wav

# FUNCTIONS ----

give_croc <- function(data = df_wav, expansion = 0, wav_path = "ProjetInge/cleanwav/" ){
  # gives features for each break frame
  
  require(dplyr)
  require(tuneR)
  require(seewave)
  
  df_feature_event <- tibble(filename = character(),
                             start = numeric(),
                             end = numeric(),
                             event = factor(levels = c('0','1')),
                             
                             th = numeric(),
                             maxdfreq = numeric(),
                             meandfreq = numeric(),
                             
                             smean = numeric(),
                             ssd = numeric(),
                             ssem = numeric(),
                             smedian = numeric(),
                             smode = numeric(),
                             sQ25 = numeric(),
                             sQ75 = numeric(),
                             sIQR = numeric(),
                             scent = numeric(),
                             sskewness = numeric(),
                             skurtosis = numeric(),
                             ssfm = numeric(),
                             ssh = numeric())
  
  for (audio in unique(data[, "filename"])){
    
    print(audio)
    
    crocs <- data %>% filter(filename ==  audio)
    
    
    #Selection d'un événement croc 
    for(l_croc in 1:nrow(crocs)){
      if (expansion == 0){
        wav_file <- readWave(paste0(wav_path, audio),
                             from = crocs$start[l_croc],
                             to = crocs$end[l_croc],
                             units = "seconds") 
        wav_file <- tuneR::normalize(wav_file, center = TRUE)
        
        #Features de la frame
        sp <- seewave::specprop(seewave::spec(wav_file@left, f = wav_file@samp.rate, plot = FALSE, scaled = FALSE, norm = FALSE))
        
        # Ajout des features de la frame
        df_feature_event <- df_feature_event %>% add_row(
          filename = audio,
          start =  crocs$start[l_croc],
          end =  crocs$end[l_croc],
          event = as.factor(1),
          
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
        
      } else if((crocs[l_croc,"start"] * (1 - expansion) > 0)){
        e <- crocs[l_croc,"duration"] * expansion
        moment <- list(c(crocs[l_croc,"start"] - e, crocs[l_croc, "end"] - e),
                            c(crocs[l_croc,"start"], crocs[l_croc, "end"]),
                            c(crocs[l_croc,"start"] + e, crocs[l_croc, "end"] + e))
        
        for (k in 1:3){
          wav_file <- readWave(paste0(wav_path, audio),
                               from = moment[[k]][1],
                               to = moment[[k]][2],
                               units = "seconds") 
          wav_file<- tuneR::normalize(wav_file, center = TRUE)
          
          #Features de la frame
          sp <- seewave::specprop(seewave::spec(wav_file@left, f = wav_file@samp.rate, plot = FALSE, scaled = FALSE, norm = FALSE))
          
          # Ajout des features de la frame
          df_feature_event <- df_feature_event %>% add_row(
            filename = audio,
            start =  moment[[k]][1],
            end =  moment[[k]][2],
            event = as.factor(1),
            
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
    }
  }
  return(as.data.frame(df_feature_event))
}

give_no_event <- function(data = df_wav, frame_size = 0.02, nb_ech = 5, wav_path = "ProjetInge/cleanwav/"){
  # gives features for each no_event frame
  
  require(pracma)
  require(dplyr)
  require(tuneR)
  require(seewave)
  
  df_feature_no_event <- tibble(filename = character(),
                             start = numeric(),
                             end = numeric(),
                             event = factor(levels = c('0','1')),
                             
                             th = numeric(),
                             maxdfreq = numeric(),
                             meandfreq = numeric(),
                             
                             smean = numeric(),
                             ssd = numeric(),
                             ssem = numeric(),
                             smedian = numeric(),
                             smode = numeric(),
                             sQ25 = numeric(),
                             sQ75 = numeric(),
                             sIQR = numeric(),
                             scent = numeric(),
                             sskewness = numeric(),
                             skurtosis = numeric(),
                             ssfm = numeric(),
                             ssh = numeric())
  
  for (audio in unique(data[, "filename"])){
    
    print(audio)
    
    no_event <- data %>% filter(filename ==  audio)
    
    for (l_no_event in 1:(nrow(no_event)+1)){
      
      if (l_no_event == 1){
        deb <- 0
        fin <- no_event$start[l_no_event]
      } else if (l_no_event > nrow(no_event)){
        deb <- no_event$end[l_no_event - 1]
        fin <- duration(readWave(filename = paste0(wav_path, audio)))
      } else if (l_no_event >1){
        deb <- no_event$end[l_no_event - 1]
        fin <- no_event$start[l_no_event]
      } 
      
      if (fin - deb > nb_ech * frame_size){
        
        deb_rand <- round(runif(min =  deb, max = (fin-frame_size), n = nb_ech),digits =  2)
        
        for (moment in deb_rand){
          wav_file <- readWave(paste0(wav_path, audio),
                               from = moment,
                               to = moment + frame_size,
                               units = "seconds") 
          wav_file <- tuneR::normalize(wav_file, center = TRUE)
          
          #Features de la frame
          sp <- seewave::specprop(seewave::spec(wav_file@left, f = wav_file@samp.rate, plot = FALSE, scaled = TRUE, norm = FALSE))
          #
          df_feature_no_event <- df_feature_no_event %>% add_row(
            filename = audio,
            start = moment,
            end = moment + frame_size,
            event = as.factor(0),
            
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
    }
  }
  return(as.data.frame(df_feature_no_event))
}

# VISUALISATION ----

# df1 <- give_croc(df_wav, expansion = 0.2)
# 
# require(Factoshiny)
# df1 %>% mutate(chat = as.character(map(strsplit(filename, "_"), 1)),
#               kibble = as.character(map(strsplit(filename, "_"), 2))) %>%
# Factoshiny()
# 
# df2 <- give_no_event(data = df_wav)
#
# rbind.data.frame(df1,df2) %>% 
#   mutate(chat = as.character(map(strsplit(filename, "_"), 1)),
#          kibble = as.character(map(strsplit(filename, "_"), 2))) %>% 
#   Factoshiny()

# COMPLETE DATASET, TRAIN/TEST ----

filename_train <- sample(unique(df_wav$filename), 0.7 * length(unique(df_wav$filename)))

df_train <- df_wav %>% filter(filename %in% filename_train)
df_test <- df_wav %>%  filter(!(filename %in% filename_train))

croc_train <- give_croc(data = df_train, expansion = 0.2)
no_event_train <- give_no_event(data = df_train)

croc_test <- give_croc(data = df_test, expansion = 0.2)
no_event_test <- give_no_event(data = df_test)

y_train <- as.factor(c(croc_train$event, no_event_train$event))
x_train <- rbind.data.frame(croc_train[, 5:20], no_event_train[, 5:20])

y_test <- as.factor((c(croc_test$event, no_event_test$event)))
x_test <- rbind.data.frame(croc_test[, 5:20], no_event_test[, 5:20])


# RANDOM FOREST ALGORITHM ----

require(randomForest)

RF <- randomForest::randomForest(
  y_train ~ .,
  data = x_train,
  ntree = 40, 
  mtry = 4, 
  x_test = x_test,
  y_test = y_test,
  importance = TRUE)
RF
varImpPlot(RF) # ssem = standard error of the mean of frequency

saveRDS(RF, file = 'C:/Users/HP/Documents/GitHub/ProjetInge/model_29_01_1.rds') 
