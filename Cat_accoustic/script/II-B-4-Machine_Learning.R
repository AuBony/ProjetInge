#######################################################################
# PROJECT : CAT ACCOUSTIC DATA PROJECT
# TITLE : IIB4 - Method 4
# AUTHOR : BONY Audrey & de CROUTTE Anne-Victoire (AGROCAMPUS OUEST)
# DATE : DECEMBER 2020 TO FEBRURARY 2021
#######################################################################

## DATASET DF_WAV ----
## Goal : Obtain a dataframe containing all the events labeled in the recordings
##        Here we use the second type labeling. We labeled only a part of a break and not the whole break sound 
## Input : Txt files per recordings resulting from the audacity labelling process (In each file the start and end of each event labellised)
## Output : Dataframe (nb_observation x 8 : 
##         filename,
##         start of the label, end of the label, duration of the event (end - start),
##         annotation : croc (= break) or mach (= bite),
##         cat, kibble 


#Libraries 
require(readr)  # for read_csv()
require(dplyr)  # for mutate()
require(tidyr)  # for unnest()
require(purrr)  # for map(), reduce()
library(stringr) # for str_replace()

#Data manipulation
data_path <- "data/labels/labels_2/"
files <- dir(data_path, pattern = "*.txt")

data <- data_frame(filename = files) %>%
  mutate(file_contents = map(filename,         
                             ~ read_delim(file.path(data_path, .),
                                          delim="\t",
                                          escape_double = FALSE,
                                          col_names = c("start", "end", "annotation"),
                                          trim_ws = TRUE)))

data_modif <- unnest(data, cols = c(file_contents))

# Add Cat and Kibble
data_modif_chat_kibble <- data_modif %>% 
  mutate(chat = as.character(map(strsplit(data_modif$filename, "_"), 1)), 
         kibble = as.character(map(strsplit(data_modif$filename, "_"), 2)))

# Add duration
data_modif_chat_kibble_duration <- data_modif_chat_kibble %>%  mutate(duration = end - start)

# Add id_label
df_txt <- cbind.data.frame(data_frame(id = seq(1, nrow(data_modif_chat_kibble_duration))), data_modif_chat_kibble_duration)

# Modify filename .txt to .wav
IIB4_df_wav <- df_txt
IIB4_df_wav$filename <- str_replace(df_txt$filename, ".txt", ".wav")

#Cleaning environment
remove(data, data_modif, data_modif_chat_kibble_duration, data_modif_chat_kibble,df_txt) 

## FEATURES ----
## Goal : Extract features from recordings. Each break sound is sampled.
##        For background noise sampling, only a fixed number of frame is taken.
## Input : df_wav (list of labeled events)
## Output : IIB4_break list of features per frame for all breaks frames
##          IIB4_noise list of features per frame for all background noises frames

IIB4_give_break <- function(shift = 0, wav_path = "data/wav/", data = IIB4_df_wav){
  # Goal : Give features of frames from IIB4_df_wav recordings for break sounds
  # Input : list of labeled events
  #         shift : percent of sound duration used to shift.
  #         If shift > 0, you'll get 3 frames for each sound instead of one.
  #         Frame 1 : the actual sound
  #         Frame 2 : the sound shifted by shift% of the duration of the sound on the right
  #         Frame 3 : the sound shifted by shift% of the duration of the sound on the left
  # Output : Dataframe with features for each frame.
  
  #Library
  require(dplyr)
  require(tuneR)
  require(seewave)
  
  #Initialise the dataframe
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
  
  #Browse through the recordings.
  for (audio in unique(data[, "filename"])){
    
    print(audio)
    
    crocs <- data %>% filter(filename ==  audio)
    
    #Selection of breaks in a recording  
    for(l_croc in 1:nrow(crocs)){
      
      #Only one frame per sound
      if (shift == 0){
        wav_file <- readWave(paste0(wav_path, audio),
                             from = crocs$start[l_croc],
                             to = crocs$end[l_croc],
                             units = "seconds") 
        
        #Extraction of spectro properties
        sp <- seewave::specprop(seewave::spec(wav_file@left, f = wav_file@samp.rate, plot = FALSE, scaled = FALSE, norm = FALSE))
        
        #Data recording.
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
        
      # Here shift > 0, we check that the shift to the left does not take us out of the recording
      } else if((crocs[l_croc,"start"] * (1 - shift) > 0)){
        e <- crocs[l_croc,"duration"] * shift
        moment <- list(c(crocs[l_croc,"start"] - e, crocs[l_croc, "end"] - e),
                       c(crocs[l_croc,"start"], crocs[l_croc, "end"]),
                       c(crocs[l_croc,"start"] + e, crocs[l_croc, "end"] + e))
        
        for (k in 1:3){
          wav_file <- readWave(paste0(wav_path, audio),
                               from = moment[[k]][1],
                               to = moment[[k]][2],
                               units = "seconds") 
          
          #Extraction of spectro properties
          sp <- seewave::specprop(seewave::spec(wav_file@left, f = wav_file@samp.rate, plot = FALSE, scaled = FALSE, norm = FALSE))
          
          #Data recording.
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

IIB4_give_no_event <- function(frame_size = 0.02, nb_ech = 5, wav_path = "data/wav/", data = IIB4_df_wav){
  # Goal : Give features of frames from IIB4_df_wav recordings for background noises
  # Input : list of labeled events
  #         frame size, nb_ech : number of frame per part of background noise.
  # Output : Dataframe with features for each frame.
  #         End of recordings are not sampled nor too short background noises parts.
  
  #Library
  require(pracma)
  require(dplyr)
  require(tuneR)
  require(seewave)
  
  #Initialise the dataframe
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
  #Browse into recordings
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
      
      if (((fin - deb) > (frame_size + nb_ech*0.0001)) & ((fin - deb) > 0.2)){
        
        deb_rand <- sample(x = (deb*10000):((fin-frame_size)*10000), size = nb_ech,
                           replace = FALSE) / 10000
        
        for (moment in deb_rand){
          wav_file <- readWave(paste0(wav_path, audio),
                               from = moment,
                               to = moment + frame_size,
                               units = "seconds") 
          
          #Extraction of spectro properties
          sp <- seewave::specprop(seewave::spec(wav_file@left, f = wav_file@samp.rate, plot = FALSE, scaled = TRUE, norm = FALSE))
          #Data recording.
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

## DATASET TRAIN TEST ----

filename_train <- sample(unique(IIB4_df_wav$filename), 0.7 * length(unique(IIB4_df_wav$filename)))

IIB4_df_train <- IIB4_df_wav %>% filter(filename %in% filename_train)
IIB4_df_test <- IIB4_df_wav %>%  filter(!(filename %in% filename_train))

IIB4_break_train <- IIB4_give_break(shift = 0.2, wav_path = "data/wav/", data = IIB4_df_train)
IIB4_no_event_train <- IIB4_give_no_event(frame_size = 0.02, nb_ech = 10, data = IIB4_df_train)

IIB4_break_test <- IIB4_give_break(shift = 0.2, wav_path = "data/wav/", data = IIB4_df_test)
IIB4_no_event_test <- IIB4_give_no_event(frame_size = 0.02, nb_ech = 10, data = IIB4_df_test)

# IIB4_break_train <- read.table("data/features/IIB4_break_train.txt")
# IIB4_no_event_train <- read.table("data/features/IIB4_no_event_train.txt")
# IIB4_break_test <- read.table("data/features/IIB4_break_test.txt")
# IIB4_no_event_test<- read.table("data/features/IIB4_no_event_test.txt")
IIB4_y_train <- as.factor(c(IIB4_break_train$event, IIB4_no_event_train$event))
IIB4_x_train <- rbind.data.frame(IIB4_break_train[, 5:20], IIB4_no_event_train[, 5:20])

IIB4_y_test <- as.factor((c(IIB4_break_test$event, IIB4_no_event_test$event)))
IIB4_x_test <- rbind.data.frame(IIB4_break_test[, 5:20], IIB4_no_event_test[, 5:20])


## ALGORITHM ----
## Goal : Attribute the event or no event class to a frame.
## Input : IIB4_df_feature (descriptors for each frame of the recordings)
## Output : Predicted class event or breaks for each frame
##          or breaks and bites VS background noises depending on the df_feature chosen

# Library 
require(randomForest)

# 
RF <- randomForest::randomForest(
  y_train ~ .,
  data = x_train,
  ntree = 40, 
  mtry = 4, 
  x_test = x_test,
  y_test = y_test,
  importance = TRUE)