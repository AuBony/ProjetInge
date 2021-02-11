#######################################################################
# PROJECT : CAT ACCOUSTIC DATA PROJECT
# TITLE : IIB3 - Method 3
# AUTHOR : BONY Audrey & de CROUTTE Anne-Victoire (AGROCAMPUS OUEST)
# DATE : DECEMBER 2020 TO FEBRURARY 2021
#######################################################################

## DATASET DF_WAV ----
## Goal :  Obtain a dataframe containing all the events labelled in the recordings
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
data_path <- "data/labels/labels_1/"
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
IIB3_df_wav <- df_txt
IIB3_df_wav$filename <- str_replace(df_txt$filename, ".txt", ".wav")

#Cleaning environment
remove(data, data_modif, data_modif_chat_kibble_duration, data_modif_chat_kibble,df_txt) 


## FEATURES ----
## Goal : Extract features from recordings. Each break sound is cut into fix-size frames.
##        All areas between 2 breaks are considered as background noise and fully sampled.
## Input : df_wav (list of labelled events)
## Output : IIB3_df_feature list of features per frame for all noises
##          IIB3_break list of features per frame for all breaks
##          IIB3_noise list of features per frame for all background noises

# Functions
give_breaks <- function(frame_size = 0.1, ovlp_frame = 0, percent_expansion = 0, wav_path = "data/wav/", data = IIB3_df_wav){
  require(dplyr)
  require(tuneR)
  require(seewave)
  
  df_feature_event <- tibble(filename = character(),
                             start = numeric(),
                             end = numeric(),
                             event = numeric(),
                             
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
  
  #Selection d'un enregistrement
  for (audio in unique(data[data$annotation == "croc", "filename"])){
    
    cat(".")
    
    crocs <- data %>% filter(filename ==  audio)
    
    
    #Selection d'un événement croc 
    for(l_croc in 1:nrow(crocs)){
      
      
      #Definition d'une zone de sample pour nos frames (on peut définir un interval un peu plus grand)
      #Decoupage en frame
      if (crocs[l_croc,"end"] + (frame_size*percent_expansion) - frame_size - crocs[l_croc,"start"] + (frame_size*percent_expansion) > frame_size ){
        
        for (moment in seq(from =  crocs[l_croc,"start"] - (frame_size*percent_expansion), to = crocs[l_croc,"end"] + (frame_size*percent_expansion) - frame_size, by = frame_size * (1-ovlp_frame))){
          
          wav_file <- readWave(paste0(wav_path, audio),
                               from = moment,
                               to = moment + frame_size,
                               units = "seconds") 
          wav_file <- tuneR::normalize(wav_file, center = TRUE)
          
          #Features de la frame
          sp <- seewave::specprop(seewave::spec(wav_file@left, f = wav_file@samp.rate, plot = FALSE, scaled = TRUE, norm = FALSE))
          # Ajout des features de la frame
          df_feature_event <- df_feature_event %>% add_row(
            filename = audio,
            start = moment,
            end = moment + frame_size,
            event = 1,
            
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
give_no_event <- function(frame_size = 0.1, ovlp_frame = 0, wav_path = "data/wav/", data = IIB3_df_wav){
  require(dplyr)
  require(tuneR)
  require(seewave)
  
  df_feature_no_event <- tibble(filename = character(),
                                start = numeric(),
                                end = numeric(),
                                event = numeric(),
                                
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
  
  #Selection d'un enregistrement
  for (audio in unique(data$filename)){
    
    cat("_")
    
    no_event <- data %>% filter(filename ==  audio, annotation == "croc") #liste des crocs dans un enregistrement
    
    #Il n'y a pas de croc dans l'enregistrement
    if (dim(no_event)[1] == 0){
      deb <- 0
      audio_wav <- readWave(paste0(wav_path, audio), units = "seconds")
      fin <- round(length(audio_wav@left) / audio_wav@samp.rate, 2)
      
      for (moment in seq(from =  deb, to = fin - frame_size, by = frame_size * (1-ovlp_frame))){
        
        wav_file <- readWave(paste0(wav_path, audio),
                             from = moment,
                             to = moment + frame_size,
                             units = "seconds") 
        #Features de la frame
        sp <- seewave::specprop(seewave::spec(wav_file@left, f = wav_file@samp.rate, plot = FALSE, scaled = TRUE, norm = FALSE))
        #
        df_feature_no_event <- df_feature_no_event %>% add_row(
          filename = audio,
          start = moment,
          end = moment + frame_size,
          event = 0,
          
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
      
      
      #Il y a bien un croc dans l'enregistrement
    }else{
      
      for (l_no_event in 1: nrow(no_event)){
        #Deb et fin
        if (l_no_event == 1){
          deb <- 0
          fin <- no_event$start[1]
        }
        
        if (l_no_event >1){
          deb <- no_event$end[l_no_event - 1]
          fin <- no_event$start[l_no_event]
        }
        
        if (fin - deb > frame_size){
          for (moment in seq(from =  deb, to = fin - frame_size, by = frame_size * (1-ovlp_frame))){
            
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
              event = 0,
              
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
  }
  return(as.data.frame(df_feature_no_event))
}

# Execution
croc <- give_croc()
no_event<- give_no_event(data = df_wav_c)