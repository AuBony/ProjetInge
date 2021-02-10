#######################################################################
# PROJECT : CAT ACCOUSTIC DATA PROJECT
# TITLE : IIB2 - Method 1
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
IIB2_df_wav <- df_txt
IIB2_df_wav$filename <- str_replace(df_txt$filename, ".txt", ".wav")

#Cleaning environment
remove(data, data_modif, data_modif_chat_kibble_duration, data_modif_chat_kibble,df_txt) 


## FEATURES ----
## Goal : Extract features from recordings. Recordings are trimmed into fix-size frames. 
##        If an event or a part of an event is within the frame the frame is considered as an event.
## Input : df_wav (list of labelled events)
## Output : IIB2_df_feature list of features per frame for all recordings

# Functions
give_classif_event <- function(window_length = 0.8, wav_path = "data/wav/", data = IIB2_df_wav){
  # Goal : Trim recordings and get the class (event or no event) for each frame
  # Input : IIB2_df_wav (list of labelled event)
  # Output : Dataframe with all frames trimmed and if they're considered as an event or not
  
  #Library
  require(tuneR)
  require(dplyr)
  
  #Initialise the dataframe
  df_classif <- tibble(filename = character(),
                       start = numeric(),
                       end = numeric(),
                       event = numeric())
  
  #Browse through the recordings.
  for (audio_path in unique(data$filename)) {
    audio <- readWave(paste0(wav_path, audio_path))
    duration <- round(length(audio@left) / audio@samp.rate, 2)
    
    #Trimming a recording
    for(moment in seq(0, duration - window_length, by = window_length)){
      
      #Assignment of the event or non-event class (0: no event, 1: event)
      #If a part of an event is between moment and moment + window length (in the frame) then the frame is considered as an event
      isevent <- ifelse(test = nrow(data %>% filter((moment < start) & (start < moment + 0.1) | (moment < end) & (end < moment + 0.1))) > 0,
                        yes =  1,
                        no =  0)
      
      df_classif <- df_classif %>% add_row(filename = audio_path,
                                           start = moment,
                                           end = moment + window_length,
                                           event = isevent)
      
    }
  }
  return(as.data.frame(df_classif))
}

give_feature_2 <- function(wav_path = "data/wav/", data = df_classif){
  # Goal : Give features of frames
  # Input : Output of give_classif_event : a dataframe with all frames trimmed and
  #         if they're considered as an event or not
  # Output : Dataframe with features for each frames and if they are condidered as an event or not
  
  #Library 
  require(dplyr)
  require(tuneR)
  require(seewave)
  
  #Initialise the dataframe
  df_feature <- tibble(filename = character(),
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
               ssh = numeric(),
               sprec = numeric())
  
  for (j in 1:nrow(data)){
    
    #Counter
    if (j%%100){
      cat(".")
    }else{
      cat(".", "\n")
    }
    
    #Importing the frame as a wav file
    wav_file <- readWave(paste0(wav_path, data[j,1]),
                         from = data[j,2],
                         to = data[j,3],
                         units = "seconds") 
    #Extraction of spectro properties
    sp <- specprop(seewave::spec(wav_file@left, f = wav_file@samp.rate, plot = FALSE, scaled = TRUE, norm = FALSE))
    #Data recording.
    df_feature <- df_feature %>% add_row(filename = data[j,1],
                         start = data[j,2],
                         end = data[j,3],
                         event = data[j,4],
                         
                         th = th(env(wav_file, plot = FALSE)),
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
                         ssh = sp$sh,
                         sprec = sp$prec
    ) 
  }
  return(df_feature)
}

# Exectution
#Here bites and breaks are considered as event VS background noises
df_classif_event <- give_classif_event(window_length = 0.8, wav_path = "data/wav/", data = IIB2_df_wav)
IIB2_df_feature_event <- give_feature_2(wav_path = "data/wav/", data = df_classif_event)

#Here only breaks are considered as event VS other sounds
df_classif_croc <- give_classif_event(window_length = 0.8,
                                      wav_path = "data/wav/",
                                      data = IIB2_df_wav[IIB2_df_wav$annotation =="croc", ])
IIB2_df_feature_croc <- give_feature_2(wav_path = "data/wav/", data = df_classif_croc)

## ALGORITHM ----
## Goal : Attribute the event or no event class to a frame.
## Input : IIB2_df_feature (descriptors for each frame of the recordings)
## Output : Predicted class event or breaks for each frame
##          or breaks and bites VS background noises depending on the df_feature chosen

# Df_feature (If you have not run the previous part)
# IIB2_df_feature_event <- read.table("data/features/IIB2_df_feature_event.txt")
# IIB2_df_feature_croc <- read.table("data/features/IIB2_df_feature_croc.txt")


