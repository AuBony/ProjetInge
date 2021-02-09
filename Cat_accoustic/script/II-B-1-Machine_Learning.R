###################################################
# PROJECT : CAT ACCOUSTIC DATA PROJECT
# TITLE : IIB1 - Method 1
# AUTHOR : BONY Audrey & de CROUTTE Anne-Victoire (AGROCAMPUS OUEST)
# DATE : DECEMBER 2020 TO FEBRURARY 2021
###################################################

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
IIB1_df_wav <- df_txt
IIB1_df_wav$filename <- str_replace(df_txt$filename, ".txt", ".wav")

#Cleaning environment
remove(data, data_modif, data_modif_chat_kibble_duration, data_modif_chat_kibble,df_txt) 

## FEATURES ----
## Goal : Extract features from an event (croc or mach). The entire labelled event is used and NOT cut into frames.
## Input : df_wav (list of labelled events)
## Output : IIB1_df_feature list of features per event

#Libraries
library(soundgen)
library(tuneR)
library(seewave)

#Edit path (direction where the wav files are)
wav_path <- "data/wav/"

#Init df_feature
IIB1_df_feature <- tibble(id = numeric(),
                     filename = character(),
                     annotation = character(),
                     
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
                     sprec = numeric()
)

# Run through df_wav line by line
for (i in 1:nrow(df_wav)){
  # wav_file contains an event (croc or mach)
  wav_file <- readWave(paste0(wav_path, df_wav[i,2]),
                       from = df_wav[i,3],
                       to = df_wav[i,4],
                       units = "seconds") 
  
  # Obtain a list of statistical properties of a frequency spectrum
  sp <- specprop(spec(wav_file@left, f = wav_file@samp.rate, plot = FALSE, scaled = TRUE, norm = FALSE))
  
  # Add Features into IIB1_df_feature
  IIB1_df_feature <- IIB1_df_feature %>% add_row(id = df_wav$id[i],
                                       filename = df_wav$filename[i],
                                       annotation = df_wav$annotation[i],
                                       
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
