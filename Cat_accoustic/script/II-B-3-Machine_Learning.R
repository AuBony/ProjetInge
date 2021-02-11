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


## 