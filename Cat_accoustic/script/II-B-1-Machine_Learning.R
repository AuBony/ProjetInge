###################################################
# PROJECT : CAT ACCOUSTIC DATA PROJECT
# TITLE : IIB1 - Method 1
# AUTHOR : BONY Audrey & de CROUTTE Anne-Victoire (AGROCAMPUS OUEST)
# DATE : DECEMBER 2020 TO FEBRURARY 2021
###################################################

# DATASET DF_WAV----
#
require(readr)  # for read_csv()
require(dplyr)  # for mutate()
require(tidyr)  # for unnest()
require(purrr)  # for map(), reduce()
library(stringr) # for str_replace()

data_path <- "data/labels/"
files <- dir(data_path, pattern = "*.txt")

data <- data_frame(filename = files) %>%
  mutate(file_contents = map(filename,         
                             ~ read_delim(file.path(data_path, .),
                                          delim="\t",
                                          escape_double = FALSE,
                                          col_names = c("start", "end", "annotation"),
                                          trim_ws = TRUE)))
data_modif <- unnest(data, cols = c(file_contents))
data_modif_chat_kibble_duration <- data_modif %>% 
  mutate(chat = as.character(map(strsplit(data_modif$filename, "_"), 1)), 
         kibble = as.character(map(strsplit(data_modif$filename, "_"), 2)),
         duration = end-start)
df_txt <- cbind.data.frame(data_frame(id = seq(1, nrow(data_modif_chat_kibble_duration))), data_modif_chat_kibble_duration)
df_wav <- df_txt
df_wav$filename <- str_replace(df_txt$filename, ".txt", ".wav")
