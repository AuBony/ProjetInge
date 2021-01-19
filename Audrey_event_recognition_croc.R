# PROJET INGENIEUR
# Identification des évènements Croc VS mach et Event VS NoEvent
# Audrey Bony
# 12/01/2021

############# DATASET #############

#DATA_WAV ----
require(readr)  # for read_csv()
require(dplyr)  # for mutate()
require(tidyr)  # for unnest()
require(purrr)  # for map(), reduce()
library(stringr) # for str_replace()

data_path <- "ProjetInge/labels/"
files <- dir(data_path, pattern = "*.txt")

data <- tibble(filename = files) %>%
  mutate(file_contents = map(filename,         
                             ~ read_delim(file.path(data_path, .),
                                          delim="\t",
                                          escape_double = FALSE,
                                          col_names = c("start", "end", "annotation"),
                                          trim_ws = TRUE))) %>% 
  unnest(cols = c(file_contents)) %>% 
  filter(annotation == "croc") %>% 
  mutate(chat = as.character(map(strsplit(filename, "_"), 1)), 
         kibble = as.character(map(strsplit(filename, "_"), 2)),
         duration = end-start)
data_id <- cbind.data.frame(data_frame(id = seq(1, nrow(data))), data)
data_wav <- data_id
data_wav$filename <- str_replace(data_wav$filename, ".txt", ".wav")
data_wav

