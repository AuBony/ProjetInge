#######################################################################
# PROJECT : CAT ACCOUSTIC DATA PROJECT
# TITLE : IIB5 - Different labelisation
# AUTHOR : BONY Audrey & de CROUTTE Anne-Victoire (AGROCAMPUS OUEST)
# DATE : DECEMBER 2020 TO FEBRURARY 2021
#######################################################################

# DATA ----
require(readr)  # for read_csv()
require(dplyr)  # for mutate()
require(tidyr)  # for unnest()
require(purrr)  # for map(), reduce()
library(stringr) # for str_replace()

# Labellisation 1
data_path_1 <- "ProjetInge/labels/"
files_1 <- dir(data_path_1, pattern = "*.txt")

data_1 <- tibble(filename = files_1) %>%
  mutate(file_contents = map(filename,         
                             ~ read_delim(file.path(data_path_1, .),
                                          delim="\t",
                                          escape_double = FALSE,
                                          col_names = c("start", "end", "annotation"),
                                          trim_ws = TRUE)))


vec_noms <- c()
start <- c()
end <- c()
annotation <- c()
for (k in 1:nrow(data_1)){
  vec_noms <- c(vec_noms, rep(data_1[[1]][[k]], nrow(data_1[[2]][[k]])))
  start <- c(start, data_1[[2]][[k]][[1]])
  end <- c(end, data_1[[2]][[k]][[2]])
  annotation <- c(annotation, data_1[[2]][[k]][[3]])
}

data_modif_1 <- data.frame(filename = vec_noms, start = start, 
                           end = end, annotation = annotation)

data_modif_chat_kibble_duration_1 <- data_modif_1 %>% 
  mutate(chat = as.character(map(strsplit(data_modif_1$filename, "_"), 1)), 
         kibble = as.character(map(strsplit(data_modif_1$filename, "_"), 2)),
         duration = end-start)
df_txt_1 <- cbind.data.frame(data_frame(id = seq(1, nrow(data_modif_chat_kibble_duration_1))), data_modif_chat_kibble_duration_1)
df_wav_1 <- df_txt_1
df_wav_1$filename <- str_replace(df_txt_1$filename, ".txt", ".wav")
df_wav_1 <- df_wav_1 %>% filter(annotation == "croc")

#Labellisation 2
data_path_2 <- "ProjetInge/labs/"
files_2 <- dir(data_path_2, pattern = "*.txt")

data_2 <- tibble(filename = files_2) %>%
  mutate(file_contents = map(filename,         
                             ~ read_delim(file.path(data_path_2, .),
                                          delim="\t",
                                          escape_double = FALSE,
                                          col_names = c("start", "end", "annotation"),
                                          trim_ws = TRUE)))


vec_noms <- c()
start <- c()
end <- c()
annotation <- c()
for (k in 1:nrow(data_2)){
  vec_noms <- c(vec_noms, rep(data_2[[1]][[k]], nrow(data_2[[2]][[k]])))
  start <- c(start, data_2[[2]][[k]][[1]])
  end <- c(end, data_2[[2]][[k]][[2]])
  annotation <- c(annotation, data_2[[2]][[k]][[3]])
}

data_modif_2 <- data.frame(filename = vec_noms, start = start, 
                           end = end, annotation = annotation)

data_modif_chat_kibble_duration_2 <- data_modif_2 %>% 
  mutate(chat = as.character(map(strsplit(data_modif_2$filename, "_"), 1)), 
         kibble = as.character(map(strsplit(data_modif_2$filename, "_"), 2)),
         duration = end-start)
df_txt_2 <- cbind.data.frame(data_frame(id = seq(1, nrow(data_modif_chat_kibble_duration_2))), data_modif_chat_kibble_duration_2)
df_wav_2 <- df_txt_2
df_wav_2$filename <- str_replace(df_txt_2$filename, ".txt", ".wav")
