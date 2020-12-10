# PROJET INGENIEUR
# MACHINE LEARNING ET CLASSIFICATION : Recognition algorithm method
# Audrey Bony
# 7/12/2020
# Source : https://blogs.rstudio.com/ai/posts/2018-06-06-simple-audio-classification-keras/

# Download ----
load("../data/Mars.RData")

# Import ----
"
Création d'un tibble avec 3 colonnes : 
  1. Nom de fichier
  2. Classe du fichier
  3. Classe_id

"
library(stringr)
library(dplyr)

files <- fs::dir_ls(
  path = "../data/audio_train/", 
  recurse = TRUE, 
  glob = "*.wav"
)

files <- files[!str_detect(files, "background_noise")]

get_class <- function(fname){
  if (fname %>% str_extract("croc") == "croc"){
    class = fname %>% str_extract("croc")
  }else{
    class = fname %>% str_extract("mach")
  }
}

df <- tibble(
  fname = files, 
  
  class = get_class(fname),
  
  class_id = class %>% as.factor() %>% as.integer() - 1L #class de 0 à n-1
)

# Generator ----
library(tfdatasets)
ds <- tensor_slices_dataset(df)

window_size_ms <- 30
window_stride_ms <- 10


