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

#DATA_FEATURE ----
#library
library(soundgen)
library(tuneR)
library(seewave)

#Trouver les pics
ffilter <- 500 # Fréquence minimale analysée
lim_db <- -20 # Seuil de décibel pour le comptage 

a <- readWave("ProjetInge/cleanwav/notchi_B_2.wav")
audio <- ffilter(a, f = a@samp.rate, channel = 1, from = 0, to = ffilter, bandpass = FALSE,
        custom = NULL, wl = 1024, ovlp = 75, wn = "hanning", fftw = FALSE,
        rescale=FALSE, listen=FALSE, output="Wave")
spe <- spectro(audio)
mat <- spe$amp 
f <- mat > lim_db
apply(f, FUN = sum, 2)



# Train Croc
for (audio in unique(data_wav$filename)){
  require(dplyr)
  
  audio <- readWave(paste0(file_wav_path, audio_path))
  dat <- data_wav %>% filter(filename ==  audio)
  
  for(l_croc in 1:nrow(dat)){
    
  }
  
}

#Function
give_classif_event <- function(window_length = 0.8, data = df_wav){
  require(tuneR)
  require(dplyr)
  
  df_classif <- tibble(filename = character(),
                       start = numeric(),
                       end = numeric(),
                       event = numeric())
  
  #Etape 1 : Parcourir les enregistrements labellisés
  for (audio_path in unique(df_wav$filename)) {
    
    audio <- readWave(paste0(file_wav_path, audio_path))
    duration <- round(length(audio@left) / audio@samp.rate, 2)
    #Etape 2 : Déplacement dans un enregistrement par frame
    for (moment in seq(0, duration - window_length, by = window_length)){
      
      #Etape 3 : Définir si la frame est un event (1) ou non (0)
      
      isevent <- dim(df_wav %>%  filter(filename == audio_path,
                                        ((start <= moment) & ((moment + window_length) < end)) | ( (moment < start) & (start < moment+window_length)) | ((moment < end) & (end < moment+window_length)) ))[1]
      isevent <- ifelse(isevent > 2, yes = 1, no = isevent)  
      df_classif <- df_classif %>% add_row(filename = audio_path,
                                           start = moment,
                                           end = moment + window_length,
                                           event =isevent
      )
    }
  }
  return(df_classif)
}

give_feature_event <- function(df_event, wav_path = "ProjetInge/cleanwav/"){
  # Obtenir les features pour un événement
  # input : une ligne de df_wav(id, filename, start, end, annotation, ...)
  # output : un tableau avec les features pour chaque événement libéllé df_feature(filename, annotation, features)
  require(soundgen)
  require(tuneR)
  require(seewave)
  wav_path <- wav_path
  df <- init_df_feature_event()
  
  for (j in 1:nrow(df_event)){
    if (j%%100){
      cat(".")
    }else{
      cat(".", "\n")
    }
    
    wav_file <- readWave(paste0(wav_path, df_event[j,1]),
                         from = df_event[j,2],
                         to = df_event[j,3],
                         units = "seconds") 
    #
    sp <- specprop(seewave::spec(wav_file@left, f = wav_file@samp.rate, plot = FALSE, scaled = TRUE, norm = FALSE))
    #
    df <- df %>% add_row(filename = df_event[j,1],
                         start = df_event[j,2],
                         end = df_event[j,3],
                         event = df_event[j,4],
                         
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
  
  return(df)
}

init_df_feature_event<- function(){
  df <- tibble(filename = character(),
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
  return(df)
}

#Execution
wav_path <- "ProjetInge/cleanwav/"
df_event <- as.data.frame(give_classif_event(data = df_wav, window_length = 0.1))
df_event
df_feature_event <- as.data.frame(give_feature_event(df_event = df_event, wav_path = "ProjetInge/cleanwav/"))
#write.table(as.data.frame(df_feature_event), file = "data/data_perso/features/df_feature_event_01_14.txt")
#df_feature_event <- df_feature_event %>% mutate(event = replace(event, event == 2, 1))

#Factoshiny
require(Factoshiny)
df_feature_event$event <- as.character(df_feature_event$event)
Factoshiny(df_feature_event)

#Data train test
train_index_event <- sample(1:nrow(df_feature_event), 0.7 * nrow(df_feature_event))
y_train_event <- as.factor(df_feature_event[train_index_event, "event"])
x_train_event <- df_feature_event[train_index_event, 5:21]
train_event <- cbind.data.frame(x_train_event,
                                y_train_event,
                                deparse.level = 1)
#Test  
y_test_event <- as.factor(df_feature_event[-train_index_event, "event"])
x_test_event <- as.data.frame(df_feature_event[-train_index_event, 5:21])
test_event <- cbind.data.frame(x_train_event,
                               y_train_event,
                               deparse.level = 1)

