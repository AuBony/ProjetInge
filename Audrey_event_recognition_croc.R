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
df_wav

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

give_croc_train <- function(frame_size = 0.1, ovlp_frame = 0, percent_expansion = 0, wav_path = "ProjetInge/cleanwav/" ){
  require(dplyr)
  require(tuneR)
  require(seewave)
  
  df_feature_event <- tibble(filename = character(),
                             start = numeric(),
                             end = numeric(),
                             event = numeric(),
                             
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
                             ssh = sp$sh)
  
  #Selection d'un enregistrement
  for (audio in unique(df_wav$filename)){
    
    cat(".")
    
    crocs <- df_wav %>% filter(filename ==  audio)
    
    #Selection d'un événement croc 
    for(l_croc in 1:nrow(crocs)){
      
      
      #Definition d'une zone de sample pour nos frames (on peut définir un interval un peu plus grand)
      duration <- crocs[l_croc,"end"] - crocs[l_croc,"start"] + 2*(frame_size*percent_expansion)
      
        #Decoupage en frame
      for (moment in seq(from =  crocs[l_croc,"start"] - (frame_size*percent_expansion), to = crocs[l_croc,"end"] + (frame_size*percent_expansion), by = frame_size * (1-ovlp_frame))){
        
        wav_file <- readWave(paste0(wav_path, audio),
                             from = moment,
                             to = moment + frame_size,
                             units = "seconds") 
        #Features de la frame
        sp <- seewave::specprop(seewave::spec(wav_file@left, f = wav_file@samp.rate, plot = FALSE, scaled = TRUE, norm = FALSE))
        #
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
  
  return(as.data.frame(df_feature_event))
}




#Execution
wav_path <- "ProjetInge/cleanwav/"
df_feature_event <- give_croc_train()

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

