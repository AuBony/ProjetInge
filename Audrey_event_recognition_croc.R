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



# TRAIN CROC
#

give_croc <- function(frame_size = 0.1, ovlp_frame = 0, percent_expansion = 0, wav_path = "ProjetInge/cleanwav/" ){
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
  for (audio in unique(df_wav[df_wav$annotation == "croc", "filename"])){
    
    cat(".")
    
    crocs <- df_wav %>% filter(filename ==  audio)
    

      
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




#Execution
wav_path <- "ProjetInge/cleanwav/"
croc <- give_croc()
croc

require(Factoshiny)
croc %>%  mutate(chat = as.character(map(strsplit(filename, "_"), 1)), 
                       kibble = as.character(map(strsplit(filename, "_"), 2))) %>% 
  Factoshiny()

#write.table(as.data.frame(df_feature_event), file = "data/data_perso/features/df_feature_event_01_14.txt")

# TRAIN NO EVENT
#

give_no_event <- function(frame_size = 0.1, ovlp_frame = 0, wav_path = "ProjetInge/cleanwav/"){
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
  for (audio in unique(df_wav$filename)){
    
    cat(".")
    
    no_event <- df_wav %>% filter(filename ==  audio, annotation == "croc")
    l_no_event <- 1
    
    if (dim(no_event)[1] == 0){
      deb <- 0
      audio_wav <- readWave(paste0(wav_path, audio), units = "seconds")
      fin <- duration <- round(length(audio_wav@left) / audio_wav@samp.rate, 2)
    }else{
      deb <- 0
      fin <- no_event$start[1]
    }
    

  
      #Definition d'une zone de sample pour nos frames (on peut définir un interval un peu plus grand)
      #Decoupage en frame
      if (fin - deb  > frame_size) {
        
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
        
        if (dim(no_event)[1] != 0){
          deb <- no_event[l_no_event, "end"]
          fin <- no_event[l_no_event + 1, "start"]
        }
        
        l_no_event <- l_no_event + 1
        
      
    }
    
  }
  
  return(as.data.frame(df_feature_no_event))
}

#Execution
no_event<- give_no_event()


# DATASET COMPLET CROC ET NO EVENT
detection  <- rbind.data.frame(croc, no_event)

#Data train test
set.seed(1234)
train_index_event_croc <- sample(1:nrow(croc), 0.7 * nrow(croc))
y_train_event_croc <- croc[train_index_event_croc, "event"]
x_train_event_croc <- croc[train_index_event_croc, 5:20]

train_index_event_no_event <- sample(1:nrow(no_event), 0.7 * nrow(no_event))
y_train_event_no_event <- no_event[train_index_event_no_event, "event"]
x_train_event_no_event <- no_event[train_index_event_no_event, 5:20]

y_train_event <- as.factor(c(y_train_event_croc, y_train_event_no_event))
x_train_event <- rbind.data.frame(x_train_event_croc, x_train_event_no_event)


#Test  
test_index_event_croc <- sample(1:nrow(croc), 0.7 * nrow(croc))
y_test_event_croc <- croc[-test_index_event_croc, "event"]
x_test_event_croc <- croc[-test_index_event_croc, 5:20]

test_index_event_no_event <- sample(1:nrow(no_event), 0.7 * nrow(no_event))
y_test_event_no_event <- no_event[-test_index_event_no_event, "event"]
x_test_event_no_event <- no_event[-test_index_event_no_event, 5:20]

y_test_event <- as.factor(c(y_test_event_croc, y_test_event_no_event))
x_test_event <- rbind.data.frame(x_test_event_croc, x_test_event_no_event)



############# CROC DETECTION #############

#RANDOM FOREST

  #Function ----
get.error <- function(class,pred){
  cont.tab <- table(class,pred)
  return((cont.tab[2,1]+cont.tab[1,2])/(sum(cont.tab)))
}

get.sensitivity <- function(class,pred){
  cont.tab <- table(class,pred)
  return((cont.tab[2,2])/(sum(cont.tab[2,])))
}


get.specificity <- function(class,pred){
  cont.tab <- table(class,pred)
  return((cont.tab[1,1])/(sum(cont.tab[1,])))
}

choose_ntree <- function(vect = c(1, 2, 3, 4, 5, 10, 15, 20, 25, 50, 75, 100), y_train, x_train, y_test, x_test){
  require(dplyr)
  df_ERROR <- data.frame(ntree = numeric(),
                         Error = numeric (), 
                         Sens = numeric(),
                         Spec = numeric())
  for (tree in vect){
    ERROR <-0
    SENS <- 0
    SPEC <- 0
    for (i in 1:100){
      model <- randomForest( y_train~ .,
                             data = x_train,
                             ntree = tree,
                             mtry = 4,
                             importance = TRUE)
      pred_test <-  predict(model, newdata = x_test)
      ERROR <- ERROR + get.error(y_test, pred_test)
      SENS <- SENS + get.sensitivity(y_test,pred_test)
      SPEC <- SPEC + get.specificity(y_test,pred_test)
    }
    cat("*")
    df_ERROR <- df_ERROR  %>% add_row(ntree = tree, Error = ERROR/100, Sens = SENS / 100, Spec = SPEC/100)
  }
  return(df_ERROR)
}

plot_dfERROR <- function(df_ERROR){
  plot(df_ERROR$ntree, df_ERROR$Sens, col = "orange", lwd = 2, type = 'l', ylim = c(0,1), xlab = "ntree", ylab = "")
  lines(df_ERROR$ntree, df_ERROR$Spec, col = 'green4', lwd = 2)
  lines(df_ERROR$ntree, df_ERROR$Error, col = 'red', lwd = 2)
  legend("right",legend = c("ERROR", "Specificity", "Sensitivity"), col = c('red', 'green4', 'orange'), fill =  c('red', 'green4', 'orange'))
}



  #Model ----
require(randomForest)

RF <- randomForest::randomForest(
  y_train_event ~ .,
  data = x_train_event,
  ntree = 40, 
  mtry = 4, 
  x_test = x_test_event,
  y_test = y_test_event,
  importance = TRUE)

RF
