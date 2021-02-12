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


## FEATURES ----
## Goal : Extract features from recordings. Each break sound is cut into fix-size frames.
##        All areas between 2 breaks are considered as background noise and fully sampled.
## Input : df_wav (list of labeled events)
## Output : IIB3_df_feature list of features per frame for all noises
##          IIB3_break list of features per frame for all breaks
##          IIB3_noise list of features per frame for all background noises

# Functions
give_breaks <- function(frame_size = 0.1, ovlp_frame = 0, percent_expansion = 0, wav_path = "data/wav/", data = IIB3_df_wav){
  # Goal : Give features of frames from IIB3_df_wav recordings for break sounds
  # Input : list of labeled events
  #         frame size, ovl_frame : percentage of overlap between frames within the same sound,
  #         percent_expansion : percentage of the window size that will be taken to exceed on either side of the labelled area,
  #         It means the beginning frame will have a proportion of percent_expansion % which will not be a break sound but a background noise sound.
  #         Same for the ending frame.
  # Output : Dataframe with features for each frame.
  
  #Library
  require(dplyr)
  require(tuneR)
  require(seewave)
  
  #Initialise the dataframe
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
  
  #Browse through the recordings.
  for (audio in unique(data[data$annotation == "croc", "filename"])){
    
    cat(".")
    
    crocs <- data %>% filter(filename ==  audio)
    
    
    #Selection of breaks in a recording 
    for(l_croc in 1:nrow(crocs)){
      
      
      #Definition of a sample area for our frames (we can define a slightly larger interval)
      #Cutting into frames
      #Checking if the break is long enough to be cut into frames
      if (crocs[l_croc,"end"] + (frame_size*percent_expansion) - frame_size - crocs[l_croc,"start"] + (frame_size*percent_expansion) > frame_size ){
        
        #Browse into a break sound
        for (moment in seq(from =  crocs[l_croc,"start"] - (frame_size*percent_expansion), to = crocs[l_croc,"end"] + (frame_size*percent_expansion) - frame_size, by = frame_size * (1-ovlp_frame))){
          
          wav_file <- readWave(paste0(wav_path, audio),
                               from = moment,
                               to = moment + frame_size,
                               units = "seconds")
          
          #Extraction of spectro properties
          sp <- seewave::specprop(seewave::spec(wav_file@left, f = wav_file@samp.rate, plot = FALSE, scaled = TRUE, norm = FALSE))
          #Data recording.
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
give_no_event <- function(frame_size = 0.1, ovlp_frame = 0, wav_path = "data/wav/", data = IIB3_df_wav){
  # Goal : Give features of frames from IIB3_df_wav recordings for background noises
  # Input : list of labeled events
  #         frame size, ovl_frame : percentage of overlap between frames within the same sound,
  # Output : Dataframe with features for each frame.
  #         End of recordings are not sampled nor too short background noises parts.
  
  #Library
  require(dplyr)
  require(tuneR)
  require(seewave)
  
  #Initialise the dataframe
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
  for (audio in unique(data$filename)){
    
    cat("_")
    
    no_event <- data %>% filter(filename ==  audio, annotation == "croc") #List of break events into the recording
    
    #In case there is no break into the recording
    if (dim(no_event)[1] == 0){
      deb <- 0
      audio_wav <- readWave(paste0(wav_path, audio), units = "seconds")
      fin <- round(length(audio_wav@left) / audio_wav@samp.rate, 2)
      
      for (moment in seq(from =  deb, to = fin - frame_size, by = frame_size * (1-ovlp_frame))){
        
        wav_file <- readWave(paste0(wav_path, audio),
                             from = moment,
                             to = moment + frame_size,
                             units = "seconds") 
        #Extraction of spectro properties
        sp <- seewave::specprop(seewave::spec(wav_file@left, f = wav_file@samp.rate, plot = FALSE, scaled = TRUE, norm = FALSE))
        #Data recording.
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

    #In case there is at least one break into the recording
    }else{
      
      for (l_no_event in 1: nrow(no_event)){
        #If there is only one break
        if (l_no_event == 1){
          deb <- 0
          fin <- no_event$start[1]
        }
        #If there is more than one
        if (l_no_event >1){
          deb <- no_event$end[l_no_event - 1]
          fin <- no_event$start[l_no_event]
        }
        #The duration of the break has to be longer than the frame size
        if (fin - deb > frame_size){
          for (moment in seq(from =  deb, to = fin - frame_size, by = frame_size * (1-ovlp_frame))){
            
            wav_file <- readWave(paste0(wav_path, audio),
                                 from = moment,
                                 to = moment + frame_size,
                                 units = "seconds") 
            wav_file <- tuneR::normalize(wav_file, center = TRUE)
            #Extraction of spectro properties
            sp <- seewave::specprop(seewave::spec(wav_file@left, f = wav_file@samp.rate, plot = FALSE, scaled = TRUE, norm = FALSE))
            #Data recording.
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
        }
      }
    }
  }
  return(as.data.frame(df_feature_no_event))
}

# Execution Data Test and Data Train
IIB3_breaks <- give_breaks(frame_size = 0.1, ovlp_frame = 0, percent_expansion = 0, wav_path = "data/wav/", data = IIB3_df_wav)
IIB3_no_event<- give_no_event(frame_size = 0.1, ovlp_frame = 0, wav_path = "data/wav/", data = IIB3_df_wav)


filename_train <- sample(unique(IIB3_df_wav$filename), 0.7 * length(unique(IIB3_df_wav$filename)))

IIB3_df_train <- IIB3_df_wav %>% filter(filename %in% filename_train)
IIB3_df_test <- IIB3_df_wav %>%  filter(!(filename %in% filename_train))

IIB3_breaks_train <- give_breaks(frame_size = 0.1, ovlp_frame = 0, percent_expansion = 0, wav_path = "data/wav/", data = IIB3_df_train)
IIB3_no_event_train <- give_no_event(frame_size = 0.1, ovlp_frame = 0, wav_path = "data/wav/", data = IIB3_df_train)

IIB3_breaks_test <- give_breaks(frame_size = 0.1, ovlp_frame = 0, percent_expansion = 0, wav_path = "data/wav/", data = IIB3_df_test)
IIB3_no_event_test <- give_no_event(frame_size = 0.1, ovlp_frame = 0, wav_path = "data/wav/", data = IIB3_df_test)


IIB3_y_train <- as.factor(c(IIB3_breaks_train$event, IIB3_no_event_train$event))
IIB3_x_train <- rbind.data.frame(IIB3_breaks_train[, 5:20], IIB3_no_event_train[, 5:20])

IIB3_y_test <- as.factor((c(IIB3_breaks_test$event, IIB3_no_event_test$event)))
IIB3_x_test <- rbind.data.frame(IIB3_breaks_test[, 5:20], IIB3_no_event_test[, 5:20])

## ALGORITHM ----
## Goal : Attribute the event or no event class to a frame.
## Input : IIB2_df_feature (descriptors for each frame of the recordings)
## Output : Predicted class event or breaks for each frame
##          or breaks and bites VS background noises depending on the df_feature chosen

# Df_feature (If you have not run the previous part)
# IIB3_y_train <- read.table("data/features/IIB3_y_train.txt")
# IIB3_x_train <- read.table("data/features/IIB3_x_train.txt")
# IIB3_y_test <- read.table("data/features/IIB3_y_test.txt")
# IIB3_x_test <- read.table("data/features/IIB3_x_test.txt")

# Function 
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

# Model Random Forest 
require(randomForest)
require(ROCR)

IIB3_model <- randomForest::randomForest(
  IIB3_y_train ~ .,
  data = IIB3_x_train,
  ntree = 40, 
  mtry = 4, 
  x_test = IIB3_x_test,
  y_test = IIB3_y_test,
  importance = TRUE)

IIB3_model

pred_RF <- predict(IIB3_model, newdata = IIB3_x_test, type = "prob")
pred_class <-  prediction(pred_RF[,2], IIB3_y_test)
pred.test <- predict(IIB3_model, newdata = IIB3_x_test)
performance_RF <- performance(pred_class,measure = "tpr",x.measure= "fpr")
plot(performance_RF, col = 4, lwd = 2)
abline(0,1)

## PARAMETERS ----
## Goal : Select the best parameters combination
## Input : A range of values for all parameters. df_wav (list of labeled events).
##        You need to execute functions in the part FEATURES.
## Output : Error, specificity and sensibility of the model for each combination of parameters.

#Parameters range
range_size <- c(0.1, 0.2, 0.05)
range_ovl_break <- c(0.5, 0.9, 0.2)
range_ovl_no_event <- c(0, 0.2, 0.2)
range_exp <- c(0, 0.2, 0.2)

repet <- 10 #Number of repetitions of a random cut of the dataset 

#Initialisation
IIB3_df_ERROR_ovl <- tibble(
  expansion = numeric(),
  size = numeric(),
  ovl_croc = numeric(),
  ovl_no_event = numeric(),
  Error = numeric (),
  Sens = numeric(),
  Spec = numeric(),
  class_error_0 = numeric(),
  class_error_1 = numeric())

#Counter
tour <- 0

#Total number of combinations
total <- length(seq(from = range_size[1], to = range_size[2], by = range_size[3] )) *
  length(seq(from = range_ovl_break[1], to = range_ovl_break[2], by = range_ovl_break[3] )) * 
  length(seq(from = range_ovl_no_event[1], to = range_ovl_no_event[2], by = range_ovl_no_event[3] )) * 
  length(seq(from = range_exp[1], to = range_exp[2], by = range_exp[3] ))
total

#Execution
#BEWARE : Depending on your choice of value ranges the execution time can be very long!
#frame size
for (size in seq(from = range_size[1], to = range_size[2], by = range_size[3] )){
  #Expansion
  for (expansion in seq(from = range_exp[1], to = range_exp[2], by = range_exp[3] )){
    #Overlap
    for (ovl_break in seq(from = range_ovl_break[1], to = range_ovl_break[2], by = range_ovl_break[3] )){
      for (ovl_no_event in seq(from = range_ovl_no_event[1], to = range_ovl_no_event[2], by = range_ovl_no_event[3] )){
        
        require(randomForest)
        require(dplyr)
        tour <- tour + 1
        print(paste0(tour, "/", total))
        
        ERROR <-0
        SENS <- 0
        SPEC <- 0
        CONFU_0 <- 0
        CONFU_1 <- 0
        
        features_croc <- give_breaks(frame_size = size, ovlp_frame = ovl_break,
                                     percent_expansion = expansion,
                                     wav_path = "data/wav/", data = IIB3_df_wav)
        features_no_event <- give_no_event(frame_size = size, ovlp_frame = ovl_no_event,
                                           wav_path = "data/wav/", data = IIB3_df_wav)
        
        
        for (i in 1:repet){
          set.seed(i)
          
          filename_train <- sample(unique(IIB3_df_wav$filename), 0.7 * length(unique(IIB3_df_wav$filename)))

          croc_train <- features_croc %>% filter(filename %in% filename_train)
          croc_test <- features_croc %>% filter(!(filename %in% filename_train))
          
          no_event_train <- features_no_event %>% filter(filename %in% filename_train)
          no_event_test <- features_no_event %>% filter(!(filename %in% filename_train))
          
          y_train <- as.factor(c(croc_train$event, no_event_train$event))
          x_train <- rbind.data.frame(croc_train[, 5:20], no_event_train[, 5:20])
          
          y_test <- as.factor((c(croc_test$event, no_event_test$event)))
          x_test <- rbind.data.frame(croc_test[, 5:20], no_event_test[, 5:20])
          
          #Model
          model <- randomForest( y_train~ .,
                                 data = x_train,
                                 ntree = 40,
                                 mtry = 4,
                                 importance = TRUE)
          
          pred_test <-  predict(model, newdata = x_test)
          ERROR <- ERROR + get.error(y_test, pred_test)
          SENS <- SENS + get.sensitivity(y_test,pred_test)
          SPEC <- SPEC + get.specificity(y_test,pred_test)
          CONFU_0 <- CONFU_0 + model$confusion[1,3]
          CONFU_1 <- CONFU_1 + model$confusion[2,3]
        }
        IIB3_df_ERROR_ovl <- IIB3_df_ERROR_ovl  %>% add_row(
          expansion = expansion,
          size = size,
          ovl_croc = ovl_break,
          ovl_no_event = ovl_no_event,
          Error = ERROR/repet,
          Sens = SENS /repet,
          Spec = SPEC/repet, 
          class_error_0 = CONFU_0/repet,
          class_error_1 = CONFU_1/repet)
      }
    }
  }
  remove(features_croc, features_no_event,
         croc_train, croc_test,
         no_event_train, no_event_test,
         y_train, x_train, y_test, x_test)
}
IIB3_df_ERROR_ovl <- as.data.frame(IIB3_df_ERROR_ovl)
## DATA SET CONSTRUCTION ON PREDICTION QUALITY : CAT ----
## Goal : Assessing the quality of prediction on an unknown cat
## Input : df_wav (list of labeled events).
##        You need to execute functions in the part FEATURES.
## Output : Error, specificity and sensibility of the model for each cat.

# Library
require(dplyr)
require(randomForest)
require(tidyr)

# Function
get.error <- function(class,pred){
  cont.tab <- table(class,pred)
  return((cont.tab[2,1]+cont.tab[1,2])/(sum(cont.tab)))
}

# Parameters
expansion <- 0.6
size <- 0.2
ovl_croc <- 0.7
ovl_no_event <- 0

#Initialisation
IIB3_Error_cat <- tibble(
  chat = character(),
  
  ERROR_glob = numeric(),
  class_1_glob = numeric(),
  class_0_glob = numeric(),
  
  ERROR_chat = numeric(),
  class_1_chat = numeric(),
  class_0_chat = numeric(),
  
  d_ERROR = numeric(),
  d_class_1 = numeric(),
  d_class_0 = numeric(),
  
  expansion = numeric(),
  size = numeric(),
  ovl_croc = numeric(),
  ovl_no_event = numeric()
)

# Features
feature_croc <- give_breaks(frame_size = size, ovlp_frame = ovl_break,
                             percent_expansion = expansion,
                             wav_path = "data/wav/", data = IIB3_df_wav)
feature_no_event <- give_no_event(frame_size = size, ovlp_frame = ovl_no_event,
                                   wav_path = "data/wav/", data = IIB3_df_wav)

for (cat in unique(IIB3_df_wav$chat)){
  print(cat)
  
  indice_chat <- grep(pattern = cat, x = IIB3_df_wav$chat)
  indice_croc <- grep(pattern = cat, x = feature_croc$filename)
  indice_event <- grep(pattern = cat, x = feature_no_event$filename)
  df_reduce <- IIB3_df_wav[-indice_chat,] #dataset without one cat
  
  ERROR_glob <-0
  CONFU_0_glob <- 0
  CONFU_1_glob <- 0
  
  ERROR_chat <- 0
  CONFU_0_chat <- 0
  CONFU_1_chat <- 0
  
  for (i in 1:10){
    set.seed(i)
    
    filename_train <- sample(unique(df_reduce$filename), 0.7 * length(unique(df_reduce$filename)))
    
    croc_train <- feature_croc %>% filter(filename %in% filename_train)
    croc_test <- feature_croc %>% filter(!(filename %in% filename_train))
    
    no_event_train <- feature_no_event %>% filter(filename %in% filename_train)
    no_event_test <- feature_no_event %>% filter(!(filename %in% filename_train))
    
    y_train <- as.factor(c(croc_train$event, no_event_train$event))
    x_train <- rbind.data.frame(croc_train[, 5:20], no_event_train[, 5:20])
    
    y_test <- as.factor((c(croc_test$event, no_event_test$event)))
    x_test <- rbind.data.frame(croc_test[, 5:20], no_event_test[, 5:20])
    
    y_test_chat <- as.factor(c(feature_croc[indice_croc,"event"], feature_no_event[indice_event,"event"]))
    x_test_chat <- rbind.data.frame(feature_croc[indice_croc, 5:20], feature_no_event[indice_event, 5:20])
    
    #Model
    model <- randomForest( y_train~ .,
                           data = x_train,
                           ntree = 40,
                           mtry = 4,
                           importance = TRUE)
    
    pred_test <-  predict(model, newdata = x_test)
    ERROR_glob <- ERROR_glob + get.error(y_test, pred_test)
    CONFU_0_glob <- CONFU_0_glob + model$confusion[1,3]
    CONFU_1_glob <- CONFU_1_glob + model$confusion[2,3]
    
    
    pred_test_chat <-  predict(model, newdata = x_test_chat)
    matrix_chat <- table(pred_test_chat, y_test_chat)
    ERROR_chat <- ERROR_chat + (matrix_chat[2] + matrix_chat[3]) / sum(matrix_chat)
    CONFU_0_chat <- CONFU_0_chat + (matrix_chat[2] / sum(matrix_chat[,1]))
    CONFU_1_chat <- CONFU_1_chat + (matrix_chat[3] / sum(matrix_chat[,2]))
  }
  
  IIB3_Error_cat <- IIB3_Error_cat %>% add_row(
    chat = cat,
    
    ERROR_glob = ERROR_glob/10,
    class_1_glob = CONFU_1_glob/10,
    class_0_glob = CONFU_0_glob/10,
    
    ERROR_chat = ERROR_chat/10,
    class_1_chat = CONFU_1_chat/10,
    class_0_chat = CONFU_0_chat/10,
    
    d_ERROR =  ERROR_chat/10 - ERROR_glob/10,
    d_class_1 = CONFU_1_chat/10 - CONFU_1_glob/10,
    d_class_0 = CONFU_0_chat/10 - CONFU_0_glob/10,
    
    expansion = expansion,
    size = size,
    ovl_croc = ovl_croc,
    ovl_no_event = ovl_no_event
  )
}
remove(feature_croc, feature_no_event,
       croc_train, croc_test,
       no_event_train, no_event_test,
       y_train, x_train, y_test, x_test)
IIB3_Error_cat <- as.data.frame(IIB3_Error_cat)

IIB3_Error_cat

## DATA SET CONSTRUCTION ON PREDICTION QUALITY : KIBBLE ----
## Goal : Assessing the quality of prediction on an unknown kibble
## Input : df_wav (list of labeled events).
##        You need to execute functions in the part FEATURES.
## Output : Error, specificity and sensibility of the model for each kibble.

# Library
require(dplyr)
require(randomForest)
require(tidyr)

# Function
get.error <- function(class,pred){
  cont.tab <- table(class,pred)
  return((cont.tab[2,1]+cont.tab[1,2])/(sum(cont.tab)))
}

#Parameters
expansion <- 0.6
size <- 0.2
ovl_croc <- 0.7
ovl_no_event <- 0

#Initialisation
IIB3_Error_kibble <- tibble(
  kibble = character(),
  
  ERROR_glob = numeric(),
  class_1_glob = numeric(),
  class_0_glob = numeric(),
  
  IIB3_Error_kibble = numeric(),
  class_1_kibble = numeric(),
  class_0_kibble = numeric(),
  
  d_ERROR = numeric(),
  d_class_1 = numeric(),
  d_class_0 = numeric(),
  
  expansion = numeric(),
  size = numeric(),
  ovl_croc = numeric(),
  ovl_no_event = numeric()
)

#Features
feature_croc <- give_breaks(frame_size = size, ovlp_frame = ovl_break,
                            percent_expansion = expansion,
                            wav_path = "data/wav/", data = IIB3_df_wav)
feature_no_event <- give_no_event(frame_size = size, ovlp_frame = ovl_no_event,
                                  wav_path = "data/wav/", data = IIB3_df_wav)

for (kibble in unique(IIB3_df_wav$kibble)){
  print(kibble)
  
  indice_kibble <- grep(pattern = kibble, x = IIB3_df_wav$kibble)
  indice_croc <- grep(pattern = kibble, x = feature_croc$filename)
  indice_event <- grep(pattern = kibble, x = feature_no_event$filename)
  df_reduce <- IIB3_df_wav[-indice_kibble,]
  
  ERROR_glob <-0
  CONFU_0_glob <- 0
  CONFU_1_glob <- 0
  
  IIB3_Error_kibble <- 0
  CONFU_0_kibble <- 0
  CONFU_1_kibble <- 0
  
  for (i in 1:10){
    set.seed(i)
    
    filename_train <- sample(unique(df_reduce$filename), 0.7 * length(unique(df_reduce$filename)))
    
    croc_train <- feature_croc %>% filter(filename %in% filename_train)
    croc_test <- feature_croc %>% filter(!(filename %in% filename_train))
    
    no_event_train <- feature_no_event %>% filter(filename %in% filename_train)
    no_event_test <- feature_no_event %>% filter(!(filename %in% filename_train))
    
    y_train <- as.factor(c(croc_train$event, no_event_train$event))
    x_train <- rbind.data.frame(croc_train[, 5:20], no_event_train[, 5:20])
    
    y_test <- as.factor((c(croc_test$event, no_event_test$event)))
    x_test <- rbind.data.frame(croc_test[, 5:20], no_event_test[, 5:20])
    
    y_test_kibble <- as.factor(c(feature_croc[indice_croc,"event"], feature_no_event[indice_event,"event"]))
    x_test_kibble <- rbind.data.frame(feature_croc[indice_croc, 5:20], feature_no_event[indice_event, 5:20])
    
    #Model
    model <- randomForest( y_train~ .,
                           data = x_train,
                           ntree = 40,
                           mtry = 4,
                           importance = TRUE)
    
    pred_test <-  predict(model, newdata = x_test)
    ERROR_glob <- ERROR_glob + get.error(y_test, pred_test)
    CONFU_0_glob <- CONFU_0_glob + model$confusion[1,3]
    CONFU_1_glob <- CONFU_1_glob + model$confusion[2,3]
    
    
    pred_test_kibble <-  predict(model, newdata = x_test_kibble)
    matrix_kibble <- table(pred_test_kibble, y_test_kibble)
    ERROR_kibble <- ERROR_kibble + (matrix_kibble[2] + matrix_kibble[3]) / sum(matrix_kibble)
    CONFU_0_kibble <- CONFU_0_kibble + (matrix_kibble[2] / sum(matrix_kibble[,1]))
    CONFU_1_kibble <- CONFU_1_kibble + (matrix_kibble[3] / sum(matrix_kibble[,2]))
  }
  
  IIB3_Error_kibble <- IIB3_Error_kibble %>% add_row(
    kibble = kibble,
    
    ERROR_glob = ERROR_glob/10,
    class_1_glob = CONFU_1_glob/10,
    class_0_glob = CONFU_0_glob/10,
    
    ERROR_kibble = ERROR_kibble/10,
    class_1_kibble = CONFU_1_kibble/10,
    class_0_kibble = CONFU_0_kibble/10,
    
    d_ERROR =  ERROR_kibble/10 - ERROR_glob/10,
    d_class_1 = CONFU_1_kibble/10 - CONFU_1_glob/10,
    d_class_0 = CONFU_0_kibble/10 - CONFU_0_glob/10,
    
    expansion = expansion,
    size = size,
    ovl_croc = ovl_croc,
    ovl_no_event = ovl_no_event
  )
}
remove(feature_croc, feature_no_event,
       croc_train, croc_test,
       no_event_train, no_event_test,
       y_train, x_train, y_test, x_test)
Error_kibble <- as.data.frame(Error_kibble)

Error_kibble

## VISUALISATION ----

# Data (If you have not performed the previous parts)
#IIB3_df_ERROR_ovl <- read.table(file = "data/error/IIB3_df_ERROR_ovl.txt")
#IIB3_Error_cat <- read.table(file = "data/error/IIB3_Error_cat.txt")
#IIB3_Error_kibble <- read.table(file = "data/error/IIB3_Error_kibble.txt")

# Plot
require(ggplot2)
require(dplyr)
require(tidyr)
require(akima)
require(rgl)
require(lazyeval)
require(plotly)
  # Cat
IIB3_Error_cat %>% 
  select(chat, ERROR_glob, ERROR_chat) %>% 
  gather("ERROR", "value", -chat) %>% 
  ggplot(aes(x= as.factor(chat), y = value, fill = ERROR)) +
  geom_bar(stat="identity", position=position_dodge()) +
  ggtitle("Loss of prediction quality on an unknown cat") +
  xlab("")+
  ylab("")+
  theme_bw()

IIB3_Error_cat %>% 
  select(chat, class_1_glob, class_1_chat) %>% 
  gather("CLASS_1", "value", -chat) %>% 
  ggplot(aes(x= as.factor(chat), y = value, fill = CLASS_1)) +
  geom_bar(stat="identity", position=position_dodge()) +
  ggtitle("Loss of prediction quality on an unknown cat") +
  xlab("")+
  ylab("Error Class 1")+
  theme_minimal()

  # Kibble
IIB3_Error_kibble %>% 
  select(kibble, ERROR_glob, ERROR_kibble) %>% 
  gather("ERROR", "value", -kibble) %>% 
  ggplot(aes(x= as.factor(kibble), y = value, fill = ERROR)) +
  geom_bar(stat="identity", position=position_dodge()) +
  ggtitle("Quality prediction of an unknown kibble (Global Error)") +
  xlab("")+
  ylab("Global Error")+
  theme_minimal()

IIB3_Error_kibble %>% 
  select(kibble, class_1_glob, class_1_kibble) %>% 
  gather("CLASS_1", "value", -kibble) %>% 
  ggplot(aes(x= as.factor(kibble), y = value, fill = CLASS_1)) +
  geom_bar(stat="identity", position=position_dodge()) +
  ggtitle("Quality prediction of an unknown kibble (Error on break class)") +
  xlab("")+
  ylab("Error Class 1")+
  theme_minimal()

IIB3_Error_kibble %>% 
  select(kibble, class_0_glob, class_0_kibble) %>% 
  gather("CLASS_0", "value", -kibble) %>% 
  ggplot(aes(x= as.factor(kibble), y = value, fill = CLASS_0)) +
  geom_bar(stat="identity", position=position_dodge()) +
  ggtitle("Quality prediction of an unknown kibble (Error on background noise class)") +
  xlab("")+
  ylab("Error Class 0")+
  theme_minimal()

  # Parameters
#Class_error_0 et class_error_1
IIB3_df_ERROR_ovl[which.min(IIB3_df_ERROR_ovl$class_error_0), ]
IIB3_df_ERROR_ovl[which.min(IIB3_df_ERROR_ovl$class_error_1), ]

ggplot(IIB3_df_ERROR_ovl, aes(x = class_error_1)) +
  geom_histogram(color="black", fill="white", aes(y=..density..), bins = 30) +
  geom_density(alpha=.2, fill="darkgreen") +
  xlim(0,1)+
  theme_bw()

ggplot(IIB3_df_ERROR_ovl, aes(x = class_error_0)) +
  geom_histogram(color="black", fill="white", aes(y=..density..), bins = 30) +
  geom_density(alpha=.2, fill="#FF6666") +
  xlim(0,1)+
  theme_bw()

#Size x Ovl

plot_ly(IIB3_df_ERROR_ovl, x = ~size, y = ~ovl_croc, z = ~Error)

x <- IIB3_df_ERROR_ovl$size
y <- IIB3_df_ERROR_ovl$ovl_croc
z <- IIB3_df_ERROR_ovl$class_error_0

plot_ly(x = x, y = y, z = z, type = "contour") %>% 
  layout(title = "Class Error 0", xaxis = list(title = "Frame Size"), yaxis = list(title = "Overlap"))

x <- IIB3_df_ERROR_ovl$size
y <- IIB3_df_ERROR_ovl$ovl_croc
z <- IIB3_df_ERROR_ovl$class_error_1

plot_ly(x = x, y = y, z = z, type = "contour",
        contours = list(
          start = 0,
          end = 1,
          size = 0.05
        )) %>% 
  layout(title = "Class Error 1", xaxis = list(title = "Window Length"), yaxis = list(title = "Overlap"))

#Size x expansion
x <- IIB3_df_ERROR_ovl$size
y <- IIB3_df_ERROR_ovl$expansion
z <- IIB3_df_ERROR_ovl$class_error_0

plot_ly(x = x, y = y, z = z, type = "contour") %>% 
  layout(title = "Class Error 0", xaxis = list(title = "Window Length"), yaxis = list(title = "Expansion"))

x <- IIB3_df_ERROR_ovl$size
y <- IIB3_df_ERROR_ovl$expansion
z <- IIB3_df_ERROR_ovl$class_error_1

plot_ly(x = x, y = y, z = z, type = "contour",
        contours = list(
          start = 0,
          end = 1,
          size = 0.05
        )) %>% 
  layout(title = "Class Error 1", xaxis = list(title = "Window Length"), yaxis = list(title = "Expansion"))

# Ovl x expansion
x <- IIB3_df_ERROR_ovl$expansion
y <- IIB3_df_ERROR_ovl$ovl_croc
z <- IIB3_df_ERROR_ovl$class_error_0

plot_ly(x = x, y = y, z = z, type = "contour")  %>% 
  layout(title = "Class Error 0", xaxis = list(title = "Expansion"), yaxis = list(title = "Overlap"))

x <- IIB3_df_ERROR_ovl$expansion
y <- IIB3_df_ERROR_ovl$ovl_croc
z <- IIB3_df_ERROR_ovl$class_error_1

plot_ly(x = x, y = y, z = z, type = "contour",
        contours = list(
          start = 0,
          end = 1,
          size = 0.05
        )) %>% 
  layout(title = "Class Error 1", xaxis = list(title = "Expansion"), yaxis = list(title = "Overlap"))
