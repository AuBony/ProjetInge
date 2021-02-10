# PROJET INGENIEUR
# Identification des évènements Croc VS mach et Event VS NoEvent
# Audrey Bony
# 12/01/2021

############# DATASET #############

df_feature <- read.table("data/data_perso/features/df_feature_01_12.txt")

#MAJ du DATASET ----
#DATA_WAV ----
require(readr)  # for read_csv()
require(dplyr)  # for mutate()
require(tidyr)  # for unnest()
require(purrr)  # for map(), reduce()
library(stringr) # for str_replace()

data_path <- "data/data_perso/labels/"
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

#DATA_FEATURE ----
#library
require(dplyr)
library(soundgen)
library(tuneR)
library(seewave)

#function
give_feature <- function(df_wav_line, shift = 0, df = df_feature){
  # Obtenir les features pour un événement
  # input : une ligne de df_wav(id, filename, start, end, annotation, ...)
  # output : un tableau avec les features pour chaque événement libéllé df_feature(filename, annotation, features)
  require(soundgen)
  require(tuneR)
  require(seewave)
  
  wav_file <- readWave(paste0(wav_path, df_wav_line[[2]]),
                       from = df_wav_line[[3]] + shift,
                       to = df_wav_line[[4]] + shift,
                       units = "seconds") 
  #
  sp <- specprop(seewave::spec(wav_file@left, f = wav_file@samp.rate, plot = FALSE, scaled = TRUE, norm = FALSE))
  #
  df <- df %>% add_row(id = df_wav_line$id,
                                       filename = df_wav_line$filename,
                                       annotation = df_wav_line$annotation,
                                       
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
  return(df)
}

give_feature_standard <- function(df_wav_line, shift = 0, df = df_feature, window.l = 0.8){
  wav_file <- readWave(paste0(wav_path, df_wav_line[[2]]),
                       from = df_wav_line[[3]] + shift,
                       to = df_wav_line[[3]] + window.l + shift,
                       units = "seconds") 
  #
  sp <- specprop(spec(wav_file@left, f = wav_file@samp.rate, plot = FALSE, scaled = TRUE, norm = FALSE))
  #
  df <- df %>% add_row(id = df_wav_line$id,
                       filename = df_wav_line$filename,
                       annotation = df_wav_line$annotation,
                       
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
  return(df)
}

init_df_feature <- function(){
  df <- tibble(id = numeric(),
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
               sprec = numeric())
  return(df)
}

give_feature_enhanced_df <- function(data){
  require(seewave)
  
  feature <- init_df_feature()
  for (i in 1:nrow(data)){
    feature <- give_feature(data[i,])
    if (data$annotation[i] == "croc") {
      feature <- give_feature(data[i,], shift = 0.1)
      feature <- give_feature(data[i,], shift = - 0.1)
    }
  }
}


#Algorithm
getwd()
wav_path <- "ProjetInge/cleanwav/"

df_feature <- give_feature_enhanced_df(data = df_wav)

#write.table(df_feature, file = "data/data_perso/features/df_feature_01_12_(2).txt")

#DATA TRAIN et DATA TEST Croc VS Mach ----
library(dplyr)

#df_feature <- as_tibble(read.table("data/data_perso/features/df_feature_01_12_(2).txt"))

croc <- df_feature %>% filter(annotation == "croc")
mach <- df_feature %>% filter(annotation == "mach")

  #Train
train_index_c <- sample(1:nrow(croc), 0.8 * nrow(croc))
train_index_m <- sample(1:nrow(mach), 0.70 * nrow(mach))

x_train_c <- df_feature[train_index_c, c(-1:-3)]
y_train_c <- df_feature[train_index_c, "annotation"]
x_train_m <- df_feature[train_index_m, c(-1:-3)]
y_train_m <- df_feature[train_index_m, "annotation"]
y_train <- as.factor(c(y_train_c$annotation, y_train_m$annotation))
x_train <- as.data.frame(rbind(x_train_c, x_train_m))
train <- rbind.data.frame(cbind.data.frame(x_train_c, y_train_c),
                          cbind.data.frame(x_train_m, y_train_m),
                          deparse.level = 1)
train_tot <- rbind.data.frame(cbind.data.frame(x_train_c, df_feature[train_index_c, c(1:3)]),
                              cbind.data.frame(x_train_m, df_feature[train_index_m, c(1:3)]),
                              deparse.level = 1)
  #Test  
x_test_c <- df_feature[-train_index_c, c(-1:-3)]
y_test_c <- df_feature[-train_index_c, "annotation"]
x_test_m <- df_feature[-train_index_m, c(-1:-3)]
y_test_m <- df_feature[-train_index_m, "annotation"]
y_test <- as.factor(c(y_test_c$annotation, y_test_m$annotation))
x_test <- as.data.frame(rbind(x_test_c, x_test_m))
test <- rbind.data.frame(cbind.data.frame(x_test_c, y_test_c),
                          cbind.data.frame(x_test_m, y_test_m),
                          deparse.level = 1)
test_tot <- rbind.data.frame(cbind.data.frame(x_test_c, df_feature[-train_index_c, c(1:3)]),
                              cbind.data.frame(x_test_m, df_feature[-train_index_m, c(1:3)]),
                              deparse.level = 1)

#DATA TRAIN et DATA TEST Event VS NoEVent ----

  #Function
give_classif_event <- function(window_length = 0.8, data = df_wav){
  require(tuneR)
  require(dplyr)
  
  df_classif <- tibble(filename = character(),
                       start = numeric(),
                       end = numeric(),
                       event = numeric())
                   
  #Etape 1 : Parcourir les enregistrements labellés
  for (audio_path in unique(df_wav$filename)) {
    
    audio <- readWave(paste0(wav_path, audio_path))
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


############# CROC VS MACH #############
  #Function
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

  #Random Forest
library(randomForest)
library(ROCR)
    #ntree
df_ERROR <- choose_ntree(vect = c(1:10, seq(15,100, 5)),y_train = y_train, x_train = x_train, y_test = y_test, x_test = x_test)
plot_dfERROR(df_ERROR)

    #RandomForest with the nicest ntree
Rf <- randomForest::randomForest(
                    y_train ~ .,
                    data = x_train,
                    ntree = 40, 
                    mtry = 4, 
                    x_test = x_test,
                    y_test = y_test,
                    importance = TRUE)
Rf

    #RandomForest Result
varImpPlot(Rf)
Rf$confusion
Rf$importance

    #ROC
pred_RF <- predict(Rf, newdata = x_test, type = "prob")
pred_class <-  prediction(pred_RF[,2], y_test)
pred.test <- predict(Rf, newdata = x_test)
performance_RF <- performance(pred_class,measure = "tpr",x.measure= "fpr")
plot(performance_RF, col = 4, lwd = 2)
abline(0,1)

  #Comparing classic data_feature and  enhanced data_feature

    #functions
give_train_test_1 <- function(feature, seed = 123, percent_croc = 0.8, percent_mach = 0.7){
  set.seed(seed)
  croc <- feature %>% filter(annotation == "croc")
  mach <- feature %>% filter(annotation == "mach")
  
  train_index_c <- sample(1:nrow(croc), percent_croc * nrow(croc))
  train_index_m <- sample(1:nrow(mach), percent_mach * nrow(mach))
  
  #Train
  x_train_c <- feature[train_index_c, c(-1:-3)]
  y_train_c <- feature[train_index_c, "annotation"]
  x_train_m <- feature[train_index_m, c(-1:-3)]
  y_train_m <- feature[train_index_m, "annotation"]
  y_train <- as.factor(c(y_train_c$annotation, y_train_m$annotation))
  x_train <- as.data.frame(rbind(x_train_c, x_train_m))
  train <- rbind.data.frame(cbind.data.frame(x_train_c, y_train_c),
                            cbind.data.frame(x_train_m, y_train_m),
                            deparse.level = 1)
  train_tot <- rbind.data.frame(cbind.data.frame(x_train_c, feature[train_index_c, c(1:3)]),
                                cbind.data.frame(x_train_m, feature[train_index_m, c(1:3)]),
                                deparse.level = 1)
  
  #Test  
  x_test_c <- feature[-train_index_c, c(-1:-3)]
  y_test_c <- feature[-train_index_c, "annotation"]
  x_test_m <- feature[-train_index_m, c(-1:-3)]
  y_test_m <- feature[-train_index_m, "annotation"]
  y_test <- as.factor(c(y_test_c$annotation, y_test_m$annotation))
  x_test <- as.data.frame(rbind(x_test_c, x_test_m))
  test <- rbind.data.frame(cbind.data.frame(x_test_c, y_test_c),
                           cbind.data.frame(x_test_m, y_test_m),
                           deparse.level = 1)
  test_tot <- rbind.data.frame(cbind.data.frame(x_test_c, feature[-train_index_c, c(1:3)]),
                               cbind.data.frame(x_test_m, feature[-train_index_m, c(1:3)]),
                               deparse.level = 1)
  
  return(list(y_train, x_train, y_test, x_test))
}

    #Comparing
require(dplyr)
data_feature_classic <- as_tibble(read.table("data/data_perso/features/df_feature_01_12.txt"))
data_feature_enhanced <- as_tibble(read.table("data/data_perso/features/df_feature_01_12.txt"))

data_classic <- give_train_test_1(feature = data_feature_classic, percent_croc = 0.7, percent_mach = 0.7)
y_train_classic <- data_classic[[1]]
x_train_classic <- data_classic[[2]]
y_test_classic <- data_classic[[3]]
x_test_classic <- data_classic[[4]]

data_enhanced <- give_train_test_1(feature = data_feature_enhanced)
y_train_enhanced <- data_enhanced[[1]]
x_train_enhanced <- data_enhanced[[2]]
y_test_enhanced <- data_enhanced[[3]]
x_test_enhanced <- data_enhanced[[4]]

Rf_classic <- randomForest::randomForest(
  y_train_classic ~ .,
  data = x_train_classic,
  ntree = 40, 
  mtry = 4,
  importance = TRUE)

Rf_enhanced <- randomForest::randomForest(
  y_train_enhanced ~ .,
  data = x_train_enhanced,
  ntree = 40, 
  mtry = 4,
  importance = TRUE)

Rf_classic
Rf_enhanced

plot(performance_RF_c, col = 4, lwd = 1)
plot(performance_RF_e, col = 3, lwd = 1, add = TRUE)
legend("bottomright", legend = c("Classic","Enhanced"), col = c(4,3), fill = c(4,3))

plot(1, type = "n", xlim = c(0,1), ylim = c(0,1))
abline(0,1)
for (i in 1:10) {
  data_classic <- give_train_test_1(feature = data_feature_classic, percent_croc = 0.7, percent_mach = 0.7, seed = i)
  y_train_classic <- data_classic[[1]]
  x_train_classic <- data_classic[[2]]
  y_test_classic <- data_classic[[3]]
  x_test_classic <- data_classic[[4]]
  
  data_enhanced <- give_train_test_1(feature = data_feature_enhanced, seed = i)
  y_train_enhanced <- data_enhanced[[1]]
  x_train_enhanced <- data_enhanced[[2]]
  y_test_enhanced <- data_enhanced[[3]]
  x_test_enhanced <- data_enhanced[[4]]
  
  Rf_classic <- randomForest::randomForest(
    y_train_classic ~ .,
    data = x_train_classic,
    ntree = 40, 
    mtry = 4,
    importance = TRUE)
  
  Rf_enhanced <- randomForest::randomForest(
    y_train_enhanced ~ .,
    data = x_train_enhanced,
    ntree = 40, 
    mtry = 4,
    importance = TRUE)
  
  pred_RF_c <- predict(Rf_classic, newdata = x_test_classic, type = "prob")
  pred_class_c <-  prediction(pred_RF_c[,2], y_test_classic)
  pred.test_c <- predict(Rf_classic, newdata = x_test_classic)
  performance_RF_c <- performance(pred_class_c,measure = "tpr",x.measure= "fpr")
  plot(performance_RF_c, col = 4, lwd = 1, add = TRUE)
  
  pred_RF_e <- predict(Rf_enhanced, newdata = x_test_enhanced, type = "prob")
  pred_class_e <-  prediction(pred_RF_e[,2], y_test_enhanced)
  pred.test_e <- predict(Rf_enhanced, newdata = x_test_enhanced)
  performance_RF_e <- performance(pred_class_e,measure = "tpr",x.measure= "fpr")
  plot(performance_RF_e, col = 3, lwd = 1, add = TRUE)
  
  legend("bottomright", legend = c("Classic","Enhanced"), col = c(4,3), fill = c(4,3))
}

############# Event VS NoEvent #############

#RF
library(randomForest)
library(ROCR)

  #ntree
df_ERROR <- choose_ntree(vect = seq(1, 100, by = 10),y_train = y_train_event, x_train = x_train_event, y_test = y_test_event, x_test = x_test_event)
plot_dfERROR(df_ERROR)

  # Window length
require(dplyr)
df_ERROR_window <- data.frame(window = numeric(),
                              event_1 = numeric(),
                              event_0 = numeric(),
                              Error = numeric (), 
                              Sens = numeric(),
                              Spec = numeric())
for (window in seq(0.1, 0.8, by = 0.1)){
  df_event_1 <- as.data.frame(give_classif_event(data = df_wav, window_length = window))
  df_feature_event_1 <- as.data.frame(give_feature_event(df_event = df_event_1, wav_path = "ProjetInge/cleanwav/"))
  
  ERROR <- 0
  SENS <- 0
  SPEC <- 0
  
  for (run in (1:10)){
    cat("*")
    train_index_event_1 <- sample(1:nrow(df_feature_event_1), 0.7 * nrow(df_feature_event_1))
    y_train_event_1 <- as.factor(df_feature_event[train_index_event_1, "event"])
    x_train_event_1 <- df_feature_event_1[train_index_event_1, 5:21]
    train_event_1 <- cbind.data.frame(x_train_event_1,
                                      y_train_event_1,
                                      deparse.level = 1)
    #Test  
    y_test_event_1 <- as.factor(df_feature_event_1[-train_index_event_1, "event"])
    x_test_event_1 <- as.data.frame(df_feature_event_1[-train_index_event_1, 5:21])
    test_event_1 <- cbind.data.frame(x_train_event_1,
                                     y_train_event_1,
                                     deparse.level = 1)
    
    model_event_1 <- randomForest::randomForest(y_train_event_1 ~ ., data = x_train_event_1,
                                                ntree = 40,
                                                importance = TRUE)
    
    
    pred_test_1 <-  predict(model_event_1, newdata = x_test_event_1)
    ERROR <- ERROR + get.error(y_test_event_1, pred_test_1)
    SENS <- SENS + get.sensitivity(y_test_event_1,pred_test_1)
    SPEC <- SPEC + get.specificity(y_test_event_1,pred_test_1)
  }

  df_ERROR_window <- df_ERROR_window %>% add_row(window = window,
                                                 event_1 = nrow(df_event_1[df_event_1$event == "1",]),
                                                 event_0 = nrow(df_event_1[df_event_1$event == "0",]),
                                                 Error = ERROR / 10,
                                                 Sens = SENS / 10,
                                                 Spec = SPEC / 10)
  cat("\n")
  }

plot_dfERROR_window <- function(df_ERROR){
  plot(df_ERROR$window, df_ERROR$Sens, col = "orange", lwd = 2, type = 'l', ylim = c(0,1), xlab = "window length", ylab = "", main = "Event Detection")
  lines(df_ERROR$window, df_ERROR$Spec, col = 'green4', lwd = 2)
  lines(df_ERROR$window, df_ERROR$Error, col = 'red', lwd = 2)
  legend("right",legend = c("ERROR", "Specificity", "Sensitivity"), col = c('red', 'green4', 'orange'), fill =  c('red', 'green4', 'orange'))
}

df_ERROR_window
plot_dfERROR_window(df_ERROR_window)

df_ggplot_event  
require(tidyr)
require(ggplot2)
df_ERROR_window %>% select(window, event_1, event_0) %>%  gather(event, value, event_0:event_1) %>% 
ggplot(aes(x = window, y = value, fill = event)) +
  geom_bar(stat = 'identity', position = "dodge") +
  scale_fill_brewer(palette = "Set1") +
  ggtitle("Déséquilibre des données en fonction de la taille de la fenêtre") +
  ylab("") +
  theme_bw()+
  theme(plot.title = element_text(size=20, face="bold"))

  #Model
model_event <- randomForest::randomForest(y_train_event ~ ., data = x_train_event,
                                          ntree = 40,
                                          importance = TRUE)
plot(model_event)
model_event
varImpPlot(model_event)
model_event$confusion

