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
library(soundgen)
library(tuneR)
library(seewave)

#function
give_feature <- function(df_wav_line, shift = 0, df = df_feature){
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
head(df_wav)
file_wav_path <- "ProjetInge/cleanwav/"
df_wav_line <- df_wav[3,]
require(tuneR)
wav_file <- readWave(paste0(file_wav_path, df_wav_line[[2]]), units = "seconds") 

head(df_wav)

give_classif_event <- function(window_length = 0.8){
  df_classif <- tibble(filename = character(),
                       start = numeric(),
                       end = numeric(),
                       event = numeric())
                   
  #Etape 1 : Parcourir les enregistrements labellés
  for (audio_path in unique(df_wav$filename)) {
    
    audio <- readWave(paste0(file_wav_path, audio_path))
    duration <- round(length(audio@left) / audio@samp.rate, 2)
    #Etape 2 : Déplacement dans un enregistrement par frame
    for (moment in seq(0, duration - window_length, by = window_length)){
      
      #Etape 3 : Définir si la frame est un event (1) ou non (0)
      df_classif <- df_classif %>% add_row(filename = audio_path,
                                           start = moment,
                                           end = moment + window_length,
                                           event = dim(df_wav %>%  filter(filename == audio_path, start < (moment + window_length) & start > moment))[1]
                                             )
    }
    
  }
}

df_wav[df_wav$filename == audio_path, "start"]
df_wav %>%  filter(filename == audio_path, start < (moment + window_length) & start > moment)
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
