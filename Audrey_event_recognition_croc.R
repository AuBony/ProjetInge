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
                                          trim_ws = TRUE)))


vec_noms <- c()
start <- c()
end <- c()
annotation <- c()
for (k in 1:nrow(data)){
  vec_noms <- c(vec_noms, rep(data[[1]][[k]], nrow(data[[2]][[k]])))
  start <- c(start, data[[2]][[k]][[1]])
  end <- c(end, data[[2]][[k]][[2]])
  annotation <- c(annotation, data[[2]][[k]][[3]])
}

data_modif <- data.frame(filename = vec_noms, start = start, 
                         end = end, annotation = annotation)

data_modif_chat_kibble_duration <- data_modif %>% 
  mutate(chat = as.character(map(strsplit(data_modif$filename, "_"), 1)), 
         kibble = as.character(map(strsplit(data_modif$filename, "_"), 2)),
         duration = end-start)
df_txt <- cbind.data.frame(data_frame(id = seq(1, nrow(data_modif_chat_kibble_duration))), data_modif_chat_kibble_duration)
df_wav <- df_txt
df_wav$filename <- str_replace(df_txt$filename, ".txt", ".wav")
df_wav
df_wav_c <- df_wav %>% filter(annotation == "croc")

#DATA_FEATURE ----
#library
library(soundgen)
library(tuneR)
library(seewave)

# TRAIN CROC
#

give_croc <- function(data = df_wav, frame_size = 0.1, ovlp_frame = 0, percent_expansion = 0, wav_path = "ProjetInge/cleanwav/" ){
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
  for (audio in unique(data[data$annotation == "croc", "filename"])){
    
    cat(".")
    
    crocs <- data %>% filter(filename ==  audio)

      
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


give_croc()

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

give_no_event <- function(data = df_wav, frame_size = 0.1, ovlp_frame = 0, wav_path = "ProjetInge/cleanwav/"){
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
  for (audio in unique(data$filename)){
    
    cat("_")
    
    no_event <- data %>% filter(filename ==  audio, annotation == "croc") #liste des crocs dans un enregistrement
  
    #Il n'y a pas de croc dans l'enregistrement
    if (dim(no_event)[1] == 0){
      deb <- 0
      audio_wav <- readWave(paste0(wav_path, audio), units = "seconds")
      fin <- round(length(audio_wav@left) / audio_wav@samp.rate, 2)
      
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
    
      
    #Il y a bien un croc dans l'enregistrement
    }else{
      
      for (l_no_event in 1: nrow(no_event)){
        #Deb et fin
        if (l_no_event == 1){
          deb <- 0
          fin <- no_event$start[1]
        }
        
        if (l_no_event >1){
          deb <- no_event$end[l_no_event - 1]
          fin <- no_event$start[l_no_event]
        }
        
        if (fin - deb > frame_size){
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
        }
      }
    }
  }
  return(as.data.frame(df_feature_no_event))
}

#Execution
no_event<- give_no_event(data = df_wav_c)


# DATASET COMPLET CROC ET NO EVENT
df_wav_c <- df_wav %>% filter(annotation == "croc")
filename_train <- sample(unique(df_wav_c$filename), 0.7 * length(unique(df_wav_c$filename)))

df_train <- df_wav_c %>% filter(filename %in% filename_train)
df_test <- df_wav_c %>%  filter(!(filename %in% filename_train))

croc_train <- give_croc(data = df_train)
no_event_train <- give_no_event(data = df_train)

croc_test <- give_croc(data = df_test)
no_event_test <- give_no_event(data = df_test)


y_train <- as.factor(c(croc_train$event, no_event_train$event))
x_train <- rbind.data.frame(croc_train[, 5:20], no_event_train[, 5:20])

y_test <- as.factor((c(croc_test$event, no_event_test$event)))
x_test <- rbind.data.frame(croc_test[, 5:20], no_event_test[, 5:20])

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

  #Model ----
require(randomForest)

RF <- randomForest::randomForest(
  y_train ~ .,
  data = x_train,
  ntree = 40, 
  mtry = 4, 
  x_test = x_test,
  y_test = y_test,
  importance = TRUE)

RF

varImpPlot(RF)
RF$confusion
RF$importance



require(ROCR)
pred_RF <- predict(RF, newdata = x_test, type = "prob")
pred_class <-  prediction(pred_RF[,2], y_test)
pred.test <- predict(RF, newdata = x_test)
performance_RF <- performance(pred_class,measure = "tpr",x.measure= "fpr")
plot(performance_RF, col = 4, lwd = 2)
abline(0,1)

get.error(y_test, pred.test)



# TEST DES PARAMETRES D'ECHANTILLONNAGE ----

#Package
require(randomForest)
require(dplyr)

#Plage des paramètres
plage_size <- c(0.1, 0.2, 0.1)
plage_ovl <- c(0, 0.75, 0.25)
plage_exp <- c(0, 0.2, 0.1)

#Initialisation
df_wav_c <- df_wav %>% filter(annotation == "croc")
df_ERROR_p <- tibble(
  expansion = numeric(),
  size = numeric(),
  ovl = numeric(),
  Error = numeric (),
  Sens = numeric(),
  Spec = numeric(),
  class_error_0 = numeric(),
  class_error_1 = numeric())
tour <- 0
total <- length(seq(from = plage_size[1], to = plage_size[2], by = plage_size[3] )) *
  length(seq(from = plage_ovl[1], to = plage_ovl[2], by = plage_ovl[3] )) * 
  length(seq(from = plage_exp[1], to = plage_exp[2], by = plage_exp[3] ))


#frame size
for (size in seq(from = plage_size[1], to = plage_size[2], by = plage_size[3] )){
  #Overlap
  for (ovl in seq(from = plage_ovl[1], to = plage_ovl[2], by = plage_ovl[3] )){
    #Expansion
    for (expansion in seq(from = plage_exp[1], to = plage_exp[2], by = plage_exp[3] )){
      tour <- tour + 1
      print(paste0(tour, "/", total))
      
      ERROR <-0
      SENS <- 0
      SPEC <- 0
      CONFU_0 <- 0
      CONFU_1 <- 0
      
      features_croc <- give_croc(df_wav_c, frame_size = size, ovlp_frame = ovl, percent_expansion = expansion)
      features_no_event <- give_no_event(df_wav_c, frame_size = size, ovlp_frame = ovl)

      
      for (i in 1:5){
        set.seed(i)
        
        filename_train <- sample(unique(df_wav_c$filename), 0.7 * length(unique(df_wav_c$filename)))
        
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
      df_ERROR_p <- df_ERROR_p  %>% add_row(
        expansion = expansion,
        size = size,
        ovl = ovl,
        Error = ERROR/5,
        Sens = SENS / 5,
        Spec = SPEC/5, 
        class_error_0 = CONFU_0/5,
        class_error_1 = CONFU_1/5)
      
    }
  }
}

df_ERROR_copie 
df_ERROR_p <- as.data.frame(df_ERROR_p)
df_ERROR_p
#write.table(df_ERROR_p, file = "data/data_perso/df_ERROR_25-01_2.txt")

# RANDOM FOREST
size <- 0.1
ovl <- 0.7
expansion <- 0.2

features_croc <- give_croc(df_wav_c, frame_size = size, ovlp_frame = ovl, percent_expansion = expansion)
features_no_event <- give_no_event(df_wav_c, frame_size = size, ovlp_frame = ovl)

filename_train <- sample(unique(df_wav_c$filename), 0.7 * length(unique(df_wav_c$filename)))
croc_train <- features_croc %>% filter(filename %in% filename_train)
croc_test <- features_croc %>% filter(!(filename %in% filename_train))

no_event_train <- features_no_event %>% filter(filename %in% filename_train)
no_event_test <- features_no_event %>% filter(!(filename %in% filename_train))

y_train <- as.factor(c(croc_train$event, no_event_train$event))
x_train <- rbind.data.frame(croc_train[, 5:20], no_event_train[, 5:20])

y_test <- as.factor((c(croc_test$event, no_event_test$event)))
x_test <- rbind.data.frame(croc_test[, 5:20], no_event_test[, 5:20])
model_RF <- randomForest::randomForest(
  y_train ~ .,
  data = x_train,
  ntree = 40,
  mtry = 4,
  importance = TRUE
)

model_RF

require(ROCR)
pred_RF <- predict(RF, newdata = x_test, type = "prob")
pred_class <-  prediction(pred_RF[,2], y_test)
pred.test <- predict(RF, newdata = x_test)
performance_RF <- performance(pred_class,measure = "tpr",x.measure= "fpr")
plot(performance_RF, col = 4, lwd = 2)
abline(0,1)

# ANALYSE de df_ERROR_p ####

#FactomineR
require(Factoshiny)
Factoshiny(df_ERROR_p)
res.PCA<-PCA(df_ERROR_p,quanti.sup=c(1,2,3,4,5,6),graph=FALSE)

#Plot

require(plotly)
plot_ly(df_ERROR_p, x = ~size, y = ~ovl, z = ~Error)

require(akima)
require(rgl)

  #Size x Ovl
require(dplyr)
x <- df_ERROR_p$size
y <- df_ERROR_p$ovl
z <- df_ERROR_p$class_error_0
s=interp(x,y,z,duplicate="strip")

plot_ly(x = s$x, y = s$y, z = s$z, type = "contour") %>% 
  layout(title = "Class Error 0", xaxis = list(title = "Frame Size"), yaxis = list(title = "Overlap"))

x <- df_ERROR_p$size
y <- df_ERROR_p$ovl
z <- df_ERROR_p$class_error_1
s=interp(x,y,z,duplicate="strip")

plot_ly(x = s$x, y = s$y, z = s$z, type = "contour") %>% 
  layout(title = "Class Error 1", xaxis = list(title = "Frame Size"), yaxis = list(title = "Overlap"))

  #Size x expansion
x <- df_ERROR_p$size
y <- df_ERROR_p$expansion
z <- df_ERROR_p$class_error_0
s=interp(x,y,z,duplicate="strip")

plot_ly(x = s$x, y = s$y, z = s$z, type = "contour") %>% 
  layout(title = "Class Error 0", xaxis = list(title = "Frame Size"), yaxis = list(title = "Expansion"))

x <- df_ERROR_p$size
y <- df_ERROR_p$expansion
z <- df_ERROR_p$class_error_1
s=interp(x,y,z,duplicate="strip")

plot_ly(x = s$x, y = s$y, z = s$z, type = "contour") %>% 
  layout(title = "Class Error 1", xaxis = list(title = "Frame Size"), yaxis = list(title = "Expansion"))

  # Ovl x expansion
x <- df_ERROR_p$expansion
y <- df_ERROR_p$ovl
z <- df_ERROR_p$class_error_0
s=interp(x,y,z,duplicate="strip")

plot_ly(x = s$x, y = s$y, z = s$z, type = "contour") %>% 
  layout(title = "Class Error 0", xaxis = list(title = "Expansion"), yaxis = list(title = "Overlap"))

x <- df_ERROR_p$expansion
y <- df_ERROR_p$ovl
z <- df_ERROR_p$class_error_1
s=interp(x,y,z,duplicate="strip")

plot_ly(x = s$x, y = s$y, z = s$z, type = "contour") %>% 
  layout(title = "Class Error 1", xaxis = list(title = "Expansion"), yaxis = list(title = "Overlap"))

#
require(Factoshiny)
Factoshiny(df_ERROR_p)

res.PCA<-PCA(df_ERROR_p,quanti.sup=c(1,2,3,4,5,6),graph=FALSE)


#write.table(df_ERROR_p, file = "data/data_perso/df_ERROR_25-01.txt")
#
plot_ly(x = s$x, y = s$y, z = s$z) %>%
  add_surface(
    contours = list(
      z = list(
        show = TRUE,
        usecolormap = TRUE,
        highlightcolor = "#ff0000",
        project = list(z = TRUE)
      )
    )
  ) %>% 
  layout(
    title = "Class_ERROR",
    scene = list(
      xaxis = list(title = "Frame Size"),
      yaxis = list(title = "Overlap"),
      zaxis = list(title = "Z")
    )
  )
