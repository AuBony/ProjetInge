# PROJET INGENIEUR
# Identification des évènements Croc VS mach et Event VS NoEvent
# Audrey Bony
# 12/01/2021

############# DATASET #############

df_feature <- read.table("data/data_perso/features/df_feature_01_12.txt")

# MAJ du DATASET ----
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

#Algorithm
getwd()
wav_path <- "ProjetInge/cleanwav/"

require(seewave)
df_feature <- init_df_feature()

for (i in 1:nrow(df_wav)){
  df_feature <- give_feature(df_wav[i,])
  if (df_wav$annotation[i] == "croc") {
    df_feature <- give_feature(df_wav[i,], shift = 0.1)
    df_feature <- give_feature(df_wav[i,], shift = - 0.1)
  }
}

#write.table(df_feature, file = "data/data_perso/features/df_feature_01_12_(2).txt")

#DATA TRAIN et DATA TEST Croc VS Mach ----
library(dplyr)
croc <- df_feature %>% filter(annotation == "croc")
mach <- df_feature %>% filter(annotation == "mach")

  #Train
train_index_c <- sample(1:nrow(croc), 0.8 * nrow(croc))
train_index_m <- sample(1:nrow(mach), 0.70 * nrow(mach))

x_train_c <- df_feature[train_index_c, c(-1:-3)]
y_train_c <- df_feature[train_index_c, "annotation"]
x_train_m <- df_feature[train_index_m, c(-1:-3)]
y_train_m <- df_feature[train_index_m, "annotation"]
y_train <- c(y_train_c$annotation, y_train_m$annotation)
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
test <- rbind.data.frame(cbind.data.frame(x_test_c, y_test_c),
                          cbind.data.frame(x_test_m, y_test_m),
                          deparse.level = 1)
test_tot <- rbind.data.frame(cbind.data.frame(x_test_c, df_feature[-train_index_c, c(1:3)]),
                              cbind.data.frame(x_test_m, df_feature[-train_index_m, c(1:3)]),
                              deparse.level = 1)

#DATA TRAIN et DATA TEST Event VS NoEVent ----


############# CROC VS MACH #############
library(randomForest)
Rf <- randomForest( y_train ~ .,
                    data = x_train,
                    ntree = 50,
                    na.action = na.omit,
                    importance = TRUE)
plot(model.50)

pred.test.rf.50 <- predict(model.50, newdata = data.test[,4:20])
CM.rf.50 <- table(data.test$annotation, pred.test.rf.50)
CM.rf.50
############# Event VS NoEvent #############