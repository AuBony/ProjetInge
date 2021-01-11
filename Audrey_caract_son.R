# PROJET INGENIEUR
# Caractérisation des sons
# Audrey Bony
# 4/01/2021


############# LABEL #############

# Solution dplyr (source : https://clauswilke.com/blog/2016/06/13/reading-and-combining-many-tidy-data-files-in-r/)----
require(readr)  # for read_csv()
require(dplyr)  # for mutate()
require(tidyr)  # for unnest()
require(purrr)  # for map(), reduce()
library(stringr) # for str_replace()

# Files
data_path <- "data/data_perso/labels/"
files <- dir(data_path, pattern = "*.txt")
files

# data with start, end and filename
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
df_wav <- df_txt
df_wav$filename <- str_replace(df_txt$filename, ".txt", ".wav")

#Retirer prune_B_1
df_wav <- df_wav[df_wav$filename != "prune_B_1.wav",]
# Solution sans le nom des fichiers ----
multmerge <- function(mypath = getwd()){
  require(dplyr)
  require(readr)
  dataset <- list.files(path=mypath,
                        full.names=TRUE, 
                        pattern="*.txt") %>% 
    lapply(read_delim, 
           delim="\t", 
           escape_double = FALSE, 
           col_names = c("start", "end", "annotation"), 
           trim_ws = TRUE) %>% 
    bind_rows()
  dataset
}

df_part <- multmerge("../data/data_perso/labels/")
df_part



############# DESCRIPTION #############
# DESCRIPTION  ----
library(ggplot2)

#Nombre de crocs et de mach
table(df_wav$annotation)
table(df_wav$annotation, df_wav$chat)
table(df_wav$annotation, df_wav$kibble)

#Comparer durée des crocs et des mach
ggplot(df_wav, aes(x=annotation, y=duration, fill=annotation)) +
  geom_violin() +
  ggtitle("Durée des crocs et des mastications")

df %>% filter(annotation == "croc") %>% 
  ggplot(aes(x = duration)) +
  geom_density(fill = "#69b3a2", color = "#69b3a2", alpha = 0.8) +
  ggtitle("Durée des crocs")

df %>% filter(annotation == "mach") %>% 
  ggplot(aes(x = duration)) +
  geom_density(fill = "#69b3a2", color = "#69b3a2", alpha = 0.8) +
  ggtitle("Durée des mastications")

#
hist(df_wav$duration,breaks =  100)
density(df$duration)

ggplot(df_wav, aes(x = duration)) +
  geom_density(fill = "#69b3a2", color = "#69b3a2", alpha = 0.8) +
  ggtitle("Durée des sons labellisés")

ggplot(df_wav, aes(x = duration, fill = chat, cut = chat)) +
  geom_density(adjust = 1.5, alpha = 0.8) +
  ggtitle("Durée des sons labellisés par chat")

df_wav %>% filter(annotation == "mach") %>% 
  ggplot(aes(x = duration, fill = kibble, cut = kibble)) +
  geom_density(adjust = 1.5, alpha = 0.8) +
  ggtitle("Durée des mastications par friandise")

df_wav %>% filter(annotation == "mach") %>% 
  ggplot(aes(x = duration, fill = chat, cut = chat)) +
  geom_density(adjust = 1.5, alpha = 0.8) +
  ggtitle("Durée des mastications par chat")

df_wav %>% filter(annotation == "croc") %>% 
  ggplot(aes(x = duration, fill = chat, cut = chat)) +
  geom_density(adjust = 1.5, alpha = 0.8) +
  ggtitle("Durée des crocs par chat")


############# MODEL #############
## FEATURES ----
library(soundgen)
library(tuneR)
library(seewave)

# Selectionner 
getwd()
wav_path <- "ProjetInge/cleanwav/"


require(seewave)
df_feature <- tibble(id = numeric(),
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
                     sprec = numeric()
                     )

for (i in 1:nrow(df_wav)){
  wav_file <- readWave(paste0(wav_path, df_wav[i,2]),
                       from = df_wav[i,3],
                       to = df_wav[i,4],
                       units = "seconds") 
  #
  sp <- specprop(spec(wav_file@left, f = wav_file@samp.rate, plot = FALSE, scaled = TRUE, norm = FALSE))
  #
  df_feature <- df_feature %>% add_row(id = df_wav$id[i],
                          filename = df_wav$filename[i],
                          annotation = df_wav$annotation[i],
                          
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

#write.table(df_feature, file = "data/data_perso/features/df_feature_01_07.txt")

a <- readWave("ProjetInge/cleanwav/cathy_A_1.wav", from = df_wav[3,3], to = df_wav[3,4],
              units = "seconds")
#Temporal Entropy
th(env(a, plot = FALSE))

#Dominant frequency of a time wave
max(dfreq(a, plot = FALSE)[,2])
mean(dfreq(a, plot = FALSE)[,2])

#Spectral centroid
#source : https://cran.r-project.org/web/packages/seewave/seewave.pdf at specprop
sp <- specprop(spec(a@left, f = a@samp.rate, plot = FALSE, scaled = TRUE, norm = FALSE))

## VARIABLE SELECTION ----
library(FactoMineR)
library(Factoshiny)

Factoshiny(df_feature)

## VIZUALISATION ----
library(plotly)
plot(df_feature$meandfreq, df_feature$maxdfreq, col = as.factor(df_feature$annotation))

plot_ly(df_feature,
        x = ~maxdfreq,
        y = ~meandfreq,
        z = ~th,
        color = ~annotation,
        colors = c("#383ED9", "#FADA23"),
        marker = list(symbol = "circle", sizemode = 'diameter'))

df_feature %>%  
  mutate(chat = as.character(map(strsplit(df_feature$filename, "_"), 1)), 
                       kibble = as.character(map(strsplit(df_feature$filename, "_"), 2))) %>% 
  plot_ly(
        x = ~maxdfreq,
        y = ~meandfreq,
        z = ~th,
        color = ~kibble,
        marker = list(symbol = "circle", sizemode = 'diameter'))

df_feature %>%  
  mutate(chat = as.character(map(strsplit(df_feature$filename, "_"), 1)), 
         kibble = as.character(map(strsplit(df_feature$filename, "_"), 2))) %>% 
  plot_ly(
    x = ~maxdfreq,
    y = ~meandfreq,
    z = ~th,
    color = ~chat,
    marker = list(symbol = "circle", sizemode = 'diameter'))

df_feature %>%  
  mutate(chat = as.character(map(strsplit(df_feature$filename, "_"), 1)), 
         kibble = as.character(map(strsplit(df_feature$filename, "_"), 2))) %>% 
  filter(annotation == "croc") %>% 
  plot_ly(
    x = ~maxdfreq,
    y = ~meandfreq,
    z = ~th,
    color = ~chat,
    marker = list(symbol = "circle", sizemode = 'diameter'))

df_feature %>%  
  mutate(chat = as.character(map(strsplit(df_feature$filename, "_"), 1)), 
         kibble = as.character(map(strsplit(df_feature$filename, "_"), 2))) %>% 
  filter(annotation == "croc") %>% 
  plot_ly(
    x = ~maxdfreq,
    y = ~meandfreq,
    z = ~th,
    color = ~kibble,
    marker = list(symbol = "circle", sizemode = 'diameter'))

## MODEL ALGORITHM ----
# source : https://towardsdatascience.com/classifying-rare-events-using-five-machine-learning-techniques-fab464573233

#library
require(class)
#Functions
get.error <- function(class,pred){
  cont.tab <- table(class,pred)
  print(cont.tab)
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

#data train and data test
set.seed(1234)
n <- nrow(df_feature)
n.train <- round(n/4, 0)
n.test <- n-n.train
ind.train <- sample(1:nrow(df_feature),n.train)
data.train <- df_feature[ind.train,]
data.test <- df_feature[-ind.train,]

#knn
pred.test.knn.1 <- knn(train=data.train[,4:20],test=data.test[,4:20],cl= data.train$annotation,k=1)

get.error(data.test$annotation,pred.test.knn.1)
get.specificity(data.test$annotation,pred.test.knn.1)
get.sensitivity(data.test$annotation,pred.test.knn.1)

#RandomForest 
library(randomForest)

x_train <- df_feature[ind.train, 4:20]
y_train <-  df_feature[ind.train, "annotation"]
y_train$annotation <- as.factor(y_train$annotation)

x_test <- df_feature[-ind.train, 4:20]
y_test <- df_feature[-ind.train, "annotation"]

##Calcul des temps d'execution
T1<-Sys.time()
model.50 <- randomForest( y_train$annotation~ .,
                          data = as.data.frame(data.train[,4:20]),
                          ntree = 50, na.action = na.omit,
                          importance = TRUE)
plot(model.50)

pred.test.rf.50 <- predict(model.50, newdata = data.test[,4:6])
CM.rf.50 <- table(data.test$annotation, pred.test.rf.50)
CM.rf.50

get.error(data.test$annotation,pred.test.rf.50)
get.specificity(data.test$annotation,pred.test.rf.50)
get.sensitivity(data.test$annotation,pred.test.rf.50)

# ROC Curve
library(ROCR)
AUC <- matrix(NA, nrow=5, ncol=1)
colnames(AUC) <- c("AUC") 
rownames(AUC) <- c("KNN", "RF")

knn_model <- knn(train=data.train[,4:20],test=data.test[,4:20],cl= data.train$annotation,k=11, prob = TRUE)
prob <- attr(knn_model, "prob")
prob <- 2*ifelse(knn_model == "-1", prob,1-prob) - 1
pred_knn <- prediction(prob, data.test$annotation)
performance_knn <- performance(pred_knn, "tpr", "fpr")
plot(performance_knn, col = 5, add = TRUE)

pred_RF <- predict(model.50, newdata = data.test[,4:20], type = "prob")
pred_class <-  prediction(pred_RF[,2], data.test$annotation)
performance_RF <- performance(pred_class,measure = "tpr",x.measure= "fpr")
plot(performance_RF, col = 3, add = TRUE)
abline(0,1)


## RARE EVENT DETECTION ----
#source : https://towardsdatascience.com/classifying-rare-events-using-five-machine-learning-techniques-fab464573233
