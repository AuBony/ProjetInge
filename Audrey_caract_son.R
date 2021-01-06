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
table(df$annotation)

#Comparer durée des crocs et des mach
ggplot(df, aes(x=annotation, y=duration, fill=annotation)) +
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
hist(df$duration,breaks =  100)
density(df$duration)

ggplot(df, aes(x = duration)) +
  geom_density(fill = "#69b3a2", color = "#69b3a2", alpha = 0.8) +
  ggtitle("Durée des sons labellisés")

ggplot(df, aes(x = duration, fill = chat, cut = chat)) +
  geom_density(adjust = 1.5, alpha = 0.8) +
  ggtitle("Durée des sons labellisés par chat")

df %>% filter(annotation == "mach") %>% 
  ggplot(aes(x = duration, fill = kibble, cut = kibble)) +
  geom_density(adjust = 1.5, alpha = 0.8) +
  ggtitle("Durée des mastications par friandise")

df %>% filter(annotation == "mach") %>% 
  ggplot(aes(x = duration, fill = chat, cut = chat)) +
  geom_density(adjust = 1.5, alpha = 0.8) +
  ggtitle("Durée des mastications par chat")

df %>% filter(annotation == "croc") %>% 
  ggplot(aes(x = duration, fill = chat, cut = chat)) +
  geom_density(adjust = 1.5, alpha = 0.8) +
  ggtitle("Durée des crocs par chat")


############# MODEL #############
# FEATURES ----
library(soundgen)
library(tuneR)

# Selectionner 
getwd()
wav_path <- "ProjetInge/cleanwav/"


require(seewave)
df_feature <- tibble(id = numeric(),
                         filename = character(),
                         annotation = character(),
                         th = numeric(),
                         maxdfreq = numeric(),
                         meandfreq = numeric())

for (i in 1:nrow(df_wav)){
  wav_file <- readWave(paste0(wav_path, df_wav[i,2]),
                       from = df_wav[i,3],
                       to = df_wav[i,4],
                       units = "seconds") 
    #
  df_feature <- df_feature %>% add_row(id = df_wav$id[i],
                          filename = df_wav$filename[i],
                          annotation = df_wav$annotation[i],
                          th = th(env(wav_file, plot = FALSE)),
                          maxdfreq = max(dfreq(wav_file, plot = FALSE)[,2]),
                          meandfreq = mean(dfreq(wav_file, plot = FALSE)[,2])
                          ) 
}

#Temporal Entropy
th(env(a, plot = FALSE))

#Dominant frequency of a time wave
max(dfreq(a, plot = FALSE)[,2])
mean(dfreq(a, plot = FALSE)[,2])

# Vizualisation
library(plotly)
plot(df_feature$meandfreq, df_feature$maxdfreq, col = as.factor(df_feature$annotation))

plot_ly(df_feature,
        x = ~maxdfreq,
        y = ~meandfreq,
        z = ~th,
        color = ~annotation,
        colors = c("#383ED9", "#FADA23"),
        marker = list(symbol = "circle", sizemode = 'diameter'))

# MODEL ALGORITHM ----
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
pred.test.knn.1 <- knn(train=data.train[,4:6],test=data.test[,4:6],cl= data.train$annotation,k=1)

get.error(data.test$annotation,pred.test.knn.1)
get.specificity(data.test$annotation,pred.test.knn.1)
get.sensitivity(data.test$annotation,pred.test.knn.1)

pred.grid.knn.1 <- knn(train=data.train[,3:4],test=data.grid,cl=data.train$default,k=1)

