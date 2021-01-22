##### detection de pic a partir des donnees brutes

load("C:/Users/HP/Documents/GitHub/ProjetInge/complete_clean_data.RData")

# importation/comptage avec normalisation des donnees

library(tuneR)
require(readr)  # for read_csv()
require(dplyr)  # for mutate()
require(tidyr)  # for unnest()
require(purrr)  # for map(), reduce()
library(stringr) # for str_replace()
library(ggplot2)
library(plotly)

path <- '~/GitHub/ProjetInge/'

# IMPORTATION DES NB_BK

data_path <- paste0(path,'labels/')
files <- dir(data_path, pattern = "*.txt")

data <- data_frame(filename = files) %>%
  mutate(file_contents = map(filename,         
                             ~ read_delim(file.path(data_path, .),
                                          delim="\t",
                                          escape_double = FALSE,
                                          col_names = c("start", "end", "annotation"),
                                          trim_ws = TRUE)))

# pour faire le tableau avec filename | start | end | annotation

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
df_wavc <- df_wav %>% filter(annotation == 'croc')
df_wavc

eff <- df_wavc %>% group_by(filename) %>% summarise(count = n())
nb_crocs_tot <- sum(eff[[2]])

# COMPTAGE

fich <- list.files(paste0(path,'cleanwav'))
wavlist <- list(normalize(readWave(paste0(path,'cleanwav/',fich[1])), center = TRUE))
for (k in 2:length(fich)){
  wavlist[[k]] <- normalize(readWave(paste0(path,'cleanwav/',fich[k])),center = TRUE)
}

count <- rep(0,length(wavlist))
n <- length(wavlist)
amp_lim <- 0.3
diff_lim <- 4000
for (i in 1:n){
  c <- 1
  vec <- wavlist[[i]]@left
  vecsel <- which(vec > amp_lim | vec < -amp_lim)
  if (length(vecsel) == 0){
    c <- 0
  } else if (length(vecsel) > 1) {
    for (k in 2:length(vecsel)){
      if (vecsel[k]-vecsel[k-1] > diff_lim){
        c <- c+1
      }
    }}
  count[i] <- c
}
print(count)

## COMPARAISON PREDICTION / COMPTAGE

df <- data.frame(actual = eff[[2]], prediction = count)
df <- df %>% add_count(prediction)

ggplot(df, aes(x = actual, y = prediction)) +
  geom_segment(aes(x = 0, y = 0, xend = 6, yend = 6)) +
  geom_point(aes(col = n)) +
  ylim(0,6) +
  xlim(0,6) +
  theme(plot.title = element_text()) +
  labs(title = 'Number of breaks predicted vs actual') +
  theme_light()

# variation de la limite d'amplitude

mse <- rep(0, 10)
for (j in 1:10){
  count <- rep(0,length(wavlist))
  n <- length(wavlist)
  amp_lim <- j*0.1
  diff_lim <- 4400
  for (i in 1:n){
    c <- 1
    vec <- wavlist[[i]]@left
    vecsel <- which(vec > amp_lim | vec < -amp_lim)
    if (length(vecsel) == 0){
      c <- 0
    } else if (length(vecsel) > 1) {
      for (k in 2:length(vecsel)){
        if (vecsel[k]-vecsel[k-1] > diff_lim){
          c <- c+1
        }
      }}
    count[i] <- c
  }
  mse[j] <- sum((count - eff[[2]])^2) / length(count)
  print(j)
}

df <- data.frame(amp_lim = seq(0.1, 1, by = 0.1), mse = mse) 
p <- ggplot(df, aes(x = amp_lim, y = mse)) +
  geom_line() +
  theme_light() +
  theme(plot.title = element_text()) +
  labs(title = 'MSE with different amplitude limits for selection')
ggplotly(p)

# variation de la limite de separation des pics -> devrait pas faire avec mse, 
# juste moyenne sur nos donnees

mse <- rep(0, 87)
a <- 0
for (j in seq(1000, 44000, by = 500)){
  a <- a + 1
  count <- rep(0,length(wavlist))
  n <- length(wavlist)
  amp_lim <- 0.4
  diff_lim <- j
  for (i in 1:n){
    c <- 1
    vec <- wavlist[[i]]@left
    vecsel <- which(vec > amp_lim | vec < -amp_lim)
    if (length(vecsel) == 0){
      c <- 0
    } else if (length(vecsel) > 1) {
      for (k in 2:length(vecsel)){
        if (vecsel[k]-vecsel[k-1] > diff_lim){
          c <- c+1
        }
      }}
    count[i] <- c
  }
  mse[a] <- sum((count - eff[[2]])^2) / length(count)
  print(j)
}

df <- data.frame(diff_lim = seq(1000, 44000, by = 500), mse = mse) 
p <- ggplot(df, aes(x = diff_lim, y = mse)) +
  geom_line() +
  theme_light() +
  theme(plot.title = element_text()) +
  labs(title = 'MSE with different distance between peaks limits for selection')
ggplotly(p)

df$diff_lim[which.min(mse)] # 18500

# ecart minimal entre 2 crocs? on va prendre 0.1 sec pour l'instant, apres on pourra
# regarder la vraie valeur
# 0.1*44100 = 4410 donc on va prendre un ecart minimal de 4400 unites entre 2 pics

# marche pas si pas de croc car dilate echelle, mais peut-etre a utiliser apres 
# le tri croc/pas croc pour compter sur le signal entier