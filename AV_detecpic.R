
path <- '~/GitHub/ProjetInge/'

#---- PACKAGES ----

library(tuneR)
require(readr)  # for read_csv()
require(dplyr)  # for mutate()
require(tidyr)  # for unnest()
require(purrr)  # for map(), reduce()
library(stringr) # for str_replace()
library(ggplot2)
library(plotly)
library(vioplot)

#---- BREAKS IMPORTATION ----

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

addmargins(table(df_wavc$chat,df_wavc$kibble))

eff <- df_wavc %>% group_by(filename) %>% summarise(nb_bk = n())
nb_bk_tot <- sum(eff$nb_bk)

#---- FUNCTIONS ----

imp_norm <- function(file_name, path){
  # INPUTS : 
  # file_name : character, name of the wave file to import and normalize
  # path : character, path to the file
  # OUTPUT : 
  # wave file imported and normalized
  return(normalize(readWave(paste0(path,'cleanwav/',file_name)), center = TRUE))
}

count_peaks <- function(wav, amp_lim, diff_lim){
  # INPUTS:
  # wav: Wave, an element of wavlist (a recording as wave)
  # amp_lim : numeric, amplitude limit of detection
  # diff_lim : numeric, minimal distance between 2 peaks
  # OUTPUT:
  # c : numeric, count of peaks in the wav
  c <- 1
  vec <- wav@left
  vecsel <- which(abs(vec) > amp_lim)
  if (length(vecsel) == 0){
    c <- 0
  } else if (length(vecsel) > 1) {
    for (k in 2:length(vecsel)){
      if (abs(vecsel[k]-vecsel[k-1]) > diff_lim){
        c <- c + 1
      }
    }}
  return(c)
}

mse <- function(pred, act){
  # INPUTS
  # pred : vector of predictions
  # act : vector of actual values
  # OUPUT
  # MSE
  return(sum((pred - act)^2) / length(pred))
}


#---- COUNTING ----

fich <- data.frame(list.files(paste0(path,'cleanwav')))
wavlist <- apply(fich, 1, FUN = imp_norm, path = path)
amp_lim = 0.4
diff_lim = 18500
count <- unlist(lapply(X = wavlist, FUN = count_peaks, 
                       amp_lim = amp_lim, diff_lim = diff_lim))
count
mse(count, eff$nb_bk)

#---- PREDICTION vs COUNTING ----

df1 <- data.frame(actual = eff$nb_bk, prediction = count)
df1 <- df1 %>% add_count(prediction)

title1 <- paste0('Number of breaks, predicted vs actual \n for amp_lim = ', amp_lim,
                ' and diff_lim = ', diff_lim)
ggplot(df1, aes(x = actual, y = prediction)) +
  geom_segment(aes(x = 0, y = 0, xend = 6, yend = 6)) +
  geom_point(aes(col = n), size = 3) +
  ylim(0,6) +
  xlim(0,6) +
  theme(plot.title = element_text()) +
  labs(title = title1) +
  theme_light()

#---- AMPLITUDE LIMIT OPTIMIZATION ----

mse2 <- rep(0, 10)
diff_lim <- 18500
for (j in 1:10){
  count2 <- unlist(lapply(X = wavlist, FUN = count_peaks, 
                         amp_lim = j*0.1, diff_lim = diff_lim))
  mse2[j] <- mse(count2, eff$nb_bk)
  print(j)
}

title2 <- paste0('MSE with different amplitude limits, \n with diff_lim = ', 
                 diff_lim)
df2 <- data.frame(amp_lim = seq(0.1, 1, by = 0.1), mse = mse2) 
p2 <- ggplot(df2, aes(x = amp_lim, y = mse)) +
  geom_line() +
  theme_light() +
  theme(plot.title = element_text()) +
  labs(title = title2) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1))
ggplotly(p2)

amp_lim_opt <- df2$amp_lim[which.min(mse2)]

#---- MINIMAL DISTANCE BETWEEN 2 PEAKS ----
# variation de la limite de separation des pics 
# -> devrait pas faire avec mse, 
# juste moyenne sur nos donnees

mse3 <- rep(0, 87)
a <- 0
amp_lim <- amp_lim_opt
for (j in seq(1000, 44000, by = 500)){
  a <- a + 1
  count3 <- unlist(lapply(X = wavlist, FUN = count_peaks, 
                         amp_lim = amp_lim, diff_lim = j))
  mse3[a] <- mse(count3, eff$nb_bk)
  print(a)
}

df3 <- data.frame(diff_lim = seq(1000, 44000, by = 500), mse = mse3) 
title3 <- paste0('MSE with different minimal distances between peaks, \n with amp_lim = ',
                 amp_lim)
p3 <- ggplot(df3, aes(x = diff_lim, y = mse3)) +
  geom_line() +
  theme_light() +
  ylab('mse') +
  theme(plot.title = element_text()) +
  labs(title = title3)
ggplotly(p3)

df3$diff_lim[which.min(mse3)] # 18500

# ecart minimal entre 2 crocs? on va prendre 0.1 sec pour l'instant, apres on pourra
# regarder la vraie valeur
# 0.1*44100 = 4410 donc on va prendre un ecart minimal de 4400 unites entre 2 pics

bk_length <- data.frame(bk_length = df_wavc$end - df_wavc$start)

sep_length <- c()
for (k in 2:nrow(df_wavc)){
  if (df_wavc$filename[k-1] == df_wavc$filename[k]){
    print(paste(df_wavc$filename[k-1], df_wavc$filename[k], sep = ','))
    sep_length <- c(sep_length, df_wavc$start[k] - df_wavc$end[k-1])
  }
}
sep_length <- data.frame(sep_length)

# length of a break (in seconds)
ggplot(bk_length, aes(x = bk_length)) +
  geom_boxplot(aes(y = -0.5, fill = "#69b3a2"), alpha = 0.6, show.legend = FALSE,
               width = 0.3) +
  geom_density(aes(x = bk_length), fill = "slateblue", 
               inherit.aes = FALSE, alpha = 0.6) +
  ylab('density') +
  xlab('bk_length (s)') +
  theme(plot.title = element_text()) +
  labs(title = 'Breaks length') +
  scale_x_continuous(breaks = seq(0, 0.8, by = 0.1)) +
  theme_minimal()

# distance between 2 peaks in the dataset (in seconds)
ggplot(sep_length, aes(x = sep_length)) +
  geom_boxplot(aes(y = -0.1, fill = "#69b3a2"), alpha = 0.6, show.legend = FALSE,
               width = 0.05) +
  geom_density(aes(x = sep_length), fill = "slateblue", 
               inherit.aes = FALSE, alpha = 0.6) +
  ylab('density') +
  xlab('sep_length (s)') +
  theme(plot.title = element_text()) +
  labs(title = 'Distance between 2 peaks') +
  scale_x_continuous(breaks = seq(0, 6, by = 0.5)) +
  theme_minimal()


#---- OVERESTIMATION MAXIMISATION ----

table(nb_bk = eff$nb_bk, count)
length(which(count - eff$nb_bk >= 0)) # -> a maximiser en fonction de lim_ampl

overest4 <- rep(0, 10)
mse4 <- rep(0, 10)
diff_lim <- 18500
for (j in 1:10){
  count4 <- unlist(lapply(X = wavlist, FUN = count_peaks, 
                          amp_lim = j*0.1, diff_lim = diff_lim))
  overest4[j] <- length(which(count4 - eff$nb_bk >= 0))
  mse4[j] <- mse(count4, eff$nb_bk)
  print(j)
}

title4 <- paste0('Overestimation and mse with different amplitude limits, \n with diff_lim = ', 
                 diff_lim)
df4 <- data.frame(amp_lim = seq(0.1, 1, by = 0.1), overestimation = overest4,
                  mse = mse4) 
scaleFactor <- max(df4$overestimation) / max(df4$mse)
ggplot(df4, aes(x = amp_lim, y = overestimation)) +
  geom_line(col = 'slateblue1', size = 1) +
  geom_line(aes(y = mse4 * scaleFactor), col = 'lightpink1', size = 1) +
  theme_minimal() +
  theme(plot.title = element_text(),
        axis.title.y.left=element_text(face="bold", color="slateblue1"),
        axis.text.y.left=element_text(face="bold", color="slateblue1"),
        axis.title.y.right=element_text(face="bold", color="lightpink1"),
        axis.text.y.right=element_text(face="bold", color="lightpink1")) +
  labs(title = title4) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
  scale_y_continuous(name = "overestimation", 
                     sec.axis = sec_axis(~./scaleFactor, name = "mse"))

amp_lim = 0.1
diff_lim = 18500
count <- unlist(lapply(X = wavlist, FUN = count_peaks, 
                       amp_lim = amp_lim, diff_lim = diff_lim))
count
mse(count, eff$nb_bk)

df5 <- data.frame(actual = eff$nb_bk, prediction = count)
df5 <- df5 %>% add_count(prediction)

title5 <- paste0('Number of breaks, predicted vs actual \n for amp_lim = ', amp_lim,
                 ' and diff_lim = ', diff_lim)
ggplot(df5, aes(x = actual, y = prediction)) +
  geom_segment(aes(x = 0, y = 0, xend = 6, yend = 6)) +
  geom_point(aes(col = n), size = 3) +
  ylim(0,10) +
  xlim(0,6) +
  theme(plot.title = element_text()) +
  labs(title = title5) +
  theme_light()

#---- COMMENTS ----

# marche pas si pas de croc car dilate echelle, mais peut-etre a utiliser apres 
# le tri croc/pas croc pour compter sur le signal entier