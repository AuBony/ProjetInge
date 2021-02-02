path <- '~/GitHub/ProjetInge/'

#---- PACKAGES ----

library(tuneR)
library(seewave)
require(readr)  # for read_csv()
require(dplyr)  # for mutate()
require(tidyr)  # for unnest()
require(purrr)  # for map(), reduce()
library(stringr) # for str_replace()
library(ggplot2)
library(plotly)
library(vioplot)
library(randomForest)
library(plyr) # ldply

#---- FUNCTIONS ----

imp <- function(file_name, path_wav){
  # INPUTS : 
  # file_name : character, name of the wave file to import and normalize
  # path : character, path to the file
  # OUTPUT : 
  # wave file imported and normalized
  return(readWave(paste0(path_wav, file_name)))
}

count_peaks <- function(wav, amp_lim, diff_lim){
  # INPUTS:
  # wav: Wave, an element of wavlist (a recording as wave)
  # amp_lim : numeric, amplitude limit of detection
  # diff_lim : numeric, minimal distance between 2 peaks
  # OUTPUT:
  # c : numeric, count of peaks in the wav
  c <- 1
  vec <- scale(wav@left)
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

fich <- data.frame(filename = list.files(paste0(path,'wav')))
wavlist <- apply(fich, 1, FUN = imp, path_wav = paste0(path,'wav/'))
actual <- read.table(paste0(path, 'nb_bk_withnobreak.csv'), sep = ';', dec = '.', header = TRUE)
actual$filename <- as.factor(actual$filename)
summary(actual)

mse1 <- rep(0, 101)
diff_lim = 18500
b <- 1
for (a in seq(0, 50, by=0.5)){
  count <- unlist(lapply(X = wavlist, FUN = count_peaks, 
                         amp_lim = a, diff_lim = diff_lim))
  print(count)
  mse1[b] <- mse(count, actual$nb_bk)
  print(b)
  b <- b+1
}

title <- paste0('MSE with different amplitude limits, \n with diff_lim = ', 
                 diff_lim)
df <- data.frame(amp_lim = seq(0, 50, by=0.5), mse = mse1) 
p <- ggplot(df, aes(x = amp_lim, y = mse)) +
  geom_line() +
  theme_light() +
  theme(plot.title = element_text()) +
  labs(title = title) +
  scale_x_continuous(breaks = seq(0, 50, by = 2))
ggplotly(p)

min(df$mse[3:101])
which.min(df$mse[3:101]) # 64

amp_lim <- df$amp_lim[64]

count <- unlist(lapply(X = wavlist, FUN = count_peaks, 
                       amp_lim = amp_lim, diff_lim = diff_lim))

mse(count, actual$nb_bk)
length(which(count == actual$nb_bk))

df <- data.frame(actual = actual$nb_bk, prediction = count)
df <- df %>% add_count(actual, prediction)
m <- max(actual$nb_bk)+1

title <- paste0('Number of breaks, predicted vs actual \n for amp_lim = ', amp_lim,
                 ', diff_lim = ', diff_lim)
ggplot(df, aes(x = actual, y = prediction)) +
  stat_density2d(geom='tile', aes(fill=..density..), contour = FALSE) +
  geom_segment(aes(x = -1, y = -1, xend = m, yend = m), 
               colour = 'lightgrey', alpha = 0.5) +
  geom_point(colour='lightgrey', size = 1) +
  scale_x_continuous(breaks = seq(-1, m, by = 1)) +
  scale_y_continuous(breaks = seq(-1, m, by = 1)) +
  theme(plot.title = element_text()) +
  labs(title = title) +
  theme_light()

