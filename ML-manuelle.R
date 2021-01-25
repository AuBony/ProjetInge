
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

# prediction (apres fonction predict(modele, newdata = X, type = prob (ou =response)) 
# = df avec 2 colonnes: proba d'appartenir a classe 0, proba d'appartenir a classe 1
# ligne = dans l'ordre des intervals donnes dans X

#---- FUNCTIONS ----

imp_norm <- function(file_name, path){
  # INPUTS : 
  # file_name : character, name of the wave file to import and normalize
  # path : character, path to the file
  # OUTPUT : 
  # wave file imported and normalized
  return(normalize(readWave(paste0(path,'cleanwav/',file_name)), center = TRUE))
}

sel_peaks <- function(wav, amp_lim, diff_lim){
  # INPUTS:
  # wav: Wave, an element of wavlist (a recording as wave)
  # amp_lim : numeric, amplitude limit of detection
  # diff_lim : numeric, minimal distance between 2 peaks
  # OUTPUT:
  # c : numeric, count of peaks in the wav
  vec <- wav@left
  vecsel <- which(abs(vec) > amp_lim)
  if (length(vecsel) == 0){
    return(0)
  } else if (length(vecsel) > 1) {
    sel <- c(vecsel[1])
    for (k in 2:length(vecsel)){
      if (abs(vecsel[k]-vecsel[k-1]) > diff_lim){
        sel <- c(sel, vecsel[k])
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

#----