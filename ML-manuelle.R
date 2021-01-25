
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