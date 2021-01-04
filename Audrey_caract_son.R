# PROJET INGENIEUR
# Caractérisation des sons
# Audrey Bony
# 4/01/2021


# Import function----
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

#Solution qui marche
read.data <- function(file){
  setwd("J:/Cours/M2_Stat/Projet_inge/data/data_perso/labels")
  dat <- read_delim(file,delim="\t", 
                    escape_double = FALSE, 
                    col_names = c("start", "end", "annotation"), 
                    trim_ws = TRUE)
  dat$fname <- file
  return(dat)
}

dataset <- do.call(rbind, lapply(list.files(path="data/data_perso/labels", pattern=".xt"),read.data))

library(plyr)
filename <- list.files(path="data/data_perso/labels",
                       full.names=TRUE, 
                       pattern="*.txt")
dat <- 
import.list <- ldply(filename, read_delim, delim="\t", 
                     escape_double = FALSE, 
                     col_names = c("start", "end", "annotation"), 
                     trim_ws = TRUE)

a <- list.files(path="data/data_perso/labels",
           full.names=TRUE, 
           pattern="*.txt") %>% lapply(read_delim, 
                                       delim="\t", 
                                       escape_double = FALSE, 
                                       col_names = c("start", "end", "annotation"), 
                                       trim_ws = TRUE) 
names(a) <- list.files(path="data/data_perso/labels")
b <- apply(mutate, filename = names(a))
  apply(mutate, filename = substring(1))


read_plus <- function(flnm) {
  read_csv(flnm) %>% 
    mutate(filename = flnm) }
tbl_with_sources <- list.files(pattern = "*.csv", full.names = T) %>% 
  map_df(~read_plus(.)) 
# Import ----
df <- multmerge(mypath = "data/data_perso/labels")

