# PROJET INGENIEUR
# ALGORITHME MACHINE LEARNING CROC DETECTION : METHODE DECOMPOSITION
# Audrey Bony
# 28/01/2021

#DATA_WAV ----
require(readr)  # for read_csv()
require(dplyr)  # for mutate()
require(tidyr)  # for unnest()
require(purrr)  # for map(), reduce()
library(stringr) # for str_replace()


data_path <- "ProjetInge/labs/"
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

#DATA FEATURES ----

give_croc <- function(data = df_wav, expansion = 0, wav_path = "ProjetInge/cleanwav/" ){
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
  
  for (audio in unique(data[, "filename"])){
    
    print(audio)
    
    crocs <- data %>% filter(filename ==  audio)
    
    
    #Selection d'un événement croc 
    for(l_croc in 1:nrow(crocs)){
      if((crocs[l_croc,"start"] * (1 - expansion) > 0)){
        e <- crocs[l_croc,"duration"] * expansion
        moment <- list(c(crocs[l_croc,"start"] - e, crocs[l_croc, "end"] - e),
                            c(crocs[l_croc,"start"], crocs[l_croc, "end"]),
                            c(crocs[l_croc,"start"] + e, crocs[l_croc, "end"] + e))
        
        for (k in 1:3){
          wav_file <- readWave(paste0(wav_path, audio),
                               from = moment[[k]][1],
                               to = moment[[k]][2],
                               units = "seconds") 
          wav_file<- tuneR::normalize(wav_file, center = TRUE)
          
          #Features de la frame
          sp <- seewave::specprop(seewave::spec(wav_file@left, f = wav_file@samp.rate, plot = FALSE, scaled = FALSE, norm = FALSE))
          
          # Ajout des features de la frame
          df_feature_event <- df_feature_event %>% add_row(
            filename = audio,
            start =  moment[[k]][1],
            end =  moment[[k]][2],
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

df <- give_croc(df_wav, expansion = 0.2)


require(Factoshiny)
df %>% mutate(chat = as.character(map(strsplit(filename, "_"), 1)), 
              kibble = as.character(map(strsplit(filename, "_"), 2))) %>% 
Factoshiny()

give_event <- function(data = df_wav){}
