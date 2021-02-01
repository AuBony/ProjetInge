# PROJET INGENIEUR
# IMPACT DE LA LAEBELLISATION
# Audrey Bony
# 31/01/2021

# DATA ----
require(readr)  # for read_csv()
require(dplyr)  # for mutate()
require(tidyr)  # for unnest()
require(purrr)  # for map(), reduce()
library(stringr) # for str_replace()

  # Labellisation 1
data_path_1 <- "ProjetInge/labels/"
files_1 <- dir(data_path_1, pattern = "*.txt")

data_1 <- tibble(filename = files_1) %>%
  mutate(file_contents = map(filename,         
                             ~ read_delim(file.path(data_path_1, .),
                                          delim="\t",
                                          escape_double = FALSE,
                                          col_names = c("start", "end", "annotation"),
                                          trim_ws = TRUE)))


vec_noms <- c()
start <- c()
end <- c()
annotation <- c()
for (k in 1:nrow(data_1)){
  vec_noms <- c(vec_noms, rep(data_1[[1]][[k]], nrow(data_1[[2]][[k]])))
  start <- c(start, data_1[[2]][[k]][[1]])
  end <- c(end, data_1[[2]][[k]][[2]])
  annotation <- c(annotation, data_1[[2]][[k]][[3]])
}

data_modif_1 <- data.frame(filename = vec_noms, start = start, 
                         end = end, annotation = annotation)

data_modif_chat_kibble_duration_1 <- data_modif_1 %>% 
  mutate(chat = as.character(map(strsplit(data_modif_1$filename, "_"), 1)), 
         kibble = as.character(map(strsplit(data_modif_1$filename, "_"), 2)),
         duration = end-start)
df_txt_1 <- cbind.data.frame(data_frame(id = seq(1, nrow(data_modif_chat_kibble_duration_1))), data_modif_chat_kibble_duration_1)
df_wav_1 <- df_txt_1
df_wav_1$filename <- str_replace(df_txt_1$filename, ".txt", ".wav")
df_wav_1 <- df_wav_1 %>% filter(annotation == "croc")

  #Labellsation 2
data_path_2 <- "ProjetInge/labs/"
files_2 <- dir(data_path_2, pattern = "*.txt")

data_2 <- tibble(filename = files_2) %>%
  mutate(file_contents = map(filename,         
                             ~ read_delim(file.path(data_path_2, .),
                                          delim="\t",
                                          escape_double = FALSE,
                                          col_names = c("start", "end", "annotation"),
                                          trim_ws = TRUE)))


vec_noms <- c()
start <- c()
end <- c()
annotation <- c()
for (k in 1:nrow(data_2)){
  vec_noms <- c(vec_noms, rep(data_2[[1]][[k]], nrow(data_2[[2]][[k]])))
  start <- c(start, data_2[[2]][[k]][[1]])
  end <- c(end, data_2[[2]][[k]][[2]])
  annotation <- c(annotation, data_2[[2]][[k]][[3]])
}

data_modif_2 <- data.frame(filename = vec_noms, start = start, 
                         end = end, annotation = annotation)

data_modif_chat_kibble_duration_2 <- data_modif_2 %>% 
  mutate(chat = as.character(map(strsplit(data_modif_2$filename, "_"), 1)), 
         kibble = as.character(map(strsplit(data_modif_2$filename, "_"), 2)),
         duration = end-start)
df_txt_2 <- cbind.data.frame(data_frame(id = seq(1, nrow(data_modif_chat_kibble_duration_2))), data_modif_chat_kibble_duration_2)
df_wav_2 <- df_txt_2
df_wav_2$filename <- str_replace(df_txt_2$filename, ".txt", ".wav")

# FUNCTION ----
give_croc_1 <- function(data = df_wav_1, frame_size = 0.1, ovlp_frame = 0, percent_expansion = 0, wav_path = "ProjetInge/cleanwav/" ){
  # Donne les features des crocs pour une labellisation large des crocs
  # INPUT
  # OUTPUT
  require(dplyr)
  require(tuneR)
  require(seewave)
  
  df_feature_event <- tibble(filename = character(),
                             start = numeric(),
                             end = numeric(),
                             event = numeric(),
                             method = factor(levels = c('1', '2')),
                             
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
  
  #Selection d'un enregistrement
  for (audio in unique(data[data$annotation == "croc", "filename"])){
    
    cat(".")
    
    crocs <- data %>% filter(filename ==  audio)
    
    
    #Selection d'un événement croc 
    for(l_croc in 1:nrow(crocs)){
      
      
      #Definition d'une zone de sample pour nos frames (on peut définir un interval un peu plus grand)
      #Decoupage en frame
      if (crocs[l_croc,"end"] + (frame_size*percent_expansion) - frame_size - crocs[l_croc,"start"] + (frame_size*percent_expansion) > frame_size ){
        
        for (moment in seq(from =  crocs[l_croc,"start"] - (frame_size*percent_expansion), to = crocs[l_croc,"end"] + (frame_size*percent_expansion) - frame_size, by = frame_size * (1-ovlp_frame))){
          
          wav_file <- readWave(paste0(wav_path, audio),
                               from = moment,
                               to = moment + frame_size,
                               units = "seconds") 
          wav_file <- tuneR::normalize(wav_file, center = TRUE)
          
          #Features de la frame
          sp <- seewave::specprop(seewave::spec(wav_file@left, f = wav_file@samp.rate, plot = FALSE, scaled = TRUE, norm = FALSE))
          # Ajout des features de la frame
          df_feature_event <- df_feature_event %>% add_row(
            filename = audio,
            start = moment,
            end = moment + frame_size,
            event = 1,
            method = as.factor(1),
            
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

give_no_event_1 <- function(data = df_wav_1, frame_size = 0.1, ovlp_frame = 0, wav_path = "ProjetInge/cleanwav/"){
  require(dplyr)
  require(tuneR)
  require(seewave)
  
  df_feature_no_event <- tibble(filename = character(),
                                start = numeric(),
                                end = numeric(),
                                event = numeric(),
                                method = factor(levels = c('1', '2')),
                                
                                th = numeric(),
                                maxdfreq =  numeric(),
                                meandfreq =  numeric(),
                                
                                smean =  numeric(),
                                ssd =  numeric(),
                                ssem =  numeric(),
                                smedian =  numeric(),
                                smode =  numeric(),
                                sQ25 =  numeric(),
                                sQ75 =  numeric(),
                                sIQR =  numeric(),
                                scent =  numeric(),
                                sskewness =  numeric(),
                                skurtosis =  numeric(),
                                ssfm =  numeric(),
                                ssh =  numeric())
  
  #Selection d'un enregistrement
  for (audio in unique(data$filename)){
    
    cat("_")
    
    no_event <- data %>% filter(filename ==  audio, annotation == "croc") #liste des crocs dans un enregistrement
    
    #Il n'y a pas de croc dans l'enregistrement
    if (dim(no_event)[1] == 0){
      deb <- 0
      audio_wav <- readWave(paste0(wav_path, audio), units = "seconds")
      fin <- round(length(audio_wav@left) / audio_wav@samp.rate, 2)
      
      for (moment in seq(from =  deb, to = fin - frame_size, by = frame_size * (1-ovlp_frame))){
        
        wav_file <- readWave(paste0(wav_path, audio),
                             from = moment,
                             to = moment + frame_size,
                             units = "seconds") 
        tuneR::normalize(wav_file, center = TRUE)
        #Features de la frame
        sp <- seewave::specprop(seewave::spec(wav_file@left, f = wav_file@samp.rate, plot = FALSE, scaled = TRUE, norm = FALSE))
        #
        df_feature_no_event <- df_feature_no_event %>% add_row(
          filename = audio,
          start = moment,
          end = moment + frame_size,
          event = 0,
          method = as.factor(1),
          
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
      
      
      #Il y a bien un croc dans l'enregistrement
    }else{
      
      for (l_no_event in 1: nrow(no_event)){
        #Deb et fin
        if (l_no_event == 1){
          deb <- 0
          fin <- no_event$start[1]
        }
        
        if (l_no_event >1){
          deb <- no_event$end[l_no_event - 1]
          fin <- no_event$start[l_no_event]
        }
        
        if (fin - deb > frame_size){
          for (moment in seq(from =  deb, to = fin - frame_size, by = frame_size * (1-ovlp_frame))){
            
            wav_file <- readWave(paste0(wav_path, audio),
                                 from = moment,
                                 to = moment + frame_size,
                                 units = "seconds") 
            wav_file <- tuneR::normalize(wav_file, center = TRUE)
            #Features de la frame
            sp <- seewave::specprop(seewave::spec(wav_file@left, f = wav_file@samp.rate, plot = FALSE, scaled = TRUE, norm = FALSE))
            #
            df_feature_no_event <- df_feature_no_event %>% add_row(
              filename = audio,
              start = moment,
              end = moment + frame_size,
              event = 0,
              method = as.factor(1),
              
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
  }
  return(as.data.frame(df_feature_no_event))
}

give_croc_2 <- function(data = df_wav_2, expansion = 0, wav_path = "ProjetInge/cleanwav/" ){
  # gives features for each break frame
  
  require(dplyr)
  require(tuneR)
  require(seewave)
  
  df_feature_event <- tibble(filename = character(),
                             start = numeric(),
                             end = numeric(),
                             event = factor(levels = c('0','1')),
                             method = factor(levels = c('1','2')),
                             
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
      if (expansion == 0){
        wav_file <- readWave(paste0(wav_path, audio),
                             from = crocs$start[l_croc],
                             to = crocs$end[l_croc],
                             units = "seconds") 
        wav_file <- tuneR::normalize(wav_file, center = TRUE)
        
        #Features de la frame
        sp <- seewave::specprop(seewave::spec(wav_file@left, f = wav_file@samp.rate, plot = FALSE, scaled = FALSE, norm = FALSE))
        
        # Ajout des features de la frame
        df_feature_event <- df_feature_event %>% add_row(
          filename = audio,
          start =  crocs$start[l_croc],
          end =  crocs$end[l_croc],
          event = as.factor(1),
          method = as.factor(2),
          
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
        
      } else if((crocs[l_croc,"start"] * (1 - expansion) > 0)){
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
            event = as.factor(1),
            method = as.factor(2),
            
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

give_no_event_2 <- function(data = df_wav_2, frame_size = 0.02, nb_ech = 10, wav_path = "ProjetInge/cleanwav/"){
  # gives features for each no_event frame
  
  require(pracma)
  require(dplyr)
  require(tuneR)
  require(seewave)
  
  df_feature_no_event <- tibble(filename = character(),
                                start = numeric(),
                                end = numeric(),
                                event = factor(levels = c('0','1')),
                                method = factor(levels = c('1','2')),
                                
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
    
    no_event <- data %>% filter(filename ==  audio)
    
    for (l_no_event in 1:(nrow(no_event)+1)){
      
      if (l_no_event == 1){
        deb <- 0
        fin <- no_event$start[l_no_event]
      } else if (l_no_event > nrow(no_event)){
        deb <- no_event$end[l_no_event - 1]
        fin <- duration(readWave(filename = paste0(wav_path, audio)))
      } else if (l_no_event >1){
        deb <- no_event$end[l_no_event - 1]
        fin <- no_event$start[l_no_event]
      } 
      
      if (fin - deb > nb_ech * frame_size){
        
        deb_rand <- round(runif(min =  deb, max = (fin-frame_size), n = nb_ech),digits =  2)
        
        for (moment in deb_rand){
          wav_file <- readWave(paste0(wav_path, audio),
                               from = moment,
                               to = moment + frame_size,
                               units = "seconds") 
          wav_file <- tuneR::normalize(wav_file, center = TRUE)
          
          #Features de la frame
          sp <- seewave::specprop(seewave::spec(wav_file@left, f = wav_file@samp.rate, plot = FALSE, scaled = TRUE, norm = FALSE))
          #
          df_feature_no_event <- df_feature_no_event %>% add_row(
            filename = audio,
            start = moment,
            end = moment + frame_size,
            event = as.factor(0),
            method = as.factor(2),
            
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
  return(as.data.frame(df_feature_no_event))
}

# FEATURES ----
croc_1 <- give_croc_1(data = df_wav_1)
event_1 <- give_no_event_1(data = df_wav_1)
croc_2 <- give_croc_2(data = df_wav_2)
event_2 <- give_no_event_2(data = df_wav_2)

# COMPARISON ----
require(FactoMineR)
res.PCA<-PCA(rbind.data.frame(croc_1, croc_2),quali.sup=c(1,4,5),quanti.sup=c(2,3),graph=FALSE)
plot.PCA(res.PCA,choix='var',title="Graphe des variables de l'ACP")
plotellipses(res.PCA, keepvar=5,invisible=c('quali','ind.sup'),title="Graphe des Crocs",cex=1.6,cex.main=1.6,cex.axis=1.6,label ='none')

res.PCA<-PCA(rbind.data.frame(event_1, event_2),quali.sup=c(1,4,5),quanti.sup=c(2,3),graph=FALSE)
plot.PCA(res.PCA,choix='var',title="Graphe des variables de l'ACP")
plotellipses(res.PCA, keepvar=5,invisible=c('quali','ind.sup'),title="Graphe des individus de l'ACP",cex=1.5,cex.main=1.5,cex.axis=1.5,label ='none')

data.frame(type = rep(c("croc", "no_event"), 2), 
           method = rep(c("1","2"), each = 2),
           value = c(nrow(croc_1), nrow(event_1), nrow(croc_2), nrow(event_2))) %>% 
ggplot(aes(x=method, y=value, fill=type)) +
  geom_bar(stat="identity", width = 0.2) +
  scale_fill_brewer(palette="Pastel2") +
  theme_light()
