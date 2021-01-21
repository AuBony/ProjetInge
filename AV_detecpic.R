##### detection de pic a partir des donnees brutes

load("C:/Users/HP/Documents/GitHub/ProjetInge/complete_clean_data.RData")

test <- dta[,c(seq(1,882000, by = 200), 882001, 882002)]
n <- ncol(test)
plot(1:(n-2), test[2,-c(n-1, n)], type = 'l')

View(test[1:10,c(1:5,4411,4412)])

testcr <- as.data.frame(t(apply(t(test),2,scale)))

l <- 2
plot(1:(n-2), testcr[l,-c(n-1, n)], type = 'l')
print(testcr[l,c(n-1, n)])

# importation/comptage avec normalisation des donnees

library(tuneR)
require(dplyr)
require(tidyr)
require(purrr)
require(readr)

path <- '~/GitHub/ProjetInge/'

# COMPTAGE

fich <- list.files(paste0(path,'cleanwav'))
wavlist <- list(normalize(readWave(paste0(path,'cleanwav/',fich[1])), center = TRUE))
for (k in 2:length(fichl)){
  wavlist[[k]] <- normalize(readWave(paste0(path,'cleanwav/',fich[k])),center = TRUE)
}

count <- rep(0,length(wavlist))
n <- length(wavlist)
amp_lim <- 0.5
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
print(count)

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
data_modif <- unnest(data, cols = c(file_contents))

data_modif_chat_kibble_duration <- data_modif %>% 
  mutate(chat = as.character(map(strsplit(data_modif$filename, "_"), 1)), 
         kibble = as.character(map(strsplit(data_modif$filename, "_"), 2)),
         duration = end-start)
df_txt <- cbind.data.frame(data_frame(id = seq(1, nrow(data_modif_chat_kibble_duration))), data_modif_chat_kibble_duration)
df_wav <- df_txt
df_wav$filename <- str_replace(df_txt$filename, ".txt", ".wav")
df_wav


fichlab <- list.files(paste0(path,'labels/'))



# ecart minimal entre 2 crocs? on va prendre 0.1 sec pour l'instant, apres on pourra
# regarder la vraie valeur
# 0.1*44100 = 4410 donc on va prendre un ecart minimal de 4400 unites entre 2 pics

# marche pas si pas de croc car dilate echelle, mais peut-etre a utiliser apres 
# le tri croc/pas croc pour compter sur le signal entier