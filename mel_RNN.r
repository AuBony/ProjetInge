library(keras)
library(tuneR)
library(ggplot2)
library(tidyverse)
library(reshape2)


############################################################
########### Importation et mise en forme des donnnees
############################################################

dataset_path = "D:/ProjetInge/audio_brute"
json_path = "data_melspec.json"
chat_path = "D:/ProjetInge"

# parametres utilises pour les donnees
# n_fft = 2048 # interval we consider to apply FFT
# sr = 22050 # sample rate
# n_mels = 20 # nb of mel bands
# hop_length = 512 # sliding window for FFT

chat <- read.table(paste0(chat_path,"/chat.csv"), 
                   sep = ";", header = TRUE)

X <- list()
for (i in 1:nrow(chat)){
  print(i)
  X[[i]] <- as.matrix(read.table(paste0(chat_path,"/Xchat/Xchat_",i,".csv"), sep = ";", header = FALSE))
}

Y <- read.table(paste0(chat_path,"/ychat.csv"), sep = ";", header = FALSE)
y <- list()
for (i in 1:nrow(Y)){
  print(i)
  y[[i]] <- as.matrix(Y[i,])
}

class(X) # list
length(X) # 194
class(X[[1]]) # matrix
dim(X[[1]]) # (20, 1077)

class(y) # list
length(y) # 194
class(y[[1]]) # matrix
dim(y[[1]]) # (1, 2)


############################################################
########### Trace d'un spectro 
############################################################

X_1 <- melt(X[[1]])

ggplot(X_1, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile() + 
  ylim(0.5,20.5) +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0,
    guide = "colourbar",
    aesthetics = "fill"
  )
print(y[[1]])


############################################################
########### Modele DL
############################################################

# decoupage train/test
dta <- X[1:60]
test <- sample(1:60, 15, replace = FALSE) # environ 30% du jdd
X_train <- dta[-test]
y_train <- dta[-test]
X_test <- dta[test]
y_test <- dta[test]

Xar_train <- array(unlist(X_train), dim=c(45,20,1077))
Xar_test <- array(unlist(X_test), dim=c(15,20,1077))
yar_train <- array(unlist(y_train), dim=c(45))
yar_test <- array(unlist(y_test), dim=c(15))


# initialisation du modele
model <- keras_model_sequential()

model %>%
  layer_flatten(input_shape = c(20, 1077)) %>%
  layer_dense(units = 256, activation="relu") %>%
  layer_dropout(0.4) %>%
  layer_dense(units = 128,activation="relu") %>%
  layer_dropout(0.4) %>%
  layer_dense(units = 64,activation="relu") %>%
  layer_dropout(0.4) %>%
  layer_dense(units = 10) %>%
  layer_activation(activation = 'softmax')

model %>% compile(
  optimizer = 'adam', 
  loss = 'categorical_crossentropy',
  metrics = c('accuracy')
)

# FONCTIONNE PAAAAAAAAS!!

h <- model %>% fit(
  Xar_train,
  yar_train, 
  batch_size = 5,
  epochs = 50)
plot(h)

class_pred <- model %>% predict_classes(y_test)
cM.DL <- caret::confusionMatrix(data = factor(class_pred,levels=levels(factor(test_labels))), 
                                reference = factor(test_labels))
cM.DL$overall[1]



