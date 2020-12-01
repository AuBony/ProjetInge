#load("../data/data_agrocampus.RData")
load("~/2020-2021/PROJET-INGE/data_agrocampus.RData")

library(plotly)
library(tidyverse)

son <- model_data[, 7:1102506]
chat <- model_data[,1:6]

toy_chat <- chat[1:10,]
f <- 25/1102500

summary(chat)
table(chat$Cat_name, chat$Session)
table(chat$Cat_name, chat$Diet)

# INTERVALLE DE TEMPS CHOISI
dep <- 0
fin <- 1102500
pas <- 500
temps <- seq(from = dep, to = fin, by = pas)
indice <- temps + 1

###########################################################################
# courbes 1eres lignes
plot(temps,son[1,temps], type = 'l', xlab = "temps", ylab = "son",
     main = "Courbe sonore")
plot(temps,son[2,temps], type = 'l', xlab = "temps", ylab = "son",
     main = "Courbe sonore")
plot(temps,son[3,temps], type = 'l', xlab = "temps", ylab = "son",
     main = "Courbe sonore")

# lignes interessantes (on peut bien entendre crunching sounds)
chat['32',1:6]
plot(temps,son['32',temps], type = 'l', xlab = "temps", ylab = "son",
     main = "Courbe sonore")
chat['34',1:6]
plot(temps,son['34',temps], type = 'l', xlab = "temps", ylab = "son",
     main = "Courbe sonore")
chat['39',1:6]
plot(temps,son['39',temps], type = 'l', xlab = "temps", ylab = "son",
     main = "Courbe sonore")
chat['40',1:6]
plot(temps,son['40',temps], type = 'l', xlab = "temps", ylab = "son",
     main = "Courbe sonore")

###########################################################################
# plotly

amplitude <- as.integer(son['32',temps])
time <- temps[-length(temps)]*f
dta <- data.frame(time = time, amplitude = amplitude)
plot_ly(x = ~time, y = ~amplitude, mode = 'lines')

p1 <- dta %>% ggplot() +
  aes(x = time, y = amplitude) +
  geom_line() +
  theme_minimal()

p1 <- p1 + annotate("text", x = 2, y = -22000, 
                    label = paste('<span: nb_bk: ',
                                  chat['32','nb_bk'],'\n',
                                  'nb_bit: ',
                                  chat['32','nb_bit']),
                    colour = 'orange', size = 5)
ggplotly(p1)


# fonction qui prend en entrée la ligne qu'on veut tracer et le pas de temps

draw_plot <- function(line,pas,dep,fin){
  dt <- 25/1102500
  dep <- dep
  fin <- fin/dt
  pas <- 500
  temps <- seq(from = dep, to = fin, by = pas)
  indice <- temps + 1
  dta <- data.frame(time = temps[-length(temps)]*dt, 
                    amplitude = as.integer(son[line,temps]))
  
  # graphe
  p1 <- dta %>% ggplot() +
    aes(x = time, y = amplitude) +
    geom_line(size = 0.2) +
    theme_minimal()
  
  p1 <- p1 + annotate("text", x = 23, y = max(dta$amplitude), 
                      label = paste('nb_bk: ',
                                    chat[line,'nb_bk'],'\n',
                                    'nb_bit: ',
                                    chat[line,'nb_bit']),
                      colour = 'orange', size = 5)
  ggplotly(p1)
}

draw_plot('39',200,0,25)
