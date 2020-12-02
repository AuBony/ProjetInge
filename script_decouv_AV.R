#load("../data/data_agrocampus.RData")
load("~/2020-2021/PROJET-INGE/data_agrocampus.RData")

library(plotly)
library(tidyverse)
library(ggplot2)

son <- model_data[, 7:1102506]
chat <- model_data[,1:6]
toy_data <- model_data[c('1','10','32','34','39','40'),]

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

draw_plot <- function(line, pas, dep = 0, fin = 25){
  dt <- 25/1102500
  dep <- dep
  fin <- fin/dt
  pas <- pas
  temps <- seq(from = dep, to = fin, by = pas)
  indice <- (temps + 1)[-length(indice)]
  dta <- data.frame(time = temps[-length(temps)]*dt, 
                    amplitude = as.integer(son[line,temps]))
  
  # graphe
  p1 <- dta %>% ggplot() +
    aes(x = time, y = amplitude) +
    ylim(-25000,25000) +
    geom_line(size = 0.2) +
    theme_minimal()
  
  p1 <- p1 + annotate("text", x = 22, y = max(dta$amplitude), 
                      label = paste('nb_bk: ',
                                    chat[line,'nb_bk'],'\n',
                                    'nb_bit: ',
                                    chat[line,'nb_bit']),
                      colour = 'orange', size = 4) +
    ggtitle(paste("Line ",line))
  ggplotly(p1)
}

draw_plot('32',200,0,25)


# indiv 70 : mâche bcp (4,34)
# indiv 105 : mâche peu (0,1)

line <- c('32','34')
dt <- 25/1102500
dep <- 0
fin <- 25/dt
pas <- 500
temps <- seq(from = dep, to = fin, by = pas)
indice <- (temps + 1)
indice <- indice[-length(indice)]
tps <- rep(indice,length(line))
amp <- c()
lines <- c()
for (i in 1:length(line)){ 
  amp <- c(amp, as.integer(son[line,indice][i,]))
  lines <- c(lines,rep(line[i],length(indice)))
}

dta <- data.frame(time = tps*dt, 
                  amplitude = amp,
                  line = as.factor(lines))

# graphe
p1 <- ggplot(dta, aes(x = time, y = amplitude)) +
  ylim(-25000,25000) +
  geom_line(size = 0.2) +
  theme_minimal()

p1 + facet_grid(.~lines)

p1 + annotate("text", x = 22, y = max(dta$amplitude), 
                    label = paste('nb_bk: ',
                                  chat[line,'nb_bk'],'\n',
                                  'nb_bit: ',
                                  chat[line,'nb_bit']),
                    colour = 'orange', size = 4) +
  ggtitle(paste("Line ",line))
ggplotly(p1) 


############################################
############################################

draw_plot <- function(line, pas, dep = 0, fin = 25){
  dt <- 25/1102500
  dep <- dep
  fin <- fin/dt
  pas <- pas
  temps <- seq(from = dep, to = fin, by = pas)
  indice <- temps + 1
  dta <- data.frame(time = temps[-length(temps)]*dt, 
                    amplitude = as.integer(son[line,temps]))
  print(paste('nb_bk: ',
              chat[line,'nb_bk'],'\n',
              'nb_bit: ',
              chat[line,'nb_bit']))
  # graphe
  p1 <- dta %>% ggplot() +
    aes(x = time, y = amplitude) +
    geom_line(size = 0.2) +
    ylim(-25000,25000) +
    theme_minimal()
  
  p1 <- p1 + annotate("text", x = 22, y = 20000, 
                      label = paste('nb_bk: ',
                                    chat[line,'nb_bk'],'\n',
                                    'nb_bit: ',
                                    chat[line,'nb_bit']),
                      colour = 'orange', size = 4) +
    annotate("text", x = 3, y = 24000, 
             label = paste("Line ",line),
             colour = 'black', size = 5)
  ggplotly(p1)
}

manygraphs <- function(rows, vec_lines, pas, dep, fin){
  p1 <- draw_plot(vec_lines[1], pas, dep, fin)
  p2 <- draw_plot(vec_lines[2], pas, dep, fin)
  p3 <- draw_plot(vec_lines[3], pas, dep, fin)
  p4 <- draw_plot(vec_lines[4], pas, dep, fin)
  subplot(p1, p2, p3, p4, nrows = rows)
}

vec_lines <- c('1','34','70','105')
pas <- 200
dep <- 0
fin <- 25
rows <- 2
manygraphs(rows, vec_lines, pas, dep, fin)

