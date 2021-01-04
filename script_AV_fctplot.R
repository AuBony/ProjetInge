
load("../data/Mars.RData")
load("../data/noise_data_agrocampus.RData")

library(plotly)
library(tidyverse)
library(ggplot2)

chat2$nb_bk <- as.integer(chat2$nb_bk)
chat2$nb_bit <- as.integer(chat2$nb_bit)

# indiv 52 : mâche bcp (4,34)
# indiv 87 : mâche peu (0,1)
# indiv 22, 23, 27, 28 : on entend bien

draw_plot <- function(line, pas, dep = 0, fin = 25, ymin = -30000, ymax = 30000){
  dt <- 25/1102500
  dep <- dep
  fin <- fin/dt
  pas <- pas
  temps <- seq(from = dep, to = fin, by = pas)
  indice <- temps + 1
  dta <- data.frame(time = temps[-length(temps)]*dt, 
                    amplitude = as.integer(son2[line,temps]))
  # graphe
  p1 <- dta %>% ggplot() +
    aes(x = time, y = amplitude) +
    geom_line(size = 0.2) +
    ylim(ymin,ymax) +
    theme_minimal()
  
  p1 <- p1 + annotate("text", x = 22, y = 25000, 
                      label = paste('nb_bk:',
                                    chat2[line,'nb_bk'],'\n',
                                    'nb_bit:',
                                    chat2[line,'nb_bit']),
                      colour = 'orange', size = 4) +
    annotate("text", x = 3, y = 25000, 
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

vec_lines <- c(22, 28, 85, 87)
pas <- 100
dep <- 0
fin <- 25
rows <- 2
manygraphs(rows, vec_lines, pas, dep, fin)


###########################################
###### Notchi data
###########################################

pas <- 1
dep <- 0
fin <- 10
ymin <- -20000
ymax <- 20000
dt <- 25/1102500
fin <- fin/dt
temps <- seq(from = dep, to = fin, by = pas)
indice <- temps + 1

# Notchi 1
line <- 'notchi1'
dta <- data.frame(time = temps[-length(temps)]*dt, 
                  amplitude = as.integer(son2[line,temps]))
# graphe
p1 <- dta %>% ggplot() +
  aes(x = time, y = amplitude) +
  geom_line(size = 0.2) +
  ylim(ymin,ymax) +
  theme_minimal()

p1 <- p1 + annotate("text", x = 9, y = 18000, 
                    label = paste('nb_bk:', chat2[line,'nb_bk'],'\n',
                                 'nb_bit:', chat2[line,'nb_bit']),
                    colour = 'orange', size = 4) +
  annotate("text", x = 1, y = 18000, 
           label = 'Notchi1',
           colour = 'black', size = 5)
p1 <- ggplotly(p1)


# Notchi 2
line <- 'notchi2'
dta <- data.frame(time = temps[-length(temps)]*dt, 
                  amplitude = as.integer(son2[line,temps]))
# graphe
p2 <- dta %>% ggplot() +
  aes(x = time, y = amplitude) +
  geom_line(size = 0.2) +
  ylim(ymin,ymax) +
  theme_minimal()

p2 <- p2 + annotate("text", x = 9, y = 18000, 
                    label = paste('nb_bk:', chat2[line,'nb_bk'],'\n',
                                 'nb_bit:', chat2[line,'nb_bit']),
                    colour = 'orange', size = 4) +
  annotate("text", x = 1, y = 18000, 
           label = 'Notchi2',
           colour = 'black', size = 5)
p2 <- ggplotly(p2)

subplot(p1, p2)

###########################################
###### Noise data
###########################################

dt <- 25/1102500
dep <- 0
pas <- 200

# 1er bruit
data <- noise1[-c(1,2)]
fin <- 2998800

temps <- seq(from = dep, to = fin, by = pas)
indice <- temps + 1
dta <- data.frame(time = temps[-length(temps)]*dt, 
                  amplitude = as.integer(data[temps]))
p2 <- dta %>% ggplot() +
  aes(x = time, y = amplitude) +
  geom_line(size = 0.2) +
  ylim(-30000,30000) +
  xlim(0,70) +
  theme_minimal()
p2 <- p2 + annotate("text", x = 10, y = 25000, 
                    label = paste("Noise 1"),
                    colour = 'black', size = 5)
p2 <- ggplotly(p2)

# 2e bruit
data <- noise2[-c(1,2)]
fin <- 882000

temps <- seq(from = dep, to = fin, by = pas)
indice <- temps + 1
dta <- data.frame(time = temps[-length(temps)]*dt, 
                  amplitude = as.integer(data[temps]))
p3 <- dta %>% ggplot() +
  aes(x = time, y = amplitude) +
  geom_line(size = 0.2) +
  ylim(-30000,30000) +
  xlim(0,70) +
  theme_minimal()
p3 <- p3 + annotate("text", x = 10, y = 25000, 
                    label = paste("Noise 2"),
                    colour = 'black', size = 5)
p3 <- ggplotly(p3)

# 3e bruit
data <- noise3[-c(1,2)]
fin <- 1764002

temps <- seq(from = dep, to = fin, by = pas)
indice <- temps + 1
dta <- data.frame(time = temps[-length(temps)]*dt, 
                  amplitude = as.integer(data[temps]))
p4 <- dta %>% ggplot() +
  aes(x = time, y = amplitude) +
  geom_line(size = 0.2) +
  ylim(-30000,30000) +
  xlim(0,70) +
  theme_minimal()
p4 <- p4 + annotate("text", x = 10, y = 25000, 
                    label = paste("Noise 3"),
                    colour = 'black', size = 5)
p4 <- ggplotly(p4)

# 5e bruit
data <- noise5[-c(1,2)]
fin <- 1764002

temps <- seq(from = dep, to = fin, by = pas)
indice <- temps + 1
dta <- data.frame(time = temps[-length(temps)]*dt, 
                  amplitude = as.integer(data[temps]))
p5 <- dta %>% ggplot() +
  aes(x = time, y = amplitude) +
  geom_line(size = 0.2) +
  ylim(-30000,30000) +
  xlim(0,70) +
  theme_minimal()
p5 <- p5 + annotate("text", x = 10, y = 25000, 
                    label = paste("Noise 5"),
                    colour = 'black', size = 5)
p5 <- ggplotly(p5)

subplot(p2, p3, p4, p5, nrows = 2)
