
load("~/2020-2021/PROJET-INGE/son2.RData")
load("~/2020-2021/PROJET-INGE/data_agrocampus.RData")

library(plotly)
library(tidyverse)
library(ggplot2)

# indiv 70 : mâche bcp (4,34)
# indiv 118 : mâche peu (0,1)
# indiv 32, 34, 39, 40 : on entend bien

son <- model_data[, 7:1102506]
chat <- model_data[,1:6]

draw_plot <- function(line, pas, dep = 0, fin = 25){
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
    ylim(-30000,30000) +
    theme_minimal()
  
  p1 <- p1 + annotate("text", x = 22, y = 25000, 
                      label = paste('nb_bk: ',
                                    chat2[line,'nb_bk'],'\n',
                                    'nb_bit: ',
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

vec_lines <- c('1','34','70','118')
pas <- 1
dep <- 0
fin <- 25
rows <- 2
manygraphs(rows, vec_lines, pas, dep, fin)

