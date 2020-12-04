# ProJET INGENIEUR
# Visualisation des données Chat
# Audrey Bony
# 2/12/2020

par(mfrow = c(1,1))
############################
# Visualisation exp?rience #
############################

#Distribution du nombre de machage
hist(chat2$nb_bk, xlab = "", main = "Histogramme des croquages")

#Distribution du nombre de croquage
hist(chat2$nb_bit, xlab = "", main = "Histogramme des mâchages")

#Nombre de croquage et nombre de mâchage 
library(plotly)
plot_ly(chat2, x = ~nb_bit, y = ~nb_bk, text = rownames(chat2),
        type = 'scatter',
        mode = 'markers',
        marker = list(size = 10,color = 'rgba(255, 182, 193, .9)',
                      line = list(color = 'rgba(152, 0, 0, .8)',
                                  width = 2)))
