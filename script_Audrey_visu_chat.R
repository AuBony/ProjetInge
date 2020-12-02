# ProJET INGENIEUR
# Visualisation des données Chat
# Audrey Bony
# 2/12/2020


############################
# Visualisation exp?rience #
############################
#Distribution du nombre de machage
hist(chat$nb_bit)

#Distribution du nombre de croquage
hist(chat$nb_bk)

#Nombre de croquage et nombre de mâchage 
library(plotly)
plot_ly(chat, x = ~nb_bit, y = ~nb_bk, text = rownames(chat),
        type = 'scatter',
        mode = 'markers',
        marker = list(size = 10,color = 'rgba(255, 182, 193, .9)',
                      line = list(color = 'rgba(152, 0, 0, .8)',
                                  width = 2)))
