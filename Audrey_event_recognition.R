# PROJET INGENIEUR
# Identification des évènements
# Audrey Bony
# 12/01/2021

############# DATASET #############

df_feature <- read.table("data/data_perso/features/df_feature_01_12.txt")

# MAJ du DATASET ----


df_feature
table(df_feature$filename, df_feature$annotation)

#############

#############