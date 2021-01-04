# PROJET INGENIEUR
# Caractérisation des sons
# Audrey Bony
# 4/01/2021

# Solution dplyr (source : https://clauswilke.com/blog/2016/06/13/reading-and-combining-many-tidy-data-files-in-r/)----
require(readr)  # for read_csv()
require(dplyr)  # for mutate()
require(tidyr)  # for unnest()
require(purrr)  # for map(), reduce()

data_path <- "../data/data_perso/labels/"
files <- dir(data_path, pattern = "*.txt")
files

data <- data_frame(filename = files) %>%
  mutate(file_contents = map(filename,         
                             ~ read_delim(file.path(data_path, .),
                                            delim="\t",
                                            escape_double = FALSE,
                                            col_names = c("start", "end", "annotation"),
                                            trim_ws = TRUE)))

data_modif <- unnest(data, cols = c(file_contents))

df <- data_modif %>% 
  mutate(chat = as.character(map(strsplit(df$filename, "_"), 1)), 
         kibble = as.character(map(strsplit(df$filename, "_"), 2)))


# Solution sans le nom des fichiers ----
multmerge <- function(mypath = getwd()){
  require(dplyr)
  require(readr)
  dataset <- list.files(path=mypath,
                        full.names=TRUE, 
                        pattern="*.txt") %>% 
    lapply(read_delim, 
           delim="\t", 
           escape_double = FALSE, 
           col_names = c("start", "end", "annotation"), 
           trim_ws = TRUE) %>% 
    bind_rows()
  dataset
}

df_part <- multmerge("../data/data_perso/labels/")
df_part
