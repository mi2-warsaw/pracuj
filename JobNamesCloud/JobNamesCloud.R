# loading libs
library("readr", lib.loc="D:/Program Files/R/R-devel/library")
library("dplyr", lib.loc="~/R/win-library/3.3")
library("data.table", lib.loc="D:/Program Files/R/R-devel/library")
library("utils", lib.loc="D:/Program Files/R/R-devel/library")

# switching dir to get some data
setwd("../Filtr")

# reading needed datasets
pracuj_filtered <- read_csv("pracuj_filtered.csv" , col_names = TRUE)
needed_complete_phrases <- read_csv("needed_complete_phrases.csv" , col_names = TRUE)

# going back home
setwd("../JobNamesCloud")

