#loading libs
library("dplyr", lib.loc="D:/Program Files/R/R-devel/library")
library("readr", lib.loc="D:/Program Files/R/R-devel/library")
library("utils", lib.loc="D:/Program Files/R/R-devel/library")


#swapping dir to get some data
setwd("../crawler")

#importing csv (to be ignored)

      # building up the filename
    
        #scanning ../crawler for names
        #f_name <- list.files("../crawler", pattern="*.csv", full.names=FALSE) %>%
    
        #extracting date from f_names in numeric format -> YYMMDD
        #gsub(pattern = "jobs\\.*\\.csv$",replacement = "", f_names) %>%
    
        #selecting latest dataset
        #max(na.rm = TRUE)
  
      #exeptions to be considered:
      # - YY, MM and DD shouldn't exceed current date
      # - MM shouldn't exceed 12
      # - DD shouldn't exceed number of days specific for a given MM
      # solution - change string to date?


    
  #actual imported data without "NA" elements
  pracuj_data <- read_csv("pracuj.csv" , col_names = TRUE) %>%
  filter(position != "NA")
  
  
  # switching back to filtering directory
  setwd("../Filtr")
  
  # creating dictionary forjob offers selection
  
  #created dictionaries:
  # - phrase_dic_eng.csv contains summary of phrases in english which determine if
  #   the job offer belongs to data.science category
  # - phrase_dic_pl.csv contains summary of phrases in polish which determine
  #   if the job offer belongs to data.science category
  # - exeptions_phrase_eng.csv, exeptions_phrase_pl.csv contain expressions which indicate job offer outside data.science industry
  #
  # - exeptions_words_eng.csv, exeptions_words_pl.csv -- || -- (words instead of full expressions)  
  
  
  
  # listing all avaliable dictionaries
  f_dic_names <- list.files("../Filtr", pattern="*.csv", full.names=FALSE)
  f_dic_names <- subset(f_dic_names, subset = (f_dic_names != "pracuj_filtered.csv" &  f_dic_names != "jobs.csv")) %>%
                 as.list()
  
  dic_list <- list()
  
  
  
  
  #reading all avaliable dictionaries
  for (dic in f_dic_names) {
   
   
   dic_list_i <- read_csv(paste0(dic) , col_names = TRUE)
   dic_list <- append(dic_list, dic_list_i)  
  }
  
  #creating propper vector names for phrases extraction from dictionaries to vectors
  f_dic_names <- lapply(f_dic_names, function (x) {gsub(pattern = "\\.*\\.csv$",replacement = "", x)})
  
  
  # naming vector of dictionaries names
  names(dic_list) <- f_dic_names
  
  

  

  
  
# Propper filtering
  
# 'href' selected for filtering due to universal structure .../position-name-city

  
  # creating vector used to filter interesting offers
  needed_complete_phrases <- unlist(dic_list[grep(pattern = ".*\\phrase_dic\\.*", names(dic_list))], use.names = FALSE)
  
  # creating vector used to filter out exeptions (offers containing "data-analyst" etc, but not in data.science industry)
  exeptions_phrases <- unlist(dic_list[grep(pattern = ".*exeptions_phrase\\.*", names(dic_list))], use.names = FALSE)
  


  # filtering according to phrases normally indicating data.science industry job   
  filtered_data <- data.frame()
  for (NCP in needed_complete_phrases)  {
    filtered_data1 <- mutate(pracuj_data, DSIndicator = grepl(paste0(".*",NCP,".*"), href) )%>% filter(DSIndicator == TRUE) 
    filtered_data <- rbind(filtered_data, filtered_data1)
  }
  
  
  # excluding offers containing phrases which indicate non-data.science affiliation 
  for (EP in exeptions_phrases)
  {
  filtered_data <- mutate(filtered_data, ExeptionIndicator = grepl(paste0(".*",EP,".*"), href) )%>% filter(ExeptionIndicator == FALSE)
  
  }
 
  # removing "Indicators" from dataset
  filtered_data <- select(filtered_data, -contains("Indicator"))%>%arrange(desc(date))
  
  # writing solution to file
  write_csv(filtered_data, "pracuj_filtered.csv")
  