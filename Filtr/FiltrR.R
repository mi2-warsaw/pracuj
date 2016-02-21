#loading libs
library("dplyr", lib.loc="D:/Program Files/R/R-devel/library")
library("readr", lib.loc="D:/Program Files/R/R-devel/library")
library("utils", lib.loc="D:/Program Files/R/R-devel/library")
library("data.table", lib.loc="D:/Program Files/R/R-devel/library")


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
  
  
  
  # changing dir to get dicts
  setwd("dict")
  
  # listing all avaliable dictionaries
  f_dic_names <- list.files("../dict", pattern="*.csv", full.names=FALSE)

  
  
  
  dic_list <- list()
  
  
  #reading all avaliable dictionaries
  for (dic in f_dic_names) {
   
   
   dic_list_i <- read_csv(paste0(dic) , col_names = TRUE)
   dic_list <- append(dic_list, dic_list_i)  
  }
  
  # going back to ../
  setwd("../")
  
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
  omited_data <- data.frame()
  for (NCP in needed_complete_phrases)  {
    filtered_data1 <- mutate(pracuj_data, DSIndicator = grepl(paste0(".*",NCP,".*"), href) )%>% filter(DSIndicator == TRUE)%>%mutate(JobName = paste0(NCP)) 
    filtered_data <- rbind(filtered_data, filtered_data1)
  }
  
   
    nonDS_primarily_omited_data <- mutate(pracuj_data, DSIndicator = grepl(paste0(".*",NCP,".*"), href) )%>%filter(DSIndicator == FALSE)
 
    

  
  
  # excluding offers containing phrases which indicate non-data.science affiliation 
    nonDS_exeptions_omited_data <- data.frame()
  for (EP in exeptions_phrases)
  {
    filtered_data <- mutate(filtered_data, ExeptionIndicator = grepl(paste0(".*",EP,".*"), href) )
    nonDS_exeptions_omited_data1 <- filter(filtered_data, ExeptionIndicator == TRUE)
    filtered_data <-filter(filtered_data, ExeptionIndicator == FALSE)
    nonDS_exeptions_omited_data <- rbind(nonDS_exeptions_omited_data, nonDS_exeptions_omited_data1)
    }

  
  
  
  # removing "Indicators" from dataset
  filtered_data <- select(filtered_data, -contains("Indicator"))%>%arrange(desc(date))
  nonDS_primarily_omited_data <- select(nonDS_primarily_omited_data, -contains("Indicator"))%>%arrange(desc(date))
  nonDS_exeptions_omited_data <- select(nonDS_exeptions_omited_data, -contains("Indicator"))%>%arrange(desc(date))
  
  
  # same ID killer
  filtered_data <- as.data.table(filtered_data)
  setkey(filtered_data, id)
  filtered_data <- filtered_data[!duplicated(filtered_data),]
  filtered_data <- as.data.frame(filtered_data)
  
  
  
  # writing solution to file
  write_csv(filtered_data, "pracuj_filtered.csv")
  write_csv( nonDS_exeptions_omited_data, "nonDS_exeptions_omited_data.csv")
  write_csv(nonDS_primarily_omited_data,  "nonDS_primarily_omited_data.csv")
  
  needed_complete_phrases <- as.data.frame(needed_complete_phrases)
  write_csv(needed_complete_phrases, "needed_complete_phrases.csv")
  
  exeptions_phrases <- as.data.frame(exeptions_phrases)
  write_csv(exeptions_phrases, "exeptions_phrases.csv")
  