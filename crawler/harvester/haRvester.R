library(rvest)
library(xml2)
library(dplyr)
library(readr)
library(RPostgreSQL)



# define amount of pages to scrap
nOfPages <- 100
mainPercentage <- c()
subPercentage <- c()


dbname = "pracuj"
user = "pracuj"
password = ""
host = "services.mini.pw.edu.pl"


sterownik <- dbDriver("PostgreSQL")
polaczenie <- dbConnect(sterownik, dbname = dbname, user = user, password = password, host = host)

maxid <- dbGetQuery(polaczenie, "SELECT max(data) FROM offers")[1,1]
total <- 0
indiv_ID <- c(0,0,0)
indiv_ID_TF <- c(FALSE, FALSE, FALSE)
indiv_ID_df <- data.frame(indiv_ID, indiv_ID_TF)

jobs_names <- c("id", "employer", "position", "grade", "location", "date", "description")

jobs <-data.frame()
jobs_1 <- data.frame()

for (i in 1:nOfPages){

mainPercentage <- i/nOfPages*100


scrappedPage <- read_html(paste0("http://www.pracuj.pl/praca?pn=",i))
scrappedNodes <- html_nodes(scrappedPage, css = "#mainOfferList .offer__list_item_link_name")
links <- html_attr(scrappedNodes, "href")
links <- na.omit(links)

  for (j in 1:length(links)) {
    # current progress
    subPercentage <- (j/length(links) *100/nOfPages) + mainPercentage - 100/nOfPages
    print(paste0("scrapped: ", format(round(subPercentage, 2), nsmall = 2), "%"))
    
    
    
    
    # getting link ready
    currentLink <- paste0("http://www.pracuj.pl", links[j])
    
    
    # reading offer ID
    id <- gsub(pattern = gsub(pattern = "([[:digit:]]*)$", replacement = "", currentLink), replacement = "", currentLink)
    
    czyjest <- as.numeric( dbGetQuery(polaczenie, paste0("SELECT count(*) FROM offers where id = '",id,"'")))
    
    #scrappingKiller per page
    indiv_ID_df$indiv_ID[1]<- indiv_ID_df$indiv_ID[2]
    indiv_ID_df$indiv_ID_TF[1] <- indiv_ID_df$indiv_ID_TF[2]
    
    indiv_ID_df$indiv_ID[2]<- indiv_ID_df$indiv_ID[3] 
    indiv_ID_df$indiv_ID_TF[2] <- indiv_ID_df$indiv_ID_TF[3]
    
    indiv_ID_df$indiv_ID[3]<- indiv_ID_df$indiv_ID[3] + 1
    if (czyjest == 0) {
      indiv_ID_df$indiv_ID_TF[3] <- FALSE
      
    } else if (czyjest > 0){
      indiv_ID_df$indiv_ID_TF[3] <- TRUE
    }
    
    if (all(indiv_ID_df$indiv_ID_TF) == TRUE) {break}
    
    
    
    if (czyjest == 0) {
        print(paste0("offer of id:   ", id, "   loaded into DB"))
        #scrapping link
        currentLinkSource <- read_html(currentLink)
    
        # reading employer name
        employer <- html_nodes(currentLinkSource, css = ".offerTop__cnt_main_emplo-inline") %>% html_text()
        employer <- gsub(pattern = "'", replacement =  " ", employer)
    
        # reading job name
        position <- html_nodes(currentLinkSource, css = ".offerTop__cnt_main_job") %>% html_text()
        position <- gsub(pattern = "'", replacement =  " ", position)

    
        # reading job grade
        grade <- html_nodes(currentLinkSource, css = ".offerTop__cnt_main_details_item--second") %>% html_text()
        # removing "\n" markers from char
        grade <- gsub(pattern = "\n", replacement = "", grade )
        grade <- gsub(pattern = "'", replacement =  " ", grade)
    
        # reading locations
        location <- html_nodes(currentLinkSource, css = ".offerTop__cnt_main_details_item:nth-child(1)") %>% html_text()
        location <- gsub(pattern = "'", replacement =  " ", location)
    
        # reading offer details
        description <- html_nodes(currentLinkSource, css = "#offCont") %>% html_text()
        description <- gsub(pattern = "\n", replacement = " ", description)
        description <- gsub(pattern = "\r", replacement = "", description)
        description <- gsub(pattern = "\t", replacement = "", description)
        description <- gsub(pattern = "'", replacement =  " ", description)
    
        # reading date of the offer announcment
        date <- html_nodes(currentLinkSource, css = ".ico-time .offerTop__cnt_main_details_item_text_ico+ span") %>% html_text()
    
    
        zeroLengthKiller <- list(id, employer, position, grade, location, date, description)
        changeIndicator <- c()
    
        for (index in 1:length(zeroLengthKiller)) {
        
            if(length(zeroLengthKiller[[index]])==0){
            zeroLengthKiller[[index]] <- c("Rekrutacja ukryta")
            changeIndicator <- c(changeIndicator, index)
            }
          }
    
        for (varName in changeIndicator){
            assign(paste0(jobs_names[varName]), zeroLengthKiller[[varName]])
          }
    
        href <- currentLink
        jobs_1 <- data.frame(id, employer, position, grade, location,date,description, href)
        jobs <- rbind(jobs, jobs_1)
    
    
        dbGetQuery(polaczenie, 
               paste0("INSERT INTO offers (id, href, position, date, location, grade, employer, description) VALUES ('",
                      id,"','",
                      href,"','",
                      position,"','",
                      date,"','",
                      location,"','",
                      grade,"','",
                      employer,"','",
                      description,
                      "')"))
        
    }    
  }

}

#write_csv(jobs, "jobs.csv")

