library(rvest)
library(xml2)
library(dplyr)
library(readr)
library(RPostgreSQL)

# test

###### FUNCTIONS ######

# Paste collected IDs
pasteIDs <- function(jfType) {
  paste0(jfType, offersIDs, ",")
}

# Check which contract type does fit the id
checkContract <- function(id) {
  if (grepl(id, jfList$full)) {
    "Pełny etat"
  } else if (grepl(id, jfList$part)) {
    "Część etatu"
  } else if (grepl(id, jfList$temporary)) {
    "Praca czasowa"
  } else if (grepl(id, jfList$contract)) {
    "Kontrakt"
  } else {
    ""
  }
}

# Convert signs
signsConverter <- function(string, signsList) {
  for (i in 1:length(signsList)) {
    string <- string %>%
      gsub(names(signsList)[[i]], signsList[[i]], ., fixed = TRUE)
  }
  string
}

# Strip single quotation marks
sqmSub <- function(string) {
  string %>%
    gsub("^[^']*'", "", .) %>%
    gsub("'.*", "", .)
}

# Collect main and sub categories
getCategories <- function(scriptNodes, funPhrase) {
  valPhrase <- switch(
    funPhrase,
    offerData = "categoryNames:",
    soc_product = "category:"
  )
  script <- scriptNodes %>%
    grep(funPhrase, .) %>%
    scriptNodes[.] %>%
    html_text() %>%
    signsConverter(signs2Convert) %>%
    strsplit("\n") %>%
    unlist()
  phrase <- script %>%
    grep(valPhrase, .) %>%
    script[.]
  switch(
    funPhrase,
    offerData = phrase %>%
      sqmSub() %>%
      strsplit(", ") %>%
      unlist() %>%
      unique() %>%
      paste0(collapse = ", "),
    soc_product = phrase %>%
      strsplit(",") %>%
      unlist() %>%
      tail(1) %>%
      sqmSub()
  )
}

##### SCRIPT #####

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

#maxid <- dbGetQuery(polaczenie, "SELECT max(data) FROM offers")[1,1]
total <- 0
indiv_ID <- c(0,0,0)
indiv_ID_TF <- c(FALSE, FALSE, FALSE)
indiv_ID_df <- data.frame(indiv_ID, indiv_ID_TF)

jobs_names <- c("id", "employer", "position", "grade", "location", "date", "description")


jobs <-data.frame()
jobs_1 <- data.frame()

# Matched signs
signs2Convert <- list("ó", "ó", "/") %>%
  setNames(c("&#243;", "\\u00f3", "\\x2f"))

# There are four different types of contracts
jf <- c(1:4)
jfList <- list(c(), c(), c(), c()) %>%
  setNames(c("full", "part", "temporary", "contract"))

# Filter offers by type of contract
for (i in jf) {
  jfLink <- paste0("http://www.pracuj.pl/praca?jf=", i)
  
  # Collect IDs for given type
  for (j in 1:nOfPages) {
    pnPage <- read_html(paste0(jfLink, "&pn=", j))
    pnNode <- html_nodes(pnPage, css = "script")
    nodeID <- grep("offersOnListForApplied", pnNode)
    offersIDs <- pnNode[nodeID] %>%
      html_text() %>%
      gsub("^[^']*'", "", .) %>%
      gsub("'.*", "", .)
    
    if (offersIDs == "") {
      break
    }
    
    switch(
      i,
      "1" = jfList$full <- pasteIDs(jfList$full),
      "2" = jfList$part <- pasteIDs(jfList$part),
      "3" = jfList$temporary <- pasteIDs(jfList$temporary),
      "4" = jfList$contract <- pasteIDs(jfList$contract)
    )
  }
}

for (i in 1:nOfPages){

mainPercentage <- i/nOfPages*100


scrappedPage <- read_html(paste0("http://www.pracuj.pl/praca?pn=",i))
scrappedNodes <- html_nodes(scrappedPage, css = "#mainOfferList .o-list_item_link_name")

#quit parsing

if(length(scrappedNodes) == 0)
{break}

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
        employer <- html_nodes(currentLinkSource, css = ".o-top__cnt_main_emplo-inline span") %>% html_text()
        employer <- gsub(pattern = "'", replacement =  " ", employer)
    
        # reading job name
        position <- html_nodes(currentLinkSource, css = ".o-top__cnt_main_job") %>% html_text()
        position <- gsub(pattern = "'", replacement =  " ", position)

    
        # reading job grade
        grade <- html_nodes(currentLinkSource, css = ".ico-briefcase") %>% html_text()
        # removing "\n" markers from char
        grade <- gsub(pattern = "\n", replacement = " ", grade )
        grade <- gsub(pattern = "'", replacement =  " ", grade)
    
        # reading locations
        location <- html_nodes(currentLinkSource, css = ".latlng span") %>% html_text()
        # obsolete
        # location <- gsub(pattern = "'", replacement =  " ", location)
        location <- paste0(location, collapse = "")
    
        # reading offer details
        description <- html_nodes(currentLinkSource, css = "#offCont") %>% html_text()
        description <- gsub(pattern = "\n", replacement = " ", description)
        description <- gsub(pattern = "\r", replacement = " ", description)
        description <- gsub(pattern = "\t", replacement = " ", description)
        description <- gsub(pattern = "'", replacement =  " ", description)
        description <- gsub(pattern = "([[:alpha:]])([[:lower:]])([[:blank:]])?([[:punct:]]?)([[:blank:]])?([[:upper:]])", replacement = "\\1\\2 \\6", description)
    
        # reading date of the offer announcment
        date <- html_nodes(currentLinkSource, css = ".ico-time .o-top__cnt_main_details_item_text_ico+ span") %>% html_text()
    
        #reading scripts from pracuj.pl
        scripts <- html_nodes(currentLinkSource, css = "script")
        
        if (length(scripts) >= 7) {
          main_category <- getCategories(scripts, "offerData")
          sub_category <- getCategories(scripts, "soc_product")
        } else {
          main_category <- ""
          sub_category <- ""
        }
        
        #reading salary
        salary <- html_nodes(currentLinkSource, css = ".o-top__cnt_main_details_item_text.ico-money") %>%
                  html_text()
        
        if(length(salary) == 0){
          
         salary <- ""
          
        } else {
          
          salary <- gsub(pattern = "[[:space:]]{0,}\n", replacement = "", salary)
          
        }
        
        # Checking contract
        contract <- checkContract(id)
        
    
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
               paste0("INSERT INTO offers (id, href, position, date, location, grade, employer, description, main_category, sub_category, salary, contract) VALUES ('",
                      id,"','",
                      href,"','",
                      position,"','",
                      date,"','",
                      location,"','",
                      grade,"','",
                      employer,"','",
                      description,"','",
                      main_category,"','",
                      sub_category,"','",
                      salary,"','",
                      contract,
                      "')"))
        
    }    
  }

}

#write_csv(jobs, "jobs.csv")

