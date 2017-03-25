library(rvest)
library(xml2)
library(dplyr)
library(readr)
library(RPostgreSQL)

##### SCRIPT #####

# define amount of pages to scrap
nOfPages <- 100
mainPercentage <- c()
subPercentage <- c()

password <- ""

polaczenie <- dbConnect(
  PostgreSQL(),
  dbname = "pracuj",
  user = "pracuj",
  password = password,
  host = "services.mini.pw.edu.pl"
)


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
    NA
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

# Trim whitespaces and remove single quotation marks
adjustString <- function(string) {
  string %>%
    gsub("'", " ", .) %>%
    trimws()
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
    script[.] %>%
    trimws()
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

# Check which offers are already in DB
isIDinDB <- function(hrefVect) {
  hrefVect %>%
    unique() %>%
    data_frame() %>%
    setNames("href") %>%
    mutate(
      href = gsub("\\+", "", href),
      id = href %>%
        gsub("[[:digit:]]*$", "", .) %>%
        mapply(
          function(x, y) {
            gsub(x, "", y)
          }, ., href
        )
    ) %>%
    filter(!(id %in% dbIDs$id))
}

#maxid <- dbGetQuery(polaczenie, "SELECT max(data) FROM offers")[1,1]
total <- 0
indiv_ID <- c(0,0,0)
indiv_ID_TF <- c(FALSE, FALSE, FALSE)
indiv_ID_df <- data.frame(indiv_ID, indiv_ID_TF)

jobs_names <- c("id", "employer", "position", "grade", "location", "date", "description")

jobs <- data_frame()
jobs_1 <- data_frame()

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
      sqmSub()

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

# Get current ids from DB
dbIDs <- dbGetQuery(polaczenie, "SELECT id FROM offers")

links <- c()

for (i in 1:nOfPages) {
  mainPercentage <- i/nOfPages*100

  scrappedPage <- read_html(paste0("http://www.pracuj.pl/praca?pn=", i))
  scrappedNodes <- html_nodes(scrappedPage, css = "#mainOfferList .o-list_item_link_name")

  if (length(scrappedNodes) == 0) {break}

  links <- c(
    links,
    html_attr(scrappedNodes, "href") %>%
      na.omit(links)
  )

  print(
    paste0(
      "scrapped pages: ", format(round(mainPercentage, 2), nsmall = 2), "%"
    )
  )
}

idLinks <- isIDinDB(links)

for (i in 1:nrow(idLinks)) {
  # current progress
  subPercentage <- i/nrow(idLinks)*100

  # Getting link ready
  href <- paste0("http://www.pracuj.pl", idLinks$href[i])

  # Reading offer ID
  id <- idLinks$id[i]

  # Reading link
  currentLinkSource <- read_html(href)

  # Reading employer name
  employer <- html_nodes(
    currentLinkSource,
    css = ".emplo_cnt_link .emplo_cnt_link_text"
  ) %>%
    html_text() %>%
    adjustString()

  # Reading job name
  position <- html_nodes(
    currentLinkSource,
    css = "#offerTitle"
  ) %>%
    html_text() %>%
    adjustString()

  # Reading job grade
  grade <- html_nodes(
    currentLinkSource,
    css = ".ico-briefcase"
  ) %>%
    html_text() %>%
    adjustString()

  # Reading locations
  location <- html_nodes(
    currentLinkSource,
    css = ".latlng"
  ) %>%
    html_text() %>%
    gsub("pokaż mapę", "", .) %>%
    adjustString()

  # Reading offer details
  description <- html_nodes(
    currentLinkSource,
    css = "#offCont"
  ) %>%
    html_text() %>%
    gsub("\n", " ", .) %>%
    gsub("\r", " ", .) %>%
    gsub("\t", " ", .) %>%
    gsub("'", " ", .) %>%
    gsub(
      "([[:alpha:]])([[:lower:]])([[:blank:]])?([[:punct:]]?)([[:blank:]])?([[:upper:]])",
      "\\1\\2 \\6",
      .
    ) %>%
    trimws()

  # Reading date of the offer announcement
  date <- html_nodes(
    currentLinkSource,
    css = ".ico-time .o-main__right_offer_cnt_details_item_text_ico+ span"
  ) %>%
    html_text() %>%
    adjustString()

  # Reading scripts with categories
  scripts <- html_nodes(
    currentLinkSource,
    css = "script"
  )

  if (length(scripts) >= 7) {
    main_category <- getCategories(scripts, "offerData")
    sub_category <- getCategories(scripts, "soc_product")
  } else {
    main_category <- NA
    sub_category <- NA
  }

  # Reading salary
  salary <- html_nodes(
    currentLinkSource,
    css = ".ico-money"
  ) %>%
    html_text()

  if (length(salary) == 0) {
    salary <- NA
  } else {
    salary <- salary %>%
      adjustString()
  }

  # Checking contract
  contract <- checkContract(id)

  # Handling hidden informations
  zeroLengthKiller <- list(
    id, employer, position, grade, location, date, description
  )
  changeIndicator <- c()

  for (index in 1:length(zeroLengthKiller)) {
    if(length(zeroLengthKiller[[index]]) == 0) {
      zeroLengthKiller[[index]] <- c("Rekrutacja ukryta")
      changeIndicator <- c(changeIndicator, index)
    }
  }

  for (varName in changeIndicator) {
    assign(paste0(jobs_names[varName]), zeroLengthKiller[[varName]])
  }

  jobs_1 <- data_frame(
    id, employer, position, grade, location, date, description, href,
    main_category, sub_category, salary, contract
  )
  jobs <- rbind(jobs, jobs_1)

  # Current progress
  print(
    paste0(
      "scrapped offers: ", format(round(subPercentage, 2), nsmall = 2), "%"
    )
  )

  # Insert into DB
  dbGetQuery(
    polaczenie,
    paste0(
      "INSERT INTO offers (id, href, position, date, location, grade, employer, description, main_category, sub_category, salary, contract) VALUES ('",
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
      "')"
    )
  )

  # Confirm load
  print(paste0("offer of id:   ", id, "   loaded into DB"))
}
