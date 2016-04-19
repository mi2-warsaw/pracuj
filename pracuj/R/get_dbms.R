#' Collecting DBMS popularity data
#'
#' Function \code{get_dbms} scrapes data from db-engines.com
#'
#' @usage get_dbms(type = c("all", Content store", "Document store", "Event Store", "Graph DBMS", "Key-value store", "Multivalue DBMS", "Native XML DBMS", "Navigational DBMS", "Object oriented DBMS", "RDF store", "Relational DBMS", "Search engine", "Time Series DBMS", "Wide column store"))
#'
#' @param type of DBMS; default: "all"
#'
#' @return data frame
#'
#' @examples
#' \dontrun{
#' # Top 10 Relational Database Management Systems
#' dbms <- get_dbms("Relational DBMS")
#' head(dbms, 10)
#' }
#' @author Krzysztof Słomczyński
#'
#' @export

get_dbms <- function(type = 'all') {
  stopifnot(is.character(type))
  
  # scrape data
  page <- read_html("http://db-engines.com/en/ranking")
  nameNodes <- html_nodes(page, css=".pad-r+ th")
  modelNodes <- html_nodes(page, css="th.small")
  scoreNodes <- html_nodes(page, css="th+ .pad-l")
  
  # create data frame
  dbms <- data.frame(name = html_text(nameNodes),
                     model = html_text(modelNodes),
                     score = as.numeric(html_text(scoreNodes)),
                     stringsAsFactors=F)
  
  # clear data
  dbms <- dbms %>%
    mutate(name = gsub("Detailed vendor-provided information available",
                       "",
                       name))
  
  # filter multi-model positions
  multiModel <- dbms %>%
    filter(grepl(",", model)) %>%
    mutate(model = gsub("Multi-model ", "", model))
  
  # create list of models from multi-models positions
  modelList <- strsplit(multiModel$model, ",")
  modelList <- setNames(modelList, multiModel$name)
  
  # delete multi-models positions from data frame
  dbms <- dbms[-grep(",", dbms$model), ]
  
  # add multi-models positions to data frame as single ones
  for (i in 1:length(modelList)) {
    for (j in 1:length(modelList[[i]])) {
      dbms[nrow(dbms)+1, "name"] <- names(modelList)[i]
      dbms[nrow(dbms), "model"] <- modelList[[i]][j]
      dbms[nrow(dbms), "score"] <- multiModel$score[i]
    }
  }
  
  # arrange data
  dbms <- dbms %>%
    arrange(desc(score))
  
  # optional type
  if (type != "all") {
    dbms <- dbms %>%
      filter(model == type)
  }
  
  return(dbms)
}