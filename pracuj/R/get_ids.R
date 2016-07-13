#' Geting indexes of offers that match selected criteria
#'
#' Function \code{get_ids} returns ids of offers that match selected criteria.
#'
#' @param offers vector with offers descriptions
#' @param patterns patterns to be found in descriptions
#' @param case should the pattern be identified case sensitive?
#'
#' @return data frame
#'
#' @examples
#' \dontrun{
#' offers <- get_offers(description=TRUE)
#' get_ids(offers$description)
#' }
#' @author Przemyslaw Biecek
#'
#' @export
#'

get_ids <- function(offers, patterns = c("data sci",
                                                 "stat.st", "anality",
                                                 "analiz. danych",
                                                 "[^A-Za-z]SAS", "[^0-9][^0-9A-Za-zóÓ+]R[^0-9A-Za-zóÓęĘśŚćĆ&“”/ö][^&]",
                                                 "big.+data", "data.+mining", "tableau", "SPSS","stata","cognos",
                                                 "azure","machine learning","spark","hadoop","matlab"),
                            case = c(T,T,T,T,T,T,F,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T)) {
  no <- lapply(seq_along(patterns), function(i){
    grep(offers, pattern = patterns[i], ignore.case = case[i])
  })
  sort(unique(unlist(no)))
}
