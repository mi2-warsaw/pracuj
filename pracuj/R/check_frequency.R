#' Checking frequencies for selectec key-words
#'
#' Function \code{check_frequency} checks frequencies of selected key-words.
#'
#' @param offers vector with offrs descriptions
#' @param patterns patterns to be found in descriptions
#' @param case should the pattern be identified case sensitive?
#'
#' @return data frame
#'
#' @examples
#' \dontrun{
#' offers <- get_offers()
#' check_frequency(offers$description)
#' }
#' @author Przemyslaw Biecek
#'
#' @export
#'

check_frequency <- function(offers, patterns = c("data sci",
                                                 "stat.st", "anality",
                                                 "analiz. danych", "excel",
                                                 "[^A-Za-z]SAS", "[^0-9][^0-9A-Za-zóÓ+]R[^0-9A-Za-zóÓęĘśŚćĆ&“”/ö][^&]",
                                                 "big.+data", "data.+mining"),
                            case = c(T,T,T,T,T,T,F,T,T)) {
  no <- sapply(seq_along(patterns), function(i){
    length(grep(offers, pattern = patterns[i], ignore.case = case[i]))
  })
  df <- data.frame(no = no, freq = no/nrow(dat))
  rownames(df) <- patterns
  df
}
