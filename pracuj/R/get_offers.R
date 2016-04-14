#' Importing offers table from a database
#'
#' Function \code{get_offers} imports offers table from a database.
#'
#' @usage get_offers(dbname = 'pracuj', user = 'reader',
#'   password = 'qux94874', host = 'services.mini.pw.edu.pl',
#'   sorted_by_id = TRUE, windows = .Platform$OS.type == 'windows')
#'
#' @param dbname name of database; default: 'pracuj'
#' @param user name of user; default: 'reader'
#' @param password password of database; default: 'qux94874'
#' @param host name of host; default: 'services.mini.pw.edu.pl'
#' @param sorted_by_id information if table should be sorted by id; default: TRUE
#' @param windows information of used operation system;
#' default: .Platform$OS.type == 'windows'
#' @param description should the column description be returned as well?
#'
#' @return data frame
#'
#' @examples
#' \dontrun{
#' offers <- get_offers()
#' dim(votes)
#' }
#' @author Przemyslaw Biecek
#'
#' @export
#'

get_offers <- function(dbname = "pracuj", user = "reader", password = "qux94874", host = "services.mini.pw.edu.pl",
                            sorted_by_id = TRUE, windows = .Platform$OS.type == "windows", description = FALSE) {
    stopifnot(is.character(dbname), is.character(user), is.character(password), is.character(host),
              is.logical(sorted_by_id), is.logical(windows))

    # connecting to database
    drv <- dbDriver("PostgreSQL")
    database <- dbConnect(drv, dbname = dbname, user = user, password = password, host = host)

    if (description) {
      offers <- dbGetQuery(database, "SELECT * FROM offers")
      if (windows) {
        offers[, 8] <- iconv(offers[, 8], from = "UTF-8", to = "Windows-1250")
      }
    } else {
      offers <- dbGetQuery(database, "SELECT id, href, position, date, location, grade, employer FROM offers")
    }

    suppressWarnings(dbDisconnect(database))
    return(offers)
}
