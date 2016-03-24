library(rvest)
library(xml2)
library(RPostgreSQL)

dbname = "pracuj"
user = "pracuj"
host = "services.mini.pw.edu.pl"
host = "192.168.137.38"



sterownik <- dbDriver("PostgreSQL")
polaczenie <- dbConnect(sterownik, dbname = dbname, user = user, password = password, host = host)


maxid <- dbGetQuery(polaczenie, "SELECT max(data) FROM offers")[1,1]
total <- 0

# zawsze parsuje 5 stron
for (strona in 1:5) {
  html <- read_html(paste0("http://www.pracuj.pl/praca?pn=",strona))
  uchwyty <- html_nodes(html, css = ".offer__list_item_link_name")
  linki <- html_attr(uchwyty, name = "href")
  linki <- na.omit(linki)
  for (k in seq_along(linki)) {
    try({
      ll <- paste0("http://www.pracuj.pl",linki[k])
      curerntid <- gsub(gsub(linki[k], pattern=".*,", replacement = ""), pattern="[^0-9]*", replacement="")
      oferta <- read_html(ll)
      tytul <- html_text(html_nodes(oferta, ".offerTop__cnt_main_job"))
      data <- html_text(html_nodes(oferta, ".ico-time span"))[2]
      lokacja <- html_text(html_nodes(oferta, ".latlng span"))[2]
      stanowisko <- html_text(html_nodes(oferta, ".ico-briefcase"))
      firma <- html_text(html_nodes(oferta, ".offerTop__cnt_main_emplo-inline span"))
      oferta <- html_text(html_node(oferta, "#offCont"))
      
      czyjest <- dbGetQuery(polaczenie, paste0("SELECT count(*) FROM offers where id = '",curerntid,"'"))[1,1]
      if (0 == czyjest) {
        dbGetQuery(polaczenie, 
                   paste0("INSERT INTO offers (id, link, title, data, location, position, company, text) VALUES ('"
                          ,curerntid,"','"
                          ,gsub(ll, pattern = "['\"]", replacement = ""),"','"
                          ,gsub(tytul, pattern = "['\"]", replacement = ""),"','"
                          ,gsub(data, pattern = "['\"]", replacement = ""),"','"
                          ,gsub(lokacja, pattern = "['\"]", replacement = ""),"','"
                          ,gsub(stanowisko, pattern = "['\"]", replacement = ""),"','"
                          ,gsub(firma, pattern = "['\"]", replacement = ""),"','"
                          ,gsub(oferta, pattern = "['\"]", replacement = ""),"')"))
        total <- total + 1
      }
    }, silent = TRUE)
  }
}

cat("Dodano: ", total, "\n")
