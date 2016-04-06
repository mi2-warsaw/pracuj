library(rvest)

library(RPostgreSQL)
dbname = "pracuj"
user = "pracuj"
host = "services.mini.pw.edu.pl"

sterownik <- dbDriver("PostgreSQL")
polaczenie <- dbConnect(sterownik, dbname = dbname, user = user, password = password, host = host)


dbGetQuery(polaczenie, "SELECT count(*) FROM offers")


dbGetQuery(polaczenie, "SELECT * FROM offers")


###########################################################
# OLD CODE
# use updateDatabase.R instead

i = 1
wynikiDF[i,"link"]

for (i in 2:nrow(wynikiDF)) {
  dbGetQuery(polaczenie, 
             paste0("INSERT INTO offers (id, link, title, data, location, position, company, text) VALUES ('"
                    ,gsub(wynikiDF[i,"link"], pattern=".*,", replacement = ""),"','"
                    ,gsub(wynikiDF[i,"link"], pattern = "['\"]", replacement = ""),"','"
                    ,gsub(wynikiDF[i,"tytul"], pattern = "['\"]", replacement = ""),"','"
                    ,gsub(wynikiDF[i,"data"], pattern = "['\"]", replacement = ""),"','"
                    ,gsub(wynikiDF[i,"lokacja"], pattern = "['\"]", replacement = ""),"','"
                    ,gsub(wynikiDF[i,"stanowisko"], pattern = "['\"]", replacement = ""),"','"
                    ,gsub(wynikiDF[i,"firma"], pattern = "['\"]", replacement = ""),"','"
                    ,gsub(wynikiDF[i,"oferta"], pattern = "['\"]", replacement = ""),"')"))
}

head(wynikiDF,1)




html <- read_html("http://www.pracuj.pl/praca?pn=1")
uchwyty <- html_nodes(html, css = ".offer__list_item_link_name")
linki <- html_attr(uchwyty, name = "href")
linki <- na.omit(linki)

wszystkieLinki <- list()
for (i in 1:474) {
  html <- read_html(paste0("http://www.pracuj.pl/praca?pn=",i))
  uchwyty <- html_nodes(html, css = ".offer__list_item_link_name")
  linki <- html_attr(uchwyty, name = "href")
  linki <- na.omit(linki)
  wszystkieLinki[[i]] <- linki
}

linki <- unlist(wszystkieLinki)
wyniki <- list()
for (i in 25110:length(linki)) {
  try({
    ll <- paste0("http://www.pracuj.pl",linki[i])
    oferta <- read_html(ll)
    
    tytul <- html_text(html_nodes(oferta, ".offerTop__cnt_main_job"))
    data <- html_text(html_nodes(oferta, ".ico-time span"))[2]
    lokacja <- html_text(html_nodes(oferta, ".latlng span"))[2]
    stanowisko <- html_text(html_nodes(oferta, ".ico-briefcase"))
    firma <- html_text(html_nodes(oferta, ".offerTop__cnt_main_emplo-inline span"))
    oferta <- html_text(html_node(oferta, "#offCont"))
    
    o2 <- c(ll, tytul, data, lokacja, stanowisko, firma, oferta)
    cat(i, "/", length(linki), "    ", tytul, "\n")
    wyniki[[i]] <- o2
  }, silent = TRUE)
}


inds <- which(sapply(wyniki, length) == 7)
wynikiDF <- t(as.data.frame(wyniki[ inds ]))
colnames(wynikiDF) <- c("link", "tytul", "data", "lokacja", "stanowisko", "firma", "oferta")
rownames(wynikiDF) <- NULL
save(wynikiDF, file="wynikiDF.rda")

