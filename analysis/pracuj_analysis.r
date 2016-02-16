# set working directory
# setwd("C:\\Users\\Krzysztof\\Documents\\GitHub\\pracuj\\analysis")

# import packages
library(dplyr)
library(tidyr)
library(maptools)
library(rgeos)
library(ggplot2)

# read data to analyze
df <- read.csv("../Filtr/pracuj_filtered.csv", stringsAsFactors=F)

# leave only unique offers and for now focus only on locations
pracuj <- df %>%
	filter(!duplicated(id)) %>%
	select(location)

# In location parameter abroad jobs contain either only country's name
# or "city, country" structure. Need to separate them from polish offers which
# mostly have "city, province" structure. Sometimes it is only province or couple
# of cities in one province (e.g. "Gdańsk, Gdynia, Sopot, pomorskie").

# vector of all europan countires
euro <- c("Albania", "Andora", "Anglia", "Austria", "Belgia", "Białoruś",
		  "Bośnia i Hercegowina", "Bułgaria", "Chorwacja", "Cypr",
		  "Czarnogóra", "Czechy", "Dania", "Estonia", "Finlandia", 
		  "Francja", "Gibraltar", "Grecja", "Gruzja", "Hiszpania",
		  "Holandia", "Irlandia", "Islandia", "Kazachstan", "Liechtenstein",
		  "Litwa", "Luksemburg", "Łotwa", "Macedonia", "Malta", "Monako",
		  "Mołdawia", "Niemcy", "Norwegia", "Polska", "Portugalia", "Rosja",
		  "Rumunia", "San Marino", "Serbia", "Szwajcaria", "Szwecja",
		  "Słowacja", "Słowenia", "Turcja", "Ukraina", "Watykan",
		  "Wielka Brytania", "Węgry", "Włochy")

# find abroad jobs
inlist <- lapply(euro, function(x) {grep(x, pracuj$location)})

# separate abroad jobs indexes
abjob <- c()
for (i in 1:length(inlist)) {
	if (length(inlist[[i]]) != 0) {
		abjob <- append(abjob, inlist[[i]])
	}
}

# --- FOREIGN OFFERS --- #

# filter abroad positions
foreign <- pracuj %>%
	filter(location %in% pracuj[abjob, ])

# paste "NA, " into foreign positions without cities for later column separation
for (i in 1:length(foreign$location)) {
	if (!grepl(", ", foreign$location[i])) {
		foreign$location[i] <- paste0(NA, ", ", foreign$location[i])
	}
}

# separate foreign locations to c(city, country) structure and convert NA strings
foreign <- foreign %>%
	separate(location, c("city", "country"), sep=", ", convert=T)

# --- POLISH OFFERS --- #

# filter polish positions
polish <- pracuj %>%
	filter(location %in% pracuj[-abjob, ])

# get multiple cities positions strings
mulCit <- polish %>%
	filter(grepl(",.*,", polish$location))

# split strings into cities and porvinces
citiesProv <- strsplit(mulCit$location, ", ")

# separate cities and provinces, for now drop cities
towns <- c()
provs <- c()
for (i in 1:length(citiesProv)) {
	provs[i] <- citiesProv[[i]][length(citiesProv[[i]])]
}

# substitute multiple cities rows with provinces only
j <- 1
for (i in grep(",.*,", polish$location)) {
	polish$location[i] <- provs[j]
	j <- j + 1
}

# paste "NA, " into polish positions without cities for later column separation
for (i in 1:length(polish$location)) {
	if (!grepl(", ", polish$location[i])) {
		polish$location[i] <- paste0(NA, ", ", polish$location[i])
	}
}

# separate polish locations to c(city, province) structure, convert NA strings
polish <- polish %>%
	separate(location, c("city", "province"), sep=", ")

# --- MAP --- #

# read provinces shapes data
shp <- readShapePoly("POL_adm_shp/POL_adm1.shp")

# vector of provinces in order as they appear in shp@data$VARNAME_1
provinces <- c("łódzkie", "świętokrzyskie", "wielkopolskie",
			   "kujawsko-pomorskie", "małopolskie", "dolnośląskie",
			   "lubelskie", "lubuskie", "mazowieckie", "opolskie", "podlaskie",
			   "pomorskie", "śląskie", "podkarpackie", "warmińsko-mazurskie",
			   "zachodniopomorskie")

# substitute to names with polish signs
shp@data$VARNAME_1 <- provinces

# fortify data
shpf <- fortify(shp, region="VARNAME_1")

# create data.frame for map filling
offersPerProvince <- data.frame(province = names(table(polish$province)),
								n = as.vector(table(polish$province)))

# create a plot
theMap <- ggplot() +
	# fill by number of offers
	geom_map(data=offersPerProvince,
			 aes(map_id=province, fill=n),
			 map=shpf) +
	# map contours
	geom_path(data=shpf,
			  aes(x=long, y=lat, group=id),
			  colour="black", size=0.25) +
	# mercator projection
	coord_map(projection="mercator") +
	# change theme
	theme_bw() +
	# change fill name and color
	scale_fill_gradient("Liczba ofert", low = "grey90", high = "black") +
	# remove unnecessary elements
	theme(axis.ticks=element_blank(), panel.border=element_blank(),
		  axis.text=element_blank(), panel.grid=element_blank(),
		  axis.title=element_blank())

# plot
theMap