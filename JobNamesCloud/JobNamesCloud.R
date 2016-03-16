# loading libs
library("readr", lib.loc="D:/Program Files/R/R-devel/library")
library("dplyr", lib.loc="~/R/win-library/3.3")
library("data.table", lib.loc="D:/Program Files/R/R-devel/library")
library("utils", lib.loc="D:/Program Files/R/R-devel/library")
library("ggplot2")
library("rCharts")
# switching dir to get some data
setwd("../Filtr")

# reading needed datasets
pracuj_filtered <- read_csv("pracuj_filtered.csv" , col_names = TRUE)
needed_complete_phrases <- read_csv("needed_complete_phrases.csv" , col_names = TRUE)

# converting data.table column to vector of strings
needed_complete_phrases <- as.vector(needed_complete_phrases$needed_complete_phrases)


exeptions_phrases <- read_csv("exeptions_phrases.csv" , col_names = TRUE)

# going back home
setwd("../JobNamesCloud")

# creating data.frame to accumulate numbers of offers for a given phrase
num_of_nam <- c()

# same ID killer
#pracuj_data <- as.data.table(pracuj_data)
#setkey(pracuj_data, id)
#pracuj_data <- pracuj_data[!duplicated(pracuj_data),]


for (NCP in needed_complete_phrases)  {
  
  work_var <- 0
  work_var <- mutate(pracuj_filtered, JobNameIndicator = grepl(paste0(".*",NCP,".*"), href) )%>% summarise(sum(JobNameIndicator))
  num_of_nam <- append(num_of_nam, work_var)
  
  #num_of_nam <- rbind(num_of_nam, as.data.frame(assign(paste0("",NCP), work_var)))
  #pracuj_filtered <- rbind(pracuj_filtered, pracuj_filtered1)
}


names(num_of_nam) <- needed_complete_phrases
num_of_nam <- sapply(num_of_nam, 
                     function (x) {return(x)})

df_row_names <- as.character(1:length(needed_complete_phrases))

num_of_nam_df <- as.data.frame(num_of_nam, row.names = df_row_names, stringsAsFactors = FALSE)
num_of_nam_df <- data.frame(needed_complete_phrases, num_of_nam_df)
names(num_of_nam_df) <- c("phrase","num_of_offers")
num_of_nam_df <-  arrange(num_of_nam_df, desc(num_of_offers))


ggplot(num_of_nam_df, aes(x=factor(1), num_of_offers, fill=as.factor(paste(phrase, num_of_offers, sep = " - ")))) + geom_bar(stat="identity", width=1) + ggtitle("Czestosc wystepowania poszukiwanej frazy")+coord_polar(theta = "y") + theme(legend.position="right", legend.title=element_blank(), plot.title = element_text(lineheight=3, face="bold", color="black", size=12)) + xlab("")+ylab("")

num_of_nam_df$phrase <- reorder(num_of_nam_df$phrase, num_of_nam_df$num_of_offers, mean)
ggplot(num_of_nam_df, aes(x=phrase, y=num_of_offers, fill=as.factor(paste(phrase, num_of_offers, sep = " - ")))) + geom_bar(stat="identity", width=1) + ggtitle("Czestosc wystepowania poszukiwanej frazy")+ theme(legend.position="right", legend.title=element_blank(), plot.title = element_text(lineheight=3, face="bold", color="black", size=12)) + xlab("")+ylab("") + coord_flip() +scale_y_sqrt(breaks=c(0,1,5,10,50,100,200,500))

setwd("../Filtr")
table_data <- read_csv("job_names_per_month_plot.csv", col_names = TRUE)

job_names_cloud_names <- names(table_data)




plot1 <- nPlot(number_of_offers_per_month ~ month, group = "phrase", data = table_data, type = "scatterChart")

plot1$xAxis(axisLabel = 'Month')
plot1$yAxis(axisLabel = 'Frequency of appereance')

plot1$set(width = 750, height = 590)
plot1$save("rCharts1.html", standalone=TRUE)

setwd("../JobNamesCloud")
# saving solution to file
write_csv(num_of_nam_df, "JobNamesCloud.csv")