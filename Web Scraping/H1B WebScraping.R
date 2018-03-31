library(jsonlite) # parsing JSON objects
install.packages("rvest")
library(rvest) #harvests” HTML
install.packages("pbapply")
library(pbapply) #progress bars to the base apply functions
library(data.table)

#Explore website structure
#Find a way to get the data from the website(keywords/Serch filters)
#Right click to inspect developer and jump on to network- copy the php url displaying distinct features

#SCRAPING
json.cities <- paste0('http://h1bdata.info/cities.php?term=', letters)
json.cities

#PARSING
all.cities <- unlist(pblapply(json.cities,fromJSON))
all.cities

#To decrease the individual page load times passing in two parameters, city and year, into each webpage query
city.year <- expand.grid(city=all.cities,yr=seq(2012,2018))
head(city.year)

#To encode 2 word cities
install.packages("urltools")
library(urltools)
city.year$city <- urltools::url_encode(as.character(city.year$city))

#To get all attributes for each obeservations
#http://h1bdata.info/index.php?em=&job=&city=cleveland&year=2015
all.urls <- paste0('http://h1bdata.info/index.php?em=&job=&city=', city.year[,1],'&year=', city.year[,2])


#Extracting Information From Pages
# main function to get data from each page
#First, a URL is accepted and read_html() parses the page contents. 
#Next, the page’s single html_table is selected from all other HTML information. 
#The main function converts the x object to a data.table so it can be stored efficiently in memory.
#Finally, before closing out main, you can add a Sys.sleep so you won’t be considered a DDOS attack.

main<-function(url.x){
  x<-read_html(url.x)
  x<-html_table(x)
  x<-data.table(x[[1]])
  return(x)
  Sys.sleep(5)
}

#Keeping track of all the scrapping process(stored as list)
all.h1b<-pblapply(all.urls, main)

#Combining all data into data table
all.h1b<-rbindlist(all.h1b)
head(all.h1b)

#Save and export
write.csv(all.h1b,'h1b_data.csv', row.names=F)


#Cleaning Data
library(lubridate)
library(stringr)
options(scipen=999)

h1b.data<-fread('h1b_data.csv')

#Renaming the column names
colnames(h1b.data)<-tolower(names(h1b.data))
colnames(h1b.data)<-gsub(' ', '_', names(h1b.data))  

#Class of all the variables
apply(h1b.data,2,class)

#correcting date variable
tail(h1b.data$submit_date)
h1b.data$submit_date<-gsub('/', '-', h1b.data$submit_date)
h1b.data$submit_date<-mdy(h1b.data$submit_date)
tail(h1b.data$submit_date)
class(h1b.data$submit_date)
h1b.data$submit_month<-month(h1b.data$submit_date, label=T)
h1b.data$submit_yr<-year(h1b.data$submit_date)
head(h1b.data)

#correcting class of base salary
h1b.data$base_salary<-gsub(',','',h1b.data$base_salary)
h1b.data$base_salary<-as.numeric(h1b.data$base_salary)
head(h1b.data$base_salary)

#correcting Location variable
state<-str_split_fixed(h1b.data$location,', ', 2)
h1b.data$city<-state[,1]
h1b.data$state<-state[,2] 

#Exploring data
state.tally<-table(h1b.data$state)
state.tally<-data.frame(state=names(state.tally), h1b=as.vector(state.tally))

#Visiualization
  #Barplot
barplot(state.tally$h1b,
        names.arg = names(table(h1b.data$state)),
        las=3)

