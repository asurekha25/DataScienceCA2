#helps to remove  objects
rm(list = ls(all=TRUE))
setwd("/Users/a.bsurekha/Downloads/NI Crime Data")

#Looking for patterns in a certain path and getting files
File=list.files(pattern = "[.]csv$",recursive = T)
File
#Assuming values with tab seperator with a header
ALLNICrimeData_List=lapply(File,function(x)read.csv(x, header = TRUE))
#Now Consider the same header columns for all files
ALLNICrimeData=do.call("rbind",ALLNICrimeData_List)
write.csv(ALLNICrimeData,"ALLNICrimeData.csv")
nrow(ALLNICrimeData)

#Modifying and Removing the attributes from the newly created file
NewALLNICrimeData=subset(ALLNICrimeData,select = -c(Crime.ID,Reported.by,Falls.within,LSOA.code,LSOA.name,Last.outcome.category))
NewALLNICrimeData
#Shortening each crime type
unique(ALLNICrimeData$Crime.type)
library(dplyr)
NewALLNICrimeData=NewALLNICrimeData%>%mutate(Crime.type=recode_factor(Crime.type,'Anti-social behaviour'='ASBO','Bicycle theft'='BITH','Burglary'='BURG',
                                                                      'Criminal damage and arson'='CDAR','Drugs'='DRUG','Other theft'='OTTH',
                                                                      'Possession of weapons'='POFW','Public order'='PUBO','Robbery'='ROBY','Shoplifting'='SHOP',
                                                                      'Theft from the person'='THPR','Vehicle crime'='VECR',
                                                                      'Violence and sexual offences'='VISO','Other crime'='OTCR'))

NewALLNICrimeData
#Plot() function
counts <- table(NewALLNICrimeData$Crime.type)
My_Plot=barplot(counts,main = "Distribution of Crime Type and frequency of each crime type", ylab = 'frequency',
                col = c(rgb(0.3,0.9,0.4,0.6),rgb(0.3,0.4,0.5,0.6),rgb(0.3,0.1,0.4,0.6),rgb(0.3,0.5,0.4,0.6),
                        rgb(0.9,0.9,0.4,0.6),rgb(0.3,0.3,0.4,0.6),rgb(0.7,0.1,0.4,0.6),rgb(0.1,0.5,0.4,0.6),
                        rgb(0.3,0.9,0.9,0.6),rgb(0.3,0.9,0.9,0.6),rgb(0.3,0.1,0.9,0.6),rgb(0.3,0.5,0.4,0.6),
                        rgb(0.3,0.9,0.4,0.9),rgb(0.3,0.9,0.4,0.9)),xlab = 'Crime Type',las=2) 
text(My_Plot,counts/2,paste("",counts,sep = ""),cex=1)

#Modifying ALLNICrimeData to set Location atribute with street names
library(stringr)
NewALLNICrimeData$Location <- str_remove(NewALLNICrimeData$Location,"On or near")

NewALLNICrimeData$Location

#From AllNICrimeData choose 5000 from crime data

NewALLNICrimeData=NewALLNICrimeData[!( NewALLNICrimeData$Location==""), ]
rows=seq(1,nrow(NewALLNICrimeData),1)
set.seed(100)
rows
random_crime_sample=NewALLNICrimeData[sample(rows, 5000), ]
random_crime_sample
CleanNIPostcodeData = read.csv('CleanNIPostcodeData.csv', header = TRUE)
head(CleanNIPostcodeData)
find_a_town <- function(i) {
  random_crime_sample$City_Town_Village<-CleanNIPostcodeData[match(toupper(random_crime_sample$Location),CleanNIPostcodeData$Primary.Thorfare),11]
}
random_crime_sample$City_Town_Village
find_a_town(toupper(random_crime_sample$Location))


#Creating a new function add_town_data
random_crime_sample$Context = NULL
VillageList = read.csv('VillageList.csv', header = TRUE)
add_town_data <- function(i) {
  random_crime_sample$POPULATION <- VillageList[match(random_crime_sample$City_Town_Village,toupper(VillageList$CITY.TOWN.VILLAGE)),2]
}
add_town_data(random_crime_sample$City_Town_Village)
write.csv(random_crime_sample,'random_crime_sample.csv', row.names = FALSE)
random_crime_sample

