#Read the csv file and store it in the dataframe.
my_dataframe <- read.csv("/Users/a.bsurekha/Downloads/section1.csv", header = FALSE)
my_dataframe

#Struture of the dataframe.
str(my_dataframe)

# Total of Rows.
nrow(my_dataframe)
ncol(my_dataframe)

#First and Last 10 rows of dataframe.
head(my_dataframe, n=10)
tail(my_dataframe, n=10)

#Change the appropraite column name in dataframe.
new_colnames <- c("Organisation_Name","Sub-building_Name","Building_Name","Number","Primary_Thorfare",
                                  "Alt_Thorfare","Secondary_Thorfare","Locality","Townland","Town","County","PostCode",
                                  "X-Cordinates","Y-Cordinates","Primary_Key")

colnames(my_dataframe) <- new_colnames
head(my_dataframe,10)

#Replace and recode all missing values.
my_dataframe[my_dataframe==""] <- NA
sum(is.na(my_dataframe))
sum(!complete.cases(my_dataframe))

#graphical view
#install.packages("mice")
library(mice)
md.pattern(my_dataframe)
library("VIM")
missing_values <- aggr(my_dataframe, prop = FALSE, numbers = TRUE)


#Missing Data Columnwise.
Missing_Count <- sapply(my_dataframe, function(y) sum(length(which(is.na(y)))))
Missing_Count <- data.frame(Missing_Count)
Missing_Count

#Move the Primary Key Identifier to the start of the database.
my_dataframe <- my_dataframe[, c(15,1,2,3,4,5,6,7,8,9,10,11,12,13,14)]
my_dataframe

#Create Limavady_data dataframe.
Limavady_data <- my_dataframe[which(my_dataframe$Locality == "LIMAVADY" | my_dataframe$Townland == "LIMAVADY" & my_dataframe$Town == "LIMAVADY"),]
Limavady_data
nrow(Limavady_data)

#Save the dataset with new csv file.
write.csv(Limavady_data,"Limavady.csv")
write.csv(my_dataframe,"CleanNIPostcodeData.csv")



