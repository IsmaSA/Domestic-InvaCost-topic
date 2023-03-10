
####################################################################
#   Global economic costs of invasive alien domestic animals #######
####################################################################

suppressMessages({
  library(dplyr, quiet = TRUE, warn.conflicts = FALSE)
  library(reshape, quiet = TRUE, warn.conflicts = FALSE)
  library(ggplot2)
  library(stringr)
  library(tidyr)  
  library(stringr)
  library(invacost)
  library(taxize)
  library(rvest)
})

# Filtering DAD-IS database

df <- read.csv2("DAD-IS.csv")
df <- df[,c(1:4)]
head(df)

#Keep unique species according also with the common name
colnames(df)
df1 <- df[!duplicated(df[c("Specie", "Breed.Most.common.name")]), ]

unique(df$Specie) #37 unique species
unique(df$Breed.Most.common.name) #10,211 common names

table(df$Specie) #how many names for each species


## Extract scientific names; I will use the next web page as contain many domestic 
#and semi domestic species scientific names

url <- "https://en.wikipedia.org/wiki/List_of_domesticated_animals"


# Read the HTML content of the website
webpage <- read_html(url)

# Extract the table from the website  (Domestic)
table_html <- html_nodes(webpage, "table.wikitable")[1]
table_df <- html_table(table_html)
a<- table_df[[1]] 
a<- a[,c(1,2)]

#Semi-domestic (Tamed) scinetific names
table_html <- html_nodes(webpage, "table.wikitable")[2]
table_df <- html_table(table_html)
b<- table_df[[1]]

colnames(a)
colnames(b)
b<- b[,c(1,2)]

data<- bind_rows(a,b) # Combine both tables
# The cleaning steps of the data have been done in excel manually, see next file to full version:


data<- readxl::read_excel("Extract_name.xlsx")
data$`Common name` <- tolower(data$`Common name`) 



# Remove duplicates
df2 <- df1[!duplicated(df1$Specie), ]
df2$Specie <- tolower(df2$Specie)


colnames(df2)[3] <- "Common name"
df2$`Common name` <- tolower(df2$`Common name`)

dataset<- right_join(data,df2, by="Common name")


# Filling those "NA" with: https://www.fao.org/3/x8750e/x8750e01.pdf 

new_df <- data.frame(Scientific_name = c("Vicugna vicugna", "Pavo cristatus", "Casuarius casuarius", 
                              "Camelus bactrianus", "Phasianus colchicus", "Phasianus versicolor", "Hirudinea"))

# Combine the existing data frame and the new data frame using rbind()
dataset1 <- bind_rows(dataset, new_df)

#Full database ! :)
  
  
  
