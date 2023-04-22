# script to combine Pet species from Turbelin et al. 2020 and domestic species identified before

suppressMessages({
  library(dplyr, quiet = TRUE, warn.conflicts = FALSE)
  library(reshape, quiet = TRUE, warn.conflicts = FALSE)
  library(ggplot2)
  library(tidyr)  
  library(stringr)
  library(invacost)
})


## Pet species
pet <- read_excel("db_withpathways_2023.xlsx")

pet<- pet %>% filter(Description2 =="Pet trade") %>% filter(Kingdom=="Animalia")

unique(pet$Species)



#Invacost
invacost <- getInvaCostVersion("4.1")
unique(invacost$Species)

### Cleaning steps
invacost <- invacost[-which(is.na(invacost$Probable_starting_year_adjusted)), ]
invacost <- invacost[-which(is.na(invacost$Probable_ending_year_adjusted)), ]
invacost <- invacost[-which(is.na(invacost$Cost_estimate_per_year_2017_USD_exchange_rate)), ]



# Main domestic animals 
df<- read.csv2("DAD-IS database.csv")
head(df)

domestic <- df$Scientific.name


#filter invacost database to main domestic animals
colnames(invacost)

data <- invacost[invacost$Species %in% domestic, ]

unique(data$Species)




##### Combine both datas
colnames(data)
data1<- data %>% select(InvaCost_ID,Cost_ID,Species,Common_name,Island,Geographic_region,
                        Official_country,Period_of_estimation,Probable_starting_year_adjusted,
                        Probable_ending_year_adjusted,Cost_estimate_per_year_2017_USD_exchange_rate,
                        Applicable_year,Implementation,Impacted_sector,Type_of_cost_merged,Method_reliability
)

colnames(pet)
pet1<- pet %>% select(InvaCost_ID,Cost_ID,Species,Common_name,Island,Geographic_region,
                      Official_country,Period_of_estimation,Probable_starting_year_adjusted,
                      Probable_ending_year_adjusted,Cost_estimate_per_year_2017_USD_exchange_rate,
                      Applicable_year,Implementation,Impacted_sector,Type_of_cost_merged,Method_reliability
)



invacost <- rbind(data1,pet1)



invacost<- invacost[!duplicated(invacost$Cost_ID), ] #1513
invacost <- invacost[-which(is.na(invacost$Probable_starting_year_adjusted)), ]
invacost <- invacost[-which(is.na(invacost$Probable_ending_year_adjusted)), ] #1496
unique(invacost$Species)

###### expanded
expanded <- expandYearlyCosts(invacost, #your help file
                              startcolumn = "Probable_starting_year_adjusted",
                              endcolumn = "Probable_ending_year_adjusted")

min(expanded$Impact_year)
max(expanded$Impact_year)

expanded<-expanded %>% filter(Impact_year <= "2022")
expanded<-expanded %>% filter(Impact_year >= "1960")             
expanded$cost <- as.numeric(gsub(",", "", expanded$Cost_estimate_per_year_2017_USD_exchange_rate))
expanded <- expanded[!is.na(expanded$cost),]
expanded$cost_bil <- (expanded$cost/1000000000)
sum(expanded$cost_bil) # 191.88
nrow(expanded) # 2251

#write_xlsx(expanded,"Expanded_domestic.xlsx")



### Add those new records from Sus Scrofa following Ana and Franck comments

### Filter those new records of pigs according to Franck comments
news <- c("Feral pig","Feral pig/Feral goat","Feral pigs","Feral goat/Feral pig","Feral Dog/Feral Pig",
          "Feral dogs and pigs","Pig","Feral goat/pig/cat") 

invacost <- invacost %>% filter(Common_name %in% news)

###### expanded
expanded <- expandYearlyCosts(invacost, #your help file
                              startcolumn = "Probable_starting_year_adjusted",
                              endcolumn = "Probable_ending_year_adjusted")

min(expanded$Impact_year)
max(expanded$Impact_year)

expanded<-expanded %>% filter(Impact_year <= "2022")
expanded<-expanded %>% filter(Impact_year >= "1960")             
expanded$cost <- as.numeric(gsub(",", "", expanded$Cost_estimate_per_year_2017_USD_exchange_rate))
expanded <- expanded[!is.na(expanded$cost),]
expanded$cost_bil <- (expanded$cost/1000000000)
sum(expanded$cost_bil) 
nrow(expanded) 

#write.xlsx(expanded, "Extra_Susscrofa.xlsx")


## Once that the Sus Scorfa records are added to the main dataset (Expanded_domestic.xlsx), I started to analyse it




###################################################################################################
#######################                   RESULTS                     #############################
###################################################################################################



df<- read_xlsx("Expanded_domestic.xlsx", sheet = "Sheet1")
unique(df$Kingdom)
df<- filter(df, !Kingdom=="Plantae")


sum(df$cost_bil) # 191.88
nrow(df) # 2251
colnames(df)


a<-df %>% group_by(Domestic) %>% summarise(Total_cost=sum(cost_bil),
                                           n=n())

#### method reliability ####

a<-df %>% group_by(Domestic, Method_reliability) %>% 
  dplyr::summarise(Total_cost=sum(cost_bil),
                   n=n())  %>% ungroup()

b <- a %>%
  group_by(Domestic) %>%
  dplyr::summarise(Total_cost1 = sum(Total_cost)) %>%
  ungroup()

result <- a %>%
  left_join(b, by = "Domestic") %>%
  mutate(percentage = (Total_cost / Total_cost1) * 100)
result

#### Implementation ####
a<-df %>% group_by(Domestic, Implementation) %>% 
  dplyr::summarise(Total_cost=sum(cost_bil),
                   n=n())

b <- a %>%
  group_by(Domestic) %>%
  dplyr::summarise(Total_cost1 = sum(Total_cost)) %>%
  ungroup()

result <- a %>%
  left_join(b, by = "Domestic") %>%
  mutate(percentage = (Total_cost / Total_cost1) * 100)
result

#### Species costs ####

a<-df %>% group_by(Species) %>% 
  dplyr::summarise(Total_cost=sum(cost_bil),
                   n=n())

a<-df %>% group_by(Species, Domestic, Official_country) %>% 
  dplyr::summarise(Total_cost=sum(cost_bil),
                   n=n())



#### Impacted sector ####
unique(df$Impacted_sector)

df[grep("/", df$Impacted_sector),]$Impacted_sector <- "Diverse"


a<-df %>% group_by(Impacted_sector) %>% 
  summarise(Total_cost=sum(cost_bil),
            n=n())

a<-df %>% group_by(Impacted_sector, Domestic) %>% 
  summarise(Total_cost=sum(cost_bil),
            n=n())


#### Type of cost ####

a<-df %>% group_by(Type_of_cost_merged) %>% 
  summarise(Total_cost=sum(cost_bil),
            n=n())



a<-df %>% group_by(Type_of_cost_merged, Domestic) %>% 
  summarise(Total_cost=sum(cost_bil),
            n=n())


#### Official country - islands #### 
unique(df$Geographic_region)

df$Geographic_region[df$Geographic_region== "Europe/North America"] <- "Diverse"
df$Geographic_region[df$Geographic_region== "Pacific Islands"] <- "Oceania"
df$Geographic_region[df$Geographic_region== "Oceania/Pacific Islands"] <- "Oceania"


a<-df %>% group_by(Geographic_region) %>% 
  summarise(Total_cost=sum(cost_bil),
            n=n())

a<-df %>% group_by(Geographic_region, Domestic) %>% 
  summarise(Total_cost=sum(cost_bil),
            n=n())

a<-df %>% group_by(Official_country) %>% 
  summarise(Total_cost=sum(cost_bil),
            n=n())

a<-df %>% group_by(Island) %>% 
  summarise(Total_cost=sum(cost_bil),
            n=n())

a<-df %>% group_by(Island, Domestic, Species) %>% 
  summarise(Total_cost=sum(cost_bil),
            n=n())


##### Wild / domestic  #### 
df<- read_xlsx("Expanded_domestic.xlsx", sheet = "Sheet1")
unique(df$Kingdom)
df<- filter(df, !Kingdom=="Plantae")
unique(df$Species)

no_tetrapods <- c("Pomacea canaliculata","Lissachatina fulica","Dreissena polymorpha",
                  "Pseudorasbora parva","Procambarus clarkii","Corbicula fluminea","Channa argus",
                  "Cyprinus carpio","Gambusia holbrooki","Helix lucorum","Lepomis gibbosus",
                  "Pomacea maculata","Carassius auratus","Coptodon zillii","Pterois volitans",
                  "Lepomis macrochirus","Carcinus maenas")

df<- df %>% filter(!Species %in% no_tetrapods)

df1 <- read.csv("tetrapods_all.csv")
unique(df1$Species)


df1<-df1[!grepl("sp.", df1$Species),]
df1<-df1[!grepl("spp.", df1$Species),]
df1<-df1[!grepl("gen. sp.", df1$Species),]

df1 <- df1[-which(is.na(df1$Probable_starting_year_adjusted)), ]
df1 <- df1[-which(is.na(df1$Probable_ending_year_adjusted)), ]



c<- intersect(unique(df$Species),unique(df1$Species))

df1<- df1 %>% filter(!Species %in% c)

expanded <- expandYearlyCosts(df1, 
                              startcolumn = "Probable_starting_year_adjusted",
                              endcolumn = "Probable_ending_year_adjusted")

min(expanded$Impact_year)
max(expanded$Impact_year)

expanded<-expanded %>% filter(Impact_year <= "2022")
expanded<-expanded %>% filter(Impact_year >= "1960")             
expanded$cost <- as.numeric(gsub(",", "", expanded$Cost_estimate_per_year_2017_USD_exchange_rate))
expanded <- expanded[!is.na(expanded$cost),]
expanded$cost_bil <- (expanded$cost/1000000000)
sum(expanded$cost_bil) # 191.88
nrow(expanded) # 2251


unique(expanded$Species)

expanded$Wild_domestic <- "wild"
df$Wild_domestic <- "domestic"

data <- bind_rows(expanded,df)

data<- data[,c(1,3,20,51,53,57,69,70)]


























