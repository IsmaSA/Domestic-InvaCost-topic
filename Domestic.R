##### Domestic animals (Invacost)

invacost <- getInvaCostVersion("4.1")
unique(invacost$Species)
### Cleaning steps
# Eliminating data with no information on starting and ending years
invacost <- invacost[-which(is.na(invacost$Probable_starting_year_adjusted)), ]
invacost <- invacost[-which(is.na(invacost$Probable_ending_year_adjusted)), ]
invacost <- invacost[-which(is.na(invacost$Cost_estimate_per_year_2017_USD_exchange_rate)), ]

# Main domestic animals 
setwd("C:/Users/isma-/OneDrive/Escritorio/Domestic_Invacost")
df<- read.csv2("DAD-IS database.csv")
head(df)

domestic <- df$Scientific.name


#filter invacost database to main domestic animals
colnames(invacost)

data <- invacost[invacost$Species %in% domestic, ]

unique(data$Species)


############ Pet animals #### 
setwd("C:/Users/isma-/OneDrive/Escritorio/Domestic_Invacost")
pet <- read_excel("db_withpathways_2023.xlsx")

pet<- pet %>% filter(Description2 =="Pet trade") %>% filter(Kingdom=="Animalia")

unique(pet$Species)


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
sum(expanded$cost_bil) # 191.88
nrow(expanded) # 2251

write.xlsx(expanded, "Extra_Susscrofa.xlsx")
#write_xlsx(expanded,"Expanded_domestic.xlsx")
unique(expanded$Species)

###################################################################################################
#######################                   RESULTS                     #############################
###################################################################################################



df<- read_xlsx("Expanded_domestic.xlsx", sheet = "Sheet1")
unique(df$Kingdom)
df<- filter(df, !Kingdom=="Plantae")
df<- filter(df, !Species=="Boiga irregularis")

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
a<-df %>% group_by(Implementation) %>% summarise(Total_cost=sum(cost_bil),
                                           n=n())

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

a<-df %>% group_by(Species, Domestic) %>% 
  dplyr::summarise(Total_cost=sum(cost_bil),
            n=n())  
###########  Lolliplot #########

a <- a %>% group_by(Domestic) %>% top_n(5, Total_cost) %>% ungroup()

df1 <- df %>% filter(Implementation=="Observed")

df1 <- semi_join(df1,a, by="Species")
df1<-df1 %>% group_by(Species, Domestic) %>% 
  summarise(Total_cost=sum(cost_bil),
            n=n())


str(a)
options(scipen = 999)

a$Domestic<-as.factor(a$Domestic)
a$Species <- factor(a$Species, 
        levels = c("Felis catus","Dreissena polymorpha","Corbicula fluminea","Lithobates catesbeianus","Pseudorasbora parva",
            "Oryctolagus cuniculus", "Sus scrofa", "Columba livia", "Capra hircus","Cervus nippon/Hydropotes inermis",
  "Bubalus bubalis","Equus asinus/Equus caballus", "Camelus dromedarius","Equus asinus",
  "Equus caballus"))

ggplot(a, aes(x=Species, y=Total_cost, group = Domestic)) + 
  geom_point(size=3, aes(colour = factor(Domestic))) + 
  geom_segment(aes(x=Species, 
                  xend=Species, 
                   y=0, 
                   yend=Total_cost, colour = factor(Domestic))) + 
  geom_segment(data=df1, aes(x=Species, 
                  xend=Species, 
                 y=0, 
                   yend=Total_cost)) +

  scale_y_continuous(trans='log10', breaks = c(0.01,0.1,1,10,100)) + theme_classic2() + theme_cleveland()+ 
  theme(axis.text.x = element_text(angle=55, vjust=1,hjust=1))

a %>% group_by(Domestic) %>% summarise(cost=mean(Total_cost))

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


a<-df %>% group_by(Geographic_region, Official_country) %>% 
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

a<-df %>% group_by(Island, Domestic) %>% 
  summarise(Total_cost=sum(cost_bil),
            n=n())

#### Ballon plot ####

df<- read_xlsx("Expanded_domestic.xlsx", sheet = "Sheet1")
unique(df$Kingdom)
df<- filter(df, !Kingdom=="Plantae")

a1<-df %>% group_by(Domestic, Method_reliability) %>% 
  summarise(Total_cost=sum(cost_bil),
            n=n())
a2<-df %>% group_by(Domestic, Implementation) %>% 
  summarise(Total_cost=sum(cost_bil),
            n=n())


str(a)
library(viridis)
a$n<- as.numeric(a$n)
a1$Domestic <- factor(a1$Domestic, 
                    levels = c("Pet","Livestook","Beast"))

ggplot(a1, aes(x = Domestic, y = Method_reliability, size = Total_cost, color = n)) +
  geom_point() +
  geom_point(data=a2, aes(Domestic,Implementation)) +
  scale_size(name = "Cost in US$ billion", range = c(1, 20), breaks = c(1, 5, 10, 15, 20))+ 
  scale_color_gradient(low = "yellow", high = "red", na.value = NA) + theme_bw()



##########Temporal plot over time ######
head(df)
unique(df$Domestic)
unique(df$Implementation)
df0 <- df %>% filter(Implementation =="Observed")
df1<- df %>% filter(Domestic =="Pet")
df2<- df %>% filter(Domestic =="Livestook")
df3<- df %>% filter(Domestic =="Beast")

computeAvgTotCost(
  df3,
  cost.column = "Cost_estimate_per_year_2017_USD_exchange_rate",
  year.column = "Impact_year",
  min.year = 1960,
  max.year = 2022
)

summarizeCosts(df2)

unique(df$Domestic)
expanded <- df %>% filter(Domestic=="Beast")

expanded_obs <- expanded[expanded$Implementation %in% c("Observed"),]
sum(expanded_obs$cost_bil) #97.63

global.raw.all <- summarizeCosts(expanded,
                                 cost.column = "cost",
                                 in.millions = FALSE,
                                 minimum.year = 1960,
                                 maximum.year = 2022,
                                 year.breaks = seq(1960, 2022, by = 20))    #you may have to use other intervals, but 10 is like the common basis
global.raw.obs <- summarizeCosts(expanded_obs,
                                 cost.column = "cost",
                                 in.millions = FALSE,
                                 minimum.year = 1960,
                                 maximum.year = 2022,
                                 year.breaks = seq(1960, 2022, by = 20))
global.raw.obs$average.cost.per.period$middle.years <- global.raw.obs$average.cost.per.period$initial_year +
  (global.raw.obs$average.cost.per.period$final_year -
     global.raw.obs$average.cost.per.period$initial_year) / 2
global.raw.all$average.cost.per.period$middle.years <- global.raw.all$average.cost.per.period$initial_year +
  (global.raw.all$average.cost.per.period$final_year -
     global.raw.all$average.cost.per.period$initial_year) / 2
all.costs <- rbind(data.frame(global.raw.obs$average.cost.per.period,
                              cost.type = "Observed"),
                   data.frame(global.raw.all$average.cost.per.period,
                              cost.type = "All"))
all.costs.per.year <- rbind(data.frame(global.raw.obs$cost.per.year,
                                       cost.type = "Observed"),
                            data.frame(global.raw.all$cost.per.year,
                                       cost.type = "All"))

p3 <-ggplot(all.costs) +
  ylab(paste0("Annual cost in US$")) +
  # Points
  geom_point(aes_string(x = "middle.years",
                        y = "annual_cost",
                        col = "cost.type"),
             shape = 15) +
  # Lines between points
  geom_line(aes_string(x = "middle.years",
                       y = "annual_cost",
                       col = "cost.type"),
            linetype = 2) +
  # Horizontal bars (year span)
  geom_segment(aes_string(x = "initial_year",
                          xend = "final_year",
                          y = "annual_cost",
                          yend = "annual_cost",
                          col = "cost.type")) +
  geom_point(data = all.costs.per.year,
             aes(x = year, y = cost,
                 size = number_estimates,
                 col = cost.type),
             alpha = .6) +
  xlab("Year") +
  scale_x_continuous(breaks = global.raw.obs$year.breaks) +
  scale_size_continuous(name = "Number of estimates\nper year",
                        breaks = c(1, 5, 20, 50)) +
  scale_color_brewer(name = "Cost estimations",
                     palette = "Dark2") + # Minimal theme
  scale_y_log10(breaks = 10^(-15:15), # y axis in log 10 with pretty labels
                labels = scales::comma) +
  annotation_logticks(sides = "l")+
  theme_bw() +
  theme(panel.border = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12))
p1+p2+p3



###  alluvial diagram ####
df<- read_xlsx("Expanded_domestic.xlsx", sheet = "Sheet1")
unique(df$Kingdom)
df<- filter(df, !Kingdom=="Plantae")

# install.packages("ggalluvial")
library(ggalluvial)
df$Impacted_sector[df$Impacted_sector== "Authorities-Stakeholders/Public and social welfare"] <- "Diverse"
df$Impacted_sector[df$Impacted_sector== "Agriculture/Forestry"] <- "Diverse"
df$Impacted_sector[df$Impacted_sector== "Agriculture/Public and social welfare"] <- "Diverse"
df$Impacted_sector[df$Impacted_sector== "Health/Public and social welfare"] <- "Diverse"
df$Impacted_sector[df$Impacted_sector== "Agriculture/Authorities-Stakeholders"] <- "Diverse"
df$Impacted_sector[df$Impacted_sector== "AAuthorities-Stakeholders/Health"] <- "Diverse"
df$Impacted_sector[df$Impacted_sector== "Environment/Public and social welfare"] <- "Diverse"
df$Impacted_sector[df$Impacted_sector== "Environment/Health/Public and social welfare"] <- "Diverse"
df$Impacted_sector[df$Impacted_sector== "Agriculture/Environment/Health/Public and social welfare"] <- "Diverse"
df$Impacted_sector[df$Impacted_sector== "Agriculture/Environment/Public and social welfare"] <- "Diverse"
df$Impacted_sector[df$Impacted_sector== "Environment/Fishery"] <- "Diverse"
df$Impacted_sector[df$Impacted_sector== "Authorities-Stakeholders/Fishery"] <- "Diverse"
df$Impacted_sector[df$Impacted_sector== "Agriculture/Environment/Fishery"] <- "Diverse"
df$Impacted_sector[df$Impacted_sector== "Authorities-Stakeholders/Environment/Fishery"] <- "Diverse"
df$Impacted_sector[df$Impacted_sector== "Fishery/Public and social welfare"] <- "Diverse"
df$Impacted_sector[df$Impacted_sector== "Environment/Fishery/Public and social welfare"] <- "Diverse"
df$Impacted_sector[df$Impacted_sector== "Authorities-Stakeholders/Health"] <- "Diverse"


df1<- df %>% group_by(Pathways, Impacted_sector,  Domestic) %>% 
  summarise(Cost=sum(cost_bil),n =n())

#df1 <- df1 %>% drop_na()


ggplot(df1, aes(fill=Impacted_sector, y=Cost, x = reorder(Domestic, -Cost))) + 
  geom_bar(position="fill", stat="identity")




ggplot(data = df1,
       aes(axis1 = Domestic, 
           axis2 = Pathways, 
           axis3=Impacted_sector, 
           y = Cost)) +
  geom_alluvium(aes(fill = Domestic)) +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum))) +
  scale_x_discrete(
                   expand = c(0.15, 0.05)) +
  theme_void() + scale_fill_manual(values = c("red", "orange", "blue")) 


ggplot(data = df1, 
       aes(fill = Domestic, 
           y = Cost, 
           axis1 = Domestic, 
           axis2 = Pathways, 
           axis3 = Impacted_sector)) +
  geom_alluvium(aes(fill = Domestic)) +
  geom_stratum(width = 1, alpha = .5) +
  geom_text(stat = "stratum", 
            aes(label = after_stat(stratum))) +
  scale_x_discrete(expand = c(0.15, 0.05)) +
  theme_void() + 
  scale_fill_manual(values = c("red", "orange", "blue"))+
  labs(x = "Domestic", y = "Cost", 
       stratum = "Pathways", alluvium = "Impacted Sector")




########## World map ######

library(ggplot2)
library(rnaturalearth)

# Load world map data
world_map <- ne_countries(scale = "medium", returnclass = "sf")

a<-df %>% group_by(Geographic_region) %>% 
  summarise(Total_cost=sum(cost_bil),
            n=n())




# Plot the world map, coloring each continent differently
#bar plor
# install.packages("ggplot2")
a<- df %>% group_by(Geographic_region, Domestic) %>% summarise(Total_cost=sum(cost_bil))
a<- a %>% filter(!Geographic_region=="Central America")
unique(a$Geographic_region)

a$Geographic_region <- factor(a$Geographic_region, 
                    levels = c("Antarctic-Subantarctic", "South America",  "Africa",
                               "Diverse", "Asia", "Europe","North America", "Oceania"))

str(a)
ggplot(a, aes(x = Geographic_region, y = Total_cost, fill=Domestic)) +
  geom_bar(stat = "identity", position = "dodge2") +
  coord_flip() +
  scale_y_break(c(20, 60)) +
  theme_bw()



################ Grafico de mancuernas #######

a<- df %>% group_by(Island, Domestic) %>% summarise(Cost= sum(cost_bil),
                                          n=n())

str(a)
a$Domestic <- as.factor(a$Domestic)



a$Domestic <- factor(a$Domestic, 
       levels = c( "Beast",  "Livestook",      "Pet"))

ggplot(a, aes(x = Domestic, y = Cost,  color = Domestic, shape=Island)) + 
  geom_point(size = 3.5) + scale_y_continuous(trans='log10') +
  theme_bw()  + coord_flip()




############# Donna plot ###############

a<-df %>% group_by(Domestic, Method_reliability, Implementation) %>% 
  dplyr::summarise(Total_cost=sum(cost_bil),
                   n=n())%>% ungroup()

a<- a %>% unite("reliability_Implementation", Method_reliability:Implementation, 
            remove = FALSE)
            
            
a1<-  a  %>%  filter(Domestic=="Beast")
a2<-  a %>% filter(Domestic=="Livestook")
a3<-  a %>% filter(Domestic=="Pet")


b <- a %>%
  group_by(Domestic) %>%
  dplyr::summarise(Total_cost1 = sum(Total_cost)) %>%
  ungroup()

result <- a %>%
  left_join(b, by = "Domestic") %>%
  mutate(percentage = (Total_cost / Total_cost1) * 100)
result

hsize <- 3

b <- b%>% 
  mutate(x = hsize)

p1<- ggplot(a1, aes(x = 3, y = Total_cost, fill = reliability_Implementation)) +
  geom_col(color = "black") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#FFF7FB", "#D0D1E6","red","yellow")) +
  xlim(c(0.2, 3 + 0.5)) +theme_bw()

p2<- ggplot(a2, aes(x = 3, y = Total_cost, fill = reliability_Implementation)) +
  geom_col(color = "black") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#FFF7FB", "#D0D1E6","red","yellow")) +
  xlim(c(0.2, 3 + 0.5)) +theme_bw()

p3<- ggplot(a3, aes(x = 3, y = Total_cost, fill = reliability_Implementation)) +
  geom_col(color = "black") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#FFF7FB", "#D0D1E6","red","yellow")) +
  xlim(c(0.2, 3 + 0.5)) +theme_bw()


p1+p2+p3


ggplot(a, aes(fill=reliability_Implementation, y=Total_cost, x=Domestic)) + 
  geom_bar(position="fill", stat="identity")



# Create data
group <- c(rep("group-1",4),rep("group-2",2),rep("group-3",3))
subgroup <- paste("subgroup" , c(1,2,3,4,1,2,1,2,3), sep="-")
value <- c(13,5,22,12,11,7,3,1,23)
data <- data.frame(group,subgroup,value)

a<-df %>% group_by(Type_of_cost_merged, Domestic) %>% 
  summarise(Total_cost=sum(cost_bil),
            n=n())

a1 <- a %>% group_by(Domestic,Type_of_cost_merged) %>% summarise(Total_cost=sum(Total_cost),
                                         n=n())

a2 <- filter(a, Domestic=="Pet")
a3 <- filter(a, Domestic=="Livestook")
a4 <- filter(a, Domestic=="Beast")

#### Tree map ####
ggplot(a1, aes(area = Total_cost, fill = Domestic, label = Type_of_cost_merged)) +
  geom_treemap() 

p1<- ggplot(a2, aes(area = Total_cost, fill = Type_of_cost_merged, label = Type_of_cost_merged)) +
  geom_treemap()

p2<- ggplot(a3, aes(area = Total_cost, fill = Type_of_cost_merged, label = Type_of_cost_merged)) +
  geom_treemap()

p3<- ggplot(a4, aes(area = Total_cost, fill = Type_of_cost_merged, label = Type_of_cost_merged)) +
  geom_treemap()
p1+p2+p3


##### Wild / domestic  #### 
df<- read_xlsx("Expanded_domestic.xlsx", sheet = "Sheet1")
unique(df$Kingdom)
df<- filter(df, !Kingdom=="Plantae")
df<- filter(df, !Species=="Boiga irregularis")
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

expanded <- expandYearlyCosts(df1, #your help file
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


data<- data %>% group_by(Wild_domestic, Implementation) %>% 
  summarise(Cost=sum(cost_bil, na.rm=T))

p1<- ggplot(data, aes(fill=Implementation, y=Cost, x=Wild_domestic)) + 
  geom_bar(position="stack", stat="identity") + ylim(0,250)

data <- bind_rows(expanded,df)

data<- data[,c(1,3,20,51,53,57,69,70)]
data<- data %>% group_by(Wild_domestic, Type_of_cost_merged) %>% 
  summarise(Cost=sum(cost_bil, na.rm=T))

p2<- ggplot(data, aes(fill=Type_of_cost_merged, y=Cost, x=Wild_domestic)) + 
  geom_bar(position="stack", stat="identity") + ylim(0,250)

p1+p2 


data2<- data %>% group_by(Wild_domestic, Implementation) %>% summarise(cost=sum(cost_bil))
data2$n_species <- c("36","36","107", "107")

data2$cost_species <- data2$cost/ as.numeric(data2$n_species)




a<-data2 %>% group_by(Wild_domestic, Implementation) %>% 
  dplyr::summarise(Total_cost=sum(cost),
                   n=n())

b <- a %>%
  group_by(Wild_domestic) %>%
  dplyr::summarise(Total_cost1 = sum(Total_cost)) %>%
  ungroup()

result <- a %>%
  left_join(b, by = "Wild_domestic") %>%
  mutate(percentage = (Total_cost / Total_cost1) * 100)
result
