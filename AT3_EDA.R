#START 
install.packages(tidyverse) # Install tidyverse package 
install.packages("corrplot") 
library(tidyverse) # Load tidyverse package 
library(corrplot)

#Reading datasets
country_code_mapping <- read.csv("Metadata_Country.csv") 
data1 <- read.csv("GDP_PPP_per_capita.csv", header = FALSE) #GDP per capita, PPP, table import w/o header 
data2 <- read.csv("Health_Expenditure_per_capita_PPP.csv", header = FALSE) #Health Expenditure per capita, PPP, table import w/o header 
data3 <- read.csv("Hospital_Beds_per_capita.csv", header = FALSE) #Hospital Beds per capita table import w/o header 
data4 <- read.csv("Doctors_per_capita.csv")
data5 <- read.csv("total-cancer-deaths-by-type.csv") # Cancer deaths datasets
data6 <- read.csv("population.csv", header = FALSE) # Population datasets

#Removing the top 4 rows and making row 4 as header for GDP Per Capita Table
colnames(data1) <- data1[3, ] 
data1_new <- data1 %>% slice(4:n())

#Removing the top 4 rows and making row 4 as header for Health Expenditure per capita table
colnames(data2) <- data2[3, ] 
data2_new <- data2 %>% slice(4:n())

#Removing the top 4 rows and making row 4 as header for Hospital beds per capita table
colnames(data3) <- data3[3, ] 
data3_new <- data3 %>% slice(4:n())

# Removing the top 4 rows and making row 4 as header for Population
colnames(data6) <- data6[3, ] 
data6 <- data6 %>% slice(4:n())

#Replacing Missing value of gdp_per_capita
gdp_per_capita <- data1_new # Duplicate data frame 
gdp_per_capita$`2019`[is.na(gdp_per_capita$`2019`)] <- rowMeans(gdp_per_capita[,c(44:63)], na.rm = TRUE)[is.na(gdp_per_capita$`2019`)] # Replace by row means gdp_per_capita

#Replacing Missing value of health_exp_per_capita
health_exp_per_capita <- data2_new # Duplicate data frame 
health_exp_per_capita$`2019`[is.na(health_exp_per_capita$`2019`)] <- rowMeans(health_exp_per_capita[,c(55:64)], na.rm = TRUE)[is.na(health_exp_per_capita$`2019`)] # Replace by row means health_exp_per_capita # Print new data frame

#Replacing Missing value of hosp_bed_per_capita
hosp_bed_per_capita <- data3_new # Duplicate data frame 
hosp_bed_per_capita$`2019`[is.na(hosp_bed_per_capita$`2019`)] <- rowMeans(hosp_bed_per_capita[,c(45:64)], na.rm = TRUE)[is.na(hosp_bed_per_capita$`2019`)] # Replace by row means hosp_bed_per_capita

#Replacing Missing value of dr_per_capita
dr_per_capita <- data4 # Duplicate data frame 
dr_per_capita$`X2019`[is.na(dr_per_capita$X2019)] <- rowMeans(dr_per_capita[,c(17:31)], na.rm = TRUE)[is.na(dr_per_capita$X2019)] # Replace by row means dr_per_capita

# Replacing Missing value of cancer_deaths
data5 <- data5 %>% filter(data5$Year == 2019)
cancer_deaths <- data5[,c(1,2,3,7)] 

# Replacing Missing value of population
population <- data6 # Duplicate data frame
population$`2019`[is.na(population$`2019`)] <- rowMeans(population[,c(45:63)], na.rm = TRUE)[is.na(population$`2019`)]  # Replace by row means

#Renaming column
names(dr_per_capita)[names(dr_per_capita) == 'X2019'] <- 'Dr_per_10000' 
names(gdp_per_capita)[names(gdp_per_capita) == '2019'] <- 'gdp_per_capita'
names(health_exp_per_capita)[names(health_exp_per_capita) == '2019'] <- 'health_exp_per_capita'
names(hosp_bed_per_capita)[names(hosp_bed_per_capita) == '2019'] <- 'hos_bed_per_1000'
names(cancer_deaths)[names(cancer_deaths) == 'Lung_cancer'] <- 'Lung_cancer_death'
names(population)[names(population) == '2019'] <- 'Population'

#Merging all datasets
m1 = select(merge(x = country_code_mapping, y = dr_per_capita, by.x = "Country.Code", by.y = "SpatialDimValueCode", all.x = TRUE), c(1,2,3,5,37)) 
m2 = select(merge(x=m1, y=gdp_per_capita, by.x = "Country.Code", by.y = "Country Code", all.x = TRUE), c(1:5,'gdp_per_capita')) 
m3 = select(merge(x=m2, y=health_exp_per_capita, by.x = "Country.Code", by.y = "Country Code", all.x = TRUE), c(1:6,'health_exp_per_capita')) 
m4 = select(merge(x=m3, y=hosp_bed_per_capita, by.x = "Country.Code", by.y = "Country Code", all.x = TRUE),c(1:7,'hos_bed_per_1000'))
m5 = select(merge(x=m4, y=cancer_deaths, by.x = "Country.Code", by.y = "Code", all.x = TRUE), c(1:8,'Lung_cancer_death'))
final_data = select(merge(x=m5, y=population, by.x = "Country.Code", by.y = "Country Code", all.x = TRUE),c(1:9,"Population"))

final_data <- within(final_data, Lung_cancer_death_rate <- final_data$Lung_cancer_death*100000/final_data$Population)
final_data <- final_data[c(1,4,5,6,7,8,11)]

dim(final_data)

final_data = na.omit(final_data)
dim(final_data)

#EDA

#Finding top 6 rows
head(final_data)

#Summarizing the descriptive stattistics
summary(final_data)

#Checking number of rows and columns
dim(final_data)


#Finding correlation b/w variables
cor(final_data[c(3:7)])

#Plotting correlation matrix graph
corrplot(cor(final_data[c(3:7)]), method = 'color') #Method 1: By showing shading 

corrplot(cor(final_data[c(3:7)]), method = 'number') #Method 2: By showing correlation number

#Plot b/w gdp_per_capita vs hos_bed_per_1000
ggplot(final_data) + aes(x = gdp_per_capita, y = Lung_cancer_death_rate) + geom_point(shape = "circle", size = 1.5, colour = "Red") + theme_minimal()

#Plot b/w gdp_per_capita vs health_exp_per_capita
ggplot(final_data) + aes(x = hos_bed_per_1000, y = Lung_cancer_death_rate) + geom_point(shape = "circle", size = 1.5, colour = "Green") + theme_minimal()

#Plot b/w gdp_per_capita vs Dr_per_10000
ggplot(final_data) + aes(x = Dr_per_10000, y = Lung_cancer_death_rate) + geom_point(shape = "circle", size = 1.5, colour = "Blue") + theme_minimal()

#Plot b/w gdp_per_capita vs Dr_per_10000
ggplot(final_data) + aes(x = health_exp_per_capita, y = Lung_cancer_death_rate) + geom_point(shape = "circle", size = 1.5, colour = "Black") + theme_minimal()

# Export
write.csv(final_data,"/Users/sareem/Library/CloudStorage/OneDrive-UTS/Statistical Thinking for Data Science/AT2/STDS_Lung_Cancer_Data_Analysis/lung_cancer.csv", row.names = FALSE)

#END
