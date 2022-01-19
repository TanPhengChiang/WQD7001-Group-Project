library(tidyverse)
library(tidyr)
library(ggplot2)
library(dplyr)
library(readxl)
suicide <- read.csv('master.csv', sep = ',', )

#Data understanding
glimpse(suicide)

#Drop HDI.for.year and generation column
sui_new <- suicide[,!(names(suicide) %in% c("HDI.for.year",
                                            "generation",
                                            "suicides.100k.pop",
                                            "country.year"))]

#Check for missing value
colSums(is.na(sui_new))
#no missing value

#Check for empty cell
colSums(sui_new=="")
#no empty cell

#Check for 0 population
sum(sui_new$Population == 0)
#all populations are correct

#Remove comma in GDP
sui_new$gdp_for_year.... <- gsub(",", "", sui_new$gdp_for_year....)

#Convert GDP to numeric data type
sui_new$gdp_for_year.... <- as.numeric(sui_new$gdp_for_year....)

#Calculate suicide rate per 100k population
sui_new <- sui_new %>%
  mutate(suicide100k = suicides_no / (population / 100000))

#Rename column
colnames(sui_new) <- c("Country","Year","Sex","Age",
                    "Suicides_no","Population","GDP","GDP_per_capita",
                    "Suicide_100k_pop")

#Export master suicide dataset
write.csv(dead, "Master_Suicide_Data.csv", row.names = FALSE)

#Data visualizations
#Group by sex
ggplot(sui_new, aes(Year, Suicide_100k_pop)) + 
  geom_point(aes(color=Sex)) + facet_wrap(~Country)

#Group by age
ggplot(sui_new, aes(Year, Suicide_100k_pop)) + 
  geom_point(aes(color=Age)) + facet_wrap(~Country)
