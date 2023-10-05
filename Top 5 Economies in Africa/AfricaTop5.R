library(data.table)
library(dplyr)
library(tidyr)


setwd("C:\\Users\\Ulrich\\SkyDrive\\Documents\\Senan\\Analyst Kingdom\\Top 5 Economies in Africa")

AfricaTop5 <- fread("Inputs\\Top 5 Economies in Africa.csv")

#Transform Country Name and Country Code into FACTOR
AfricaTop5$`Country Name` <- factor(AfricaTop5$`Country Name`)
AfricaTop5$`Country Code` <- factor(AfricaTop5$`Country Code`)

#Turning the Years into Column using the tidyr function "gather".
#ECOWASHIST is the dataframe name.
#"Year" contains all the columns gathered into a row
#"Value" represent the values for those years.
#5:29 are the columns that are gathered.
AfricaTop5 <- AfricaTop5 %>% gather(Year, Value, 5:29)

#Removing unecessary charaters and Factorising Year
AfricaTop5$Year <- factor(AfricaTop5$Year)

fwrite(AfricaTop5, "Outputs\\Africatop5.csv")


#------------------------------------------------------------------------------------------------------


#Remove all rows with NAs
#ECOWASHIST <- ECOWASHIST[complete.cases(ECOWASHIST),]

#This is the correct formula to spread a column contents into different columns.
#But not enough memory to execute the task
#ECOWASHIST <- ECOWASHIST %>% spread(Indicator.Name, Value)

#Filtering out unique variable by Indicator Name

ECOWASSUBSET <- ECOWASHIST %>% filter(Indicator.Name=="Domestic credit to private sector by banks (% of GDP)" |
                                        Indicator.Name == "GDP per capita (constant 2010 US$)" |
                                        Indicator.Name =="GDP growth (annual %)" |
                                        Indicator.Name =="Foreign direct investment, net outflows (% of GDP)" |
                                        Indicator.Name =="Foreign direct investment, net inflows (% of GDP)" |
                                        Indicator.Name =="GDP at market prices (constant 2010 US$)" |
                                        Indicator.Name =="Final consumption expenditure, etc. (annual % growth)" |
                                        Indicator.Name =="Final consumption expenditure (constant 2010 US$)" |
                                        Indicator.Name =="Central government debt, total (% of GDP)" |
                                        Indicator.Name =="Gross domestic savings (current US$)")

#Spreading rows into columns
ECOWASSUBSET <- spread(ECOWASSUBSET, Indicator.Name, Value)

#Replacing all NA's with 0
ECOWASSUBSET[is.na(ECOWASSUBSET)] <- 0

#Saving file

write.csv(ECOWASSUBSET, file="Outputs\\ECOWASSUBSET.csv")
