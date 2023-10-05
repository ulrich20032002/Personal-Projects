library(data.table)
library(dplyr)
library(tidyr)
library(stringr)


setwd("C:\\Users\\Ulrich\\SkyDrive\\Documents\\Senan\\Analyst Kingdom\\Top 5 Economies in Africa")

AfricaTop5 <- head(fread("Inputs\\Top 5 Economies in Africa.csv"),-5)

#Transform Country Name and Country Code into FACTOR
AfricaTop5$`Country Name` <- factor(AfricaTop5$`Country Name`)
AfricaTop5$`Country Code` <- factor(AfricaTop5$`Country Code`)

#Turning the Years into Column using the tidyr function "gather". 
#ECOWASHIST is the dataframe name. 
#"Year" contains all the columns gathered into a row
#"Value" represent the values for those years. 
#5:29 are the columns that are gathered.
AfricaTop5 <- AfricaTop5 %>% gather(Year, Value, 5:29)

#Remove unecessary data from year
#AfricaTop5$Year is the column from which the unecessary data will be removed
#1 and 5 are the start and end of what we want to keep
#"" shows there is a space/blank before what we want to exclude
AfricaTop5$Year <- word(AfricaTop5$Year,1,5, sep = "")

#Removing unecessary charaters and Factorising Year
AfricaTop5$Year <- factor(AfricaTop5$Year)

#------------------------------------------------------------------------------------------------------


#Remove all rows with NAs
#ECOWASHIST <- ECOWASHIST[complete.cases(ECOWASHIST),]

#This is the correct formula to spread a column contents into different columns.
#But not enough memory to execute the task
#ECOWASHIST <- ECOWASHIST %>% spread(`Series Name`, Value)

#Filtering out unique variable by Series Name

AfricaTop5Subset <- AfricaTop5 %>% filter(`Series Name` == "Domestic credit to private sector by banks (% of GDP)"| 
                                          `Series Name` == "Domestic credit to private sector by banks (% of GDP)"|
                                          `Series Name` == "GDP per capita (constant 2010 US$)"|
                                          `Series Name` == "GDP growth (annual %)"|
                                          `Series Name` == "GDP (constant 2010 US$)" |
                                          `Series Name` == "Foreign direct investment, net outflows (% of GDP)"|
                                          `Series Name` == "Foreign direct investment, net inflows (% of GDP)"|
                                          `Series Name` == "Household final consumption expenditure (constant 2010 US$)"|
                                          `Series Name` == "Household final consumption expenditure (current US$)"|
                                          `Series Name` == "Household final consumption expenditure (% of GDP)"|
                                          `Series Name` == "General government final consumption expenditure (annual % growth)"|
                                          `Series Name` == "Household final consumption expenditure (annual % growth)"|
                                          `Series Name` == "General government final consumption expenditure (constant 2010 US$)"|
                                          `Series Name` == "General government final consumption expenditure (% of GDP)"|
                                          `Series Name` == "Final consumption expenditure (constant 2010 US$)"|
                                          `Series Name` == "Final consumption expenditure (annual % growth)"|
                                          `Series Name` == "Final consumption expenditure (% of GDP)"|
                                          `Series Name` == "Central government debt, total (% of GDP)"|
                                          `Series Name` == "Gross domestic savings (current US$)"|
                                          `Series Name` == "Population, total"|
                                          `Series Name` == "Population growth (annual %)"|
                                          `Series Name` == "PM2.5 air pollution, mean annual exposure (micrograms per cubic meter)"|
                                          `Series Name` == "Urban population (% of total)"|
                                          `Series Name` == "Merchandise exports by the reporting economy (current US$)"|
                                          `Series Name` == "Researchers in R&D (per million people)"|
                                          `Series Name` == "Technicians in R&D (per million people)"|
                                          `Series Name` == "Merchandise trade (% of GDP)"|
                                          `Series Name` == "Agricultural raw materials imports (% of merchandise imports)"|
                                          `Series Name` == "Food imports (% of merchandise imports)"|
                                          `Series Name` == "Fuel imports (% of merchandise imports)"|
                                          `Series Name` == "ICT goods imports (% total goods imports)"|
                                          `Series Name` == "Insurance and financial services (% of commercial service imports)"|
                                          `Series Name` == "Manufactures imports (% of merchandise imports)"|
                                          `Series Name` == "Ores and metals imports (% of merchandise imports)"|
                                          `Series Name` == "Merchandise imports from economies in the Arab World (% of total merchandise imports)"|
                                          `Series Name` == "Merchandise imports (current US$)"|
                                          `Series Name` == "Merchandise imports from high-income economies (% of total merchandise imports)"|
                                          `Series Name` == "Merchandise imports from low- and middle-income economies outside region (% of total merchandise imports)"|
                                          `Series Name` == "Merchandise imports from low- and middle-income economies in East Asia & Pacific (% of total merchandise imports)"|
                                          `Series Name` == "Merchandise imports from low- and middle-income economies in Europe & Central Asia (% of total merchandise imports)"|
                                          `Series Name` == "Merchandise imports from low- and middle-income economies in Latin America & the Caribbean (% of total merchandise imports)"|
                                          `Series Name` == "Merchandise imports from low- and middle-income economies in Middle East & North Africa (% of total merchandise imports)" |
                                          `Series Name` == "Merchandise imports from low- and middle-income economies in South Asia (% of total merchandise imports)"|
                                          `Series Name` == "Merchandise imports from low- and middle-income economies in Sub-Saharan Africa (% of total merchandise imports)"|
                                          `Series Name` == "Merchandise imports by the reporting economy, residual (% of total merchandise imports)"|
                                          `Series Name` == "Merchandise imports by the reporting economy (current US$)"|
                                          `Series Name` == "Merchandise imports from low- and middle-income economies within region (% of total merchandise imports)"|
                                          `Series Name` == "Computer, communications and other services (% of commercial service imports)"|
                                          `Series Name` == "Commercial service imports (current US$)"|
                                          `Series Name` == "Transport services (% of commercial service imports)"|
                                          `Series Name` == "Travel services (% of commercial service imports)"|
                                          `Series Name` == "Agricultural raw materials exports (% of merchandise exports)"|
                                          `Series Name` == "Food exports (% of merchandise exports)"|
                                          `Series Name` == "Fuel exports (% of merchandise exports)"|
                                          `Series Name` == "ICT goods exports (% of total goods exports)"|
                                          `Series Name` == "Insurance and financial services (% of commercial service exports)"|
                                          `Series Name` == "Manufactures exports (% of merchandise exports)"|
                                          `Series Name` == "Ores and metals exports (% of merchandise exports)"|
                                          `Series Name` == "Merchandise exports to economies in the Arab World (% of total merchandise exports)"|
                                          `Series Name` == "Merchandise exports (current US$)"|
                                          `Series Name` == "Merchandise exports to high-income economies (% of total merchandise exports)"|
                                          `Series Name` == "Merchandise exports to low- and middle-income economies outside region (% of total merchandise exports)"|
                                          `Series Name` == "Merchandise exports to low- and middle-income economies in East Asia & Pacific (% of total merchandise exports)"|
                                          `Series Name` == "Merchandise exports to low- and middle-income economies in Europe & Central Asia (% of total merchandise exports)"|
                                          `Series Name` == "Merchandise exports to low- and middle-income economies in Latin America & the Caribbean (% of total merchandise exports)"|
                                          `Series Name` == "Merchandise exports to low- and middle-income economies in Middle East & North Africa (% of total merchandise exports)"|
                                          `Series Name` == "Merchandise exports to low- and middle-income economies in South Asia (% of total merchandise exports)"|
                                          `Series Name` == "Merchandise exports to low- and middle-income economies in Sub-Saharan Africa (% of total merchandise exports)"|
                                          `Series Name` == "Merchandise exports by the reporting economy, residual (% of total merchandise exports)"|
                                          `Series Name` == "Merchandise exports by the reporting economy (current US$)"|
                                          `Series Name` == "Merchandise exports to low- and middle-income economies within region (% of total merchandise exports)"|
                                          `Series Name` == "Computer, communications and other services (% of commercial service exports)"|
                                          `Series Name` == "Commercial service exports (current US$)"|
                                          `Series Name` == "High-technology exports (current US$)"|
                                          `Series Name` == "High-technology exports (% of manufactured exports)"|
                                          `Series Name` == "Transport services (% of commercial service exports)"|
                                          `Series Name` == "Travel services (% of commercial service exports)")
                   


#Spreading rows into columns
AfricaTop5Subset <- spread(AfricaTop5Subset, `Series Name`, Value)

#Renaming column: "GDP (constant 2010 US$)" to "GDP at market prices (constant 2010 US$)"

names(AfricaTop5Subset$`GDP (constant 2010 US$)`) <- "GDP at market prices (constant 2010 US$)"

#Replacing all NA's with 0
AfricaTop5Subset[is.na(AfricaTop5Subset)] <- 0 

#Saving file

fwrite(AfricaTop5Subset, "Outputs\\Africatop5Subset.csv")

