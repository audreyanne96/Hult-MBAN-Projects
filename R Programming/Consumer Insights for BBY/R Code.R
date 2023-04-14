# A1: Direct Mail Household EDA
# Visualizing & Analyzing Data with R: Methods & Tools - DAT-5323 - BMBAN1

# Audrey Anne Arocha
# March 29, 2023

# ENVIRONMENT
# Setting working directory
setwd("~/Desktop/MBAN R/hult-git-R/personalFiles/A1")

# Setting libraries
library(tidyverse)
library(maps)
library(dplyr)
library(mice)
library(ggplot2)
library(ggthemes)
library(leaflet)
library(RColorBrewer)
library(scales)
library(forcats)

# Accessing databases (r_ for raw)
# Consumer purchasing habits
r_consumer <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A1_Household_Direct_Mail/consumerData_training15K_studentVersion.csv')
# Household donations history
r_donations <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A1_Household_Direct_Mail/DonationsData_training15K_studentVersion.csv')
# BBY membership database
r_membership <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A1_Household_Direct_Mail/inHouse_EDA_10k.csv')
# Household magazine subscription history
r_magazine <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A1_Household_Direct_Mail/magazineData_training15K_studentVersion.csv')
# Household political leanings
r_political <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A1_Household_Direct_Mail/politicalData_training15K_studentVersion.csv')

# CREATING CLEANING FUNCTIONS
# change "Yes" to logical TRUE, all else FALSE
to_logical <- function(x) {
  return(ifelse(x == "Yes", TRUE,
                (ifelse(x == "", FALSE, NA))))
}
# retain only numerical values
clean_numeric <- function(x) {
  x <- gsub("[^[:digit:]]", "", x)
  x <- ifelse(x == "", "0", x)
  x <- as.integer(x)
  return(x)
}
# for donations database - determining who are donating overall and who aren't
any_donation <- function(df) {
  cols <- toclean_donations2
  any_true <- apply(df[, cols], 1, function(x) any(!is.na(x) & x))
  all_false <- apply(df[, cols], 1, function(x) all(is.na(x) | x == FALSE))
  result <- ifelse(any_true, TRUE, FALSE)
  result[all_false] <- FALSE
  return(result)
}

# DATA CLEANING
# In order of database importance

# Cleaning - Membership
# Instantiating separate df for cleaning to retain df for raw table
membership <- r_membership
# Structure
str(membership)
# Gender
unique(membership$Gender)
membership$Gender <- factor(membership$Gender)
# Age
membership$Age <- round(membership$Age,0)
## city
#membership$city <- gsub("[^[:alpha:]]", "", membership$city)
#membership$city <- trimws(membership$city)
#membership$city <- factor(membership$city)
# Dollar amounts - HomePurchasePrice, LandValue,EstHomeValue
membership$HomePurchasePrice <- gsub("[^[:digit:]]","",membership$HomePurchasePrice)
membership$LandValue <- gsub("[^[:digit:]]","",membership$LandValue)
membership$EstHomeValue <- gsub("[^[:digit:]]","",membership$EstHomeValue)
membership$HomePurchasePrice <- as.integer(membership$HomePurchasePrice)
membership$LandValue <- as.integer(membership$LandValue)
membership$EstHomeValue <- as.integer(membership$EstHomeValue)
# State
unique(membership$state)
membership$state <- gsub("NULL",NA,membership$state)
membership$state <- factor(membership$state)
# DwellingUnitSize
membership$DwellingUnitSize <- trimws(membership$DwellingUnitSize)
membership$DwellingUnitSize <- factor(membership$DwellingUnitSize)
# PropertyType
membership$PropertyType <- trimws(membership$PropertyType)
membership$PropertyType <- factor(membership$PropertyType)
# y_householdSpend
membership$y_householdSpend <- as.integer(membership$y_householdSpend)
# Transforming blank values to nulls
membership[membership == ""] <- NA
round(colSums(is.na(membership))/nrow(membership)*100,2) # percentage of nulls
# Structure of cleaned df
str(membership)
head(membership,5)
tail(membership,5)
# Dropping irrelevant columns
colnames(membership) # to get column names
columns_membership <- c("tmpID",
                        #"FirstName", - removed since likely not to be relevant in market analysis
                        #"LastName", - removed since likely not to be relevant in market analysis
                        "Gender",
                        "Age",
                        #"TelephonesFullPhone", - removed since likely not to be relevant in market analysis
                        #"lat", - too specific
                        #"lon", - too specific
                        # "county", - too many categories
                        #"city", - too many categories
                        "state",
                        #"fips", # duplicate location information
                        #"stateFips", duplicate location information
                        #"HomePurchasePrice", - out of focus
                        #"LandValue", - out of focus
                        #"DwellingUnitSize",- out of focus
                        "storeVisitFrequency",
                        #"PropertyType", - out of focus
                        #"EstHomeValue", - out of focus
                        "y_householdSpend")
final_membership <- subset(membership, select = columns_membership)
# Structure and summary of final cleaned df
round(colSums(is.na(final_membership))/nrow(final_membership)*100,2) # percentage of nulls
str(final_membership)
summary(final_membership)

# Cleaning - Consumer
# Instantiating separate df for cleaning and to retain df for raw table
consumer <- r_consumer
# Structure
str(consumer)
# ResidenceHHGenderDescription
unique(consumer$ResidenceHHGenderDescription)
consumer$ResidenceHHGenderDescription <- gsub(" Household","",consumer$ResidenceHHGenderDescription)
consumer$ResidenceHHGenderDescription <- trimws(consumer$ResidenceHHGenderDescription)
consumer$ResidenceHHGenderDescription <- factor(consumer$ResidenceHHGenderDescription)
# BroadEthnicGroupings - used instead of EthnicDescription since has less nulls
unique(consumer$BroadEthnicGroupings)
consumer$BroadEthnicGroupings <- gsub("\\(.*","",consumer$BroadEthnicGroupings)
consumer$BroadEthnicGroupings <- gsub("\\/.*","",consumer$BroadEthnicGroupings)
consumer$BroadEthnicGroupings <- trimws(consumer$BroadEthnicGroupings)
consumer$BroadEthnicGroupings <- factor(consumer$BroadEthnicGroupings)
#  PresenceOfChildrenCode - "modeled likely or not likely" is disregarded, assumed modeling was appropriate
unique(consumer$PresenceOfChildrenCode)
consumer$PresenceOfChildrenCode_C <- ifelse(consumer$PresenceOfChildrenCode == "Modeled Likely to have a child","Likely to have a child",
                                            ifelse(consumer$PresenceOfChildrenCode == "Modeled Not as Likely to have a child", "Not Likely to have a child",
                                                   consumer$PresenceOfChildrenCode))
unique(consumer$PresenceOfChildrenCode_C)
consumer$PresenceOfChildrenCode_C <- factor(consumer$PresenceOfChildrenCode_C)
# HomeOwnerRenter - "likely" is disregarded, assumed modeling was appropriate
unique(consumer$HomeOwnerRenter)
consumer$HomeOwnerRenter <- gsub("Likely ","",consumer$HomeOwnerRenter)
consumer$HomeOwnerRenter <- trimws(consumer$HomeOwnerRenter)
consumer$HomeOwnerRenter <- factor(consumer$HomeOwnerRenter)
# NetWorth
unique(consumer$NetWorth)
consumer$NetWorth <- factor(consumer$NetWorth)
# Investor
unique(consumer$Investor)
consumer$Investor <- to_logical(consumer$Investor)
# BusinessOwner
unique(consumer$BusinessOwner)
consumer$BusinessOwner <- to_logical(consumer$BusinessOwner)
# Education - likelihood is disregarded, assumed modeling was appropriate
unique(consumer$Education)
consumer$Education <- gsub("\\-.*","",consumer$Education)
consumer$Education <- gsub("Unknown",NA,consumer$Education)
consumer$Education <- trimws(consumer$Education)
consumer$Education <- factor(consumer$Education)
# OccupationIndustry
unique(consumer$OccupationIndustry)
consumer$OccupationIndustry <- gsub("\\/.*","",consumer$OccupationIndustry)
consumer$OccupationIndustry <- gsub("Unknown",NA,consumer$OccupationIndustry)
consumer$OccupationIndustry <- trimws(consumer$OccupationIndustry)
consumer$OccupationIndustry <- factor(consumer$OccupationIndustry)
# Ownership of animals - individually these columns have 80%+ nulls, taken together to be more efficient
unique(consumer$HorseOwner)
unique(consumer$CatOwner)
unique(consumer$DogOwner)
unique(consumer$OtherPetOwner)
consumer$AnimalOwner <- ifelse(consumer$HorseOwner == "Yes",TRUE,
                               ifelse(consumer$CatOwner == "Yes", TRUE,
                                      ifelse(consumer$DogOwner == "Yes", TRUE,
                                             ifelse(consumer$OtherPetOwner == "Yes", TRUE, FALSE))))
unique(consumer$AnimalOwner)
# HomeOffice
unique(consumer$HomeOffice)
consumer$HomeOffice <- to_logical(consumer$HomeOffice)
# ComputerOwnerInHome
unique(consumer$ComputerOwnerInHome) 
consumer$ComputerOwnerInHome <- ifelse(consumer$ComputerOwnerInHome == "Yes",TRUE,FALSE)
# Transforming blank values to nulls
consumer[consumer == ""] <- NA
round(colSums(is.na(consumer))/nrow(consumer)*100,2) # percentage of nulls
# Structure
str(consumer)
head(consumer,5)
tail(consumer,5)
summary(consumer)
# Dropping irrelevant columns
colnames(consumer) # to get column names
columns_consumer <- c("tmpID",
                        #"ResidenceHHGenderDescription", - out of focus
                        #"EthnicDescription", - dropped because duplicate description with higher % of nulls
                        "BroadEthnicGroupings",
                        #"PresenceOfChildrenCode",- dropped in preference of cleaned column
                        #"ISPSA", - too specific
                        #"HomeOwnerRenter", - out of focus
                        #"MosaicZ4", - dropped because 79% null, persona seems too specific to be a singular data point
                        #"MedianEducationYears", out of focus
                        "NetWorth",
                        #"Investor",out of focus
                        #"BusinessOwner",out of focus
                        "Education",
                        "OccupationIndustry",
                        # "HorseOwner",- dropped due to % of nulls, but information used in combined column AnimalOwner
                        # "CatOwner",- dropped due to % of nulls, but information used in combined column AnimalOwner
                        # "DogOwner",- dropped due to % of nulls, but information used in combined column AnimalOwner
                        # "OtherPetOwner",- dropped due to % of nulls, but information used in combined column AnimalOwner
                        "HomeOffice",
                        #"BookBuyerInHome", - dropped, assumed irrelevant due to specificity and luxury connotation, project is likely a mass market retailer
                        #"UpscaleBuyerInHome", - dropped, assumed irrelevant due to specificity and luxury connotation, project is likely a mass market retailer
                        #"BuyerofAntiquesinHousehold", - dropped, assumed irrelevant due to specificity and luxury connotation, project is likely a mass market retailer
                        #"BuyerofArtinHousehold", - dropped, assumed irrelevant due to specificity and luxury connotation, project is likely a mass market retailer
                        #"GeneralCollectorinHousehold", - dropped, assumed irrelevant due to specificity and luxury connotation, project is likely a mass market retailer
                        #"BooksAudioReadinginHousehold", - dropped, assumed irrelevant due to specificity and luxury connotation, project is likely a mass market retailer
                        "ComputerOwnerInHome",
                        "PresenceOfChildrenCode_C",
                        "AnimalOwner")
final_consumer <- subset(consumer, select = columns_consumer)
# Structure and summary of final cleaned df
round(colSums(is.na(final_consumer))/nrow(final_consumer)*100,2) # percentage of nulls
str(final_consumer)
summary(final_consumer)

# Cleaning - Magazines
magazine <- r_magazine
# Structure
str(magazine)
# Magazine columns with counts of purchases
colnames(magazine) # to get column names
toclean_magazine = c("FamilyMagazineInHome",
                     "FemaleOrientedMagazineInHome",
                     "ReligiousMagazineInHome",
                     "GardeningMagazineInHome",
                     "CulinaryInterestMagazineInHome",
                     "HealthFitnessMagazineInHome",
                     "DoItYourselfMagazineInHome",
                     "FinancialMagazineInHome")
magazine[, toclean_magazine] <- lapply(magazine[, toclean_magazine],clean_numeric)
unique(magazine$FamilyMagazineInHome)
# InterestinCurrentAffairsPoliticsInHousehold
unique(magazine$InterestinCurrentAffairsPoliticsInHousehold)
magazine$InterestinCurrentAffairsPoliticsInHousehold <- to_logical(magazine$InterestinCurrentAffairsPoliticsInHousehold)
# Transforming blank values to nulls
magazine[magazine == ""] <- NA
round(colSums(is.na(magazine))/nrow(magazine)*100,2) # percentage of nulls
# Dropping irrelevant columns
colnames(magazine) # to get column names
columns_magazine = c("tmpID",
                     "FamilyMagazineInHome",
                     "FemaleOrientedMagazineInHome",
                     "ReligiousMagazineInHome",
                     "GardeningMagazineInHome",
                     "CulinaryInterestMagazineInHome",
                     "HealthFitnessMagazineInHome",
                     "DoItYourselfMagazineInHome",
                     "FinancialMagazineInHome",
                     "InterestinCurrentAffairsPoliticsInHousehold")
final_magazine <- subset(magazine, select = columns_magazine)
# Structure and summary of final cleaned df
round(colSums(is.na(final_magazine))/nrow(final_magazine)*100,2) # percentage of nulls
str(final_magazine)
summary(final_magazine)

# Cleaning - Political Leanings
political <- r_political
# Structure
str(political)
# PartiesDescription
unique(political$PartiesDescription)
political$PartiesDescription <- factor(political$PartiesDescription)
# ReligionsDescription
unique(political$ReligionsDescription)
political$ReligionsDescription <- ifelse(political$ReligionsDescription == "",NA,political$ReligionsDescription)
political$ReligionsDescription <- factor(political$ReligionsDescription)
## cleaning LikelyUnionMember
#unique(political$LikelyUnionMember)
#political$LikelyUnionMember <- ifelse(political$LikelyUnionMember == "Yes",TRUE,FALSE)
## cleaning GunOwner
#unique(political$GunOwner)
#political$GunOwner <- ifelse(political$GunOwner == "Yes",TRUE,FALSE)
## cleaning Veteran
#unique(political$Veteran)
#political$Veteran <- ifelse(political$Veteran == "Yes",TRUE,FALSE)
## cleaning support columns
#unique(political$supportsAffordableCareAct)
#political$supportsAffordableCareAct <- ifelse(political$supportsAffordableCareAct == "Support",TRUE,FALSE)
#unique(political$supportsGayMarriage)
#political$supportsGayMarriage <- ifelse(political$supportsGayMarriage == "Support",TRUE,FALSE)
#unique(political$supportsGunControl)
#political$supportsGunControl <- ifelse(political$supportsGunControl == "Support",TRUE,FALSE)
#unique(political$supportsTaxesRaise)
#political$supportsTaxesRaise <- ifelse(political$supportsTaxesRaise == "Support",TRUE,FALSE)
## cleaning OverallSocialViews
#unique(political$overallsocialviews)
#political$overallsocialviews <- factor(political$overallsocialviews)
## cleaning donations
#unique(political$DonatestoConservativeCauses)
#political$DonatestoConservativeCauses <- ifelse(political$DonatestoConservativeCauses == "Yes",TRUE,FALSE)
#unique(political$DonatestoLiberalCauses)
#political$DonatestoLiberalCauses <- ifelse(political$DonatestoLiberalCauses == "Yes",TRUE,FALSE)
# Transforming blank values to nulls
political[political == ""] <- NA
round(colSums(is.na(political))/nrow(political)*100,2) # percentage of nulls
# Dropping irrelevant columns
colnames(political) # to get column names
columns_political = c("tmpID",
                     "PartiesDescription",
                     "ReligionsDescription"
                     #"LikelyUnionMember", - dropped, 90% blanks/nulls
                     #"GunOwner", - dropped, 90% blanks/nulls
                     #"Veteran", - dropped, 90% blanks/nulls
                     #"supportsAffordableCareAct", - dropped, 90% blanks/nulls
                     #"supportsGayMarriage", - dropped, 90% blanks/nulls
                     #"supportsGunControl", - dropped, 90% blanks/nulls
                     #"supportsTaxesRaise", - dropped, 90% blanks/nulls
                     #"overallsocialviews", - dropped, too many null values
                     #"DonatestoConservativeCauses", - dropped, 90% blanks/nulls
                     #"DonatestoLiberalCauses"- dropped, 90% blanks/nulls
                     ) 
final_political <- subset(political, select = columns_political)
# Structure and summary of final cleaned df
round(colSums(is.na(final_political))/nrow(final_political)*100,2) # percentage of nulls
str(final_political)
summary(final_political)

# Cleaning - Donations
donations <- r_donations
# Structure
str(donations)
# Donation columns with counts of contributors
colnames(donations) # to get column names
toclean_donations1 <- c("ReligiousContributorInHome",
                        "PoliticalContributerInHome")
donations[, toclean_donations1] <- lapply(donations[, toclean_donations1],clean_numeric)
unique(donations$ReligiousContributorInHome)
# Transforming logical donation columns
colnames(donations) # to get column names
toclean_donations2 <- c("DonatesEnvironmentCauseInHome",
                        "DonatesToCharityInHome",
                        "DonatestoAnimalWelfare",
                        "DonatestoArtsandCulture",
                        "DonatestoChildrensCauses",
                        "DonatestoHealthcare",
                        "DonatestoInternationalAidCauses",
                        "DonatestoVeteransCauses",
                        "DonatestoHealthcare1",
                        "DonatestoInternationalAidCauses1",
                        "DonatestoWildlifePreservation",
                        "DonatestoLocalCommunity")
donations[, toclean_donations2] <- lapply(donations[,toclean_donations2],to_logical)
unique(donations$DonatesEnvironmentCauseInHome)
# Adding column for general donation
donations$AnyDonation <- any_donation(donations)
unique(donations$AnyDonation)
# Transforming blank values to nulls
donations[donations == ""] <- NA
round(colSums(is.na(donations))/nrow(donations)*100,2) # percentage of nulls
# Dropping irrelevant columns
colnames(donations) # to get column names
columns_donations = c("tmpID",
                      "ReligiousContributorInHome",
                      "PoliticalContributerInHome",
                      #"DonatesEnvironmentCauseInHome", - dropped, will use instead combined AnyDonation to  avoid nulls
                      #"DonatesToCharityInHome", - dropped, will use instead combined AnyDonation to avoid nulls
                      #"DonatestoAnimalWelfare", - dropped, will use instead combined AnyDonation to avoid nulls
                      #"DonatestoArtsandCulture", - dropped, will use instead combined AnyDonation to  avoid nulls
                      #"DonatestoChildrensCauses", - dropped, will use instead combined AnyDonation to avoid nulls
                      #"DonatestoHealthcare", - dropped, will use instead combined AnyDonation to  avoid nulls
                      #"DonatestoInternationalAidCauses", - dropped, will use instead combined AnyDonation to  avoid nulls
                      #"DonatestoVeteransCauses", - dropped, will use instead combined AnyDonation to  avoid nulls
                      #"DonatestoHealthcare1", - dropped, will use instead combined AnyDonation to avoid nulls
                      #"DonatestoInternationalAidCauses1", - dropped, will use instead combined AnyDonation to avoid nulls
                      #"DonatestoWildlifePreservation", - dropped, will use instead combined AnyDonation to  avoid nulls
                      #"DonatestoLocalCommunity", - dropped, will use instead combined AnyDonation to  avoid nulls
                      "AnyDonation"
                      )
final_donations <- subset(donations, select = columns_donations)
# Structure and summary of final cleaned df
round(colSums(is.na(final_donations))/nrow(final_donations)*100,2) # percentage of nulls
str(final_donations)
summary(final_donations)

# IMPUTING NULLS

# External
# Merged external data
merged_external <- left_join(final_consumer, final_political, by = "tmpID") %>% 
                   left_join(final_magazine, by = "tmpID") %>% 
                   left_join(final_donations, by = "tmpID")
round(colSums(is.na(merged_external))/nrow(merged_external)*100,2)
head(merged_external,5)
str(merged_external)

# Imputing nulls from external data using mice and PMM
# Using PMM (Predictive Mean Matching) based on the assumption that missing data are MAR or CAR
# Run multiple imputations with PMM
merged_external_forimp <- mice(merged_external, m = 5, method = "pmm", seed = 123)
# Complete the imputations
merged_external_impfin <- complete(merged_external_forimp)
# Show the final merged external df
str(merged_external_impfin)
summary(merged_external_impfin)
round(colSums(is.na(merged_external_impfin))/nrow(merged_external_impfin)*100,2) # NO NULLS

# Internal

internal <- final_membership
# Imputing nulls from internal data using mice and PMM
# Using PMM (Predictive Mean Matching) based on the assumption that missing data are MAR or CAR
# Run multiple imputations with PMM
internal_forimp <- mice(internal, m = 5, method = "pmm", seed = 123)
# Complete the imputations
internal_impfin <- complete(internal_forimp)
# Show the final internal df
str(internal_impfin)
summary(internal_impfin)
round(colSums(is.na(internal_impfin))/nrow(internal_impfin)*100,2) # NO NULLS

# JOIN ALL DATABASES (NO NULLS)
# Joining based on the membership database

mergedall <- left_join(internal_impfin,merged_external_impfin,by="tmpID")
str(mergedall)
summary(mergedall)

# ANALYSIS

# GENDER COUNT
gender_counts <- table(mergedall$Gender)
gender_counts
# F = 5614, 4718

# GENDER PER ETHNICITY
gender_ethnic <- mergedall[,c("Gender","BroadEthnicGroupings")]
# limit ethnicity to top 10
gender_ethnic_sum <- gender_ethnic %>% 
  group_by(BroadEthnicGroupings) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  filter(n >= quantile(n, 0.2)) %>% 
  select(-n)
# prep df for plotting
gender_ethnic_sum <- table(gender_ethnic_sum)
gender_ethnic_sum <- data.frame(gender_ethnic_sum)
gender_ethnic_sum <- gender_ethnic_sum[gender_ethnic_sum$Freq != 0, ]
gender_ethnic_sum$percent <- percent(round(gender_ethnic_sum$Freq/10322,2))
# create stacked bar plot
ggplot(gender_ethnic_sum, aes(x = Gender, y = Freq ,fill = BroadEthnicGroupings)) + 
  geom_bar(stat = "identity") + 
  geom_text(aes(label = percent), position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Pastel2") +
  labs(x = "", 
       y = "",
       fill = "Ethnic Group") +
  theme_minimal()

# AGE DISTRIBUTION
# Create a vector of ages
age <- mergedall$Age
# bin ages
age_bin <- cut(age, breaks = c(20,35,50,65,80,95,110))
age_count <- table(age_bin)
age_count <- data.frame(age_count)
# create a bar plot to show the distribution
ggplot(age_count, aes(x = age_bin, y = Freq, fill = age_bin)) +
  geom_bar(stat = "identity",fill = "darkseagreen3") +
  #scale_fill_manual(values = "darksalmon")
  geom_text(aes(label = Freq), position = position_stack(vjust = 0.5)) +
  guides(fill = FALSE) +
  labs(x = "Age Group", y = "") +
  theme_minimal()
# Customer base is largely 50+

# No bins - Line graph
age_dist <- data.frame(age)
# Plot
ggplot(age_dist, aes(x = age)) +
  geom_density(color = "darkslategray", fill = "darkseagreen3", alpha = .5) +
  labs(x = "Age", y = "Distribution") +
  theme_minimal()

# NETWORTH DISTRIBUTION
networth <- mergedall$NetWorth
networth_count <- table(networth)
networth_count <- data.frame(networth_count)
networth_count <- networth_count[networth_count$Freq != 0,]
networth_count$order <- c(1, 3, 6, 4, 7, 8, 2, 5)
networth_count$order <- factor(networth_count$order)
networth_count <- networth_count[order(networth_count$order), ]
networth_count
# create a bar plot to show the distribution
ggplot(networth_count, aes(x = order, y = Freq, fill = networth)) +
  geom_bar(stat = "identity",fill="darkseagreen3") +
  geom_text(aes(label = Freq), position = position_stack(vjust = 0.5)) +
  guides(fill = FALSE) +
  labs(x = "", y = "") +
  scale_x_discrete(labels = networth_count$networth) +
  theme_minimal()

# CUSTOMER LOCATION

# per state
cus_locations <- mergedall$state
cus_locations <- table(cus_locations)
cus_locations <- data.frame(cus_locations)
head(cus_locations)
top_states <- cus_locations %>%
  group_by(cus_locations) %>%
  summarise(total_customers = sum(Freq)) %>%
  top_n(10, total_customers) %>%
  arrange(desc(total_customers))
top_states
top_states$percent <- percent(round(top_states$total_customers/sum(top_states$total_customers),3))
# plot
ggplot(top_states, aes(x = reorder(cus_locations, total_customers), y = total_customers)) +
  geom_bar(stat = "identity", fill = "darkseagreen3") +
  geom_text(aes(label = percent),position = position_stack(vjust = 0.5)) +
  labs(title = "", x = "", y = "Number of Customers") +
  theme_minimal()

# per region
cus_region <- mergedall$state
cus_region <- table(cus_region)
cus_region <- data.frame(cus_region)
cus_region$region <- state.region[match(cus_region$cus_region,state.name)]
cus_region
top_regions <- cus_region %>%
  group_by(region) %>%
  summarise(total_customers = sum(Freq)) %>%
  arrange(desc(total_customers))
top_regions <- na.omit(top_regions)
top_regions$percent <- percent(round(top_regions$total_customers/sum(top_regions$total_customers),3))
# plot
ggplot(top_regions, aes(x = reorder(region, total_customers), y = total_customers)) +
  geom_bar(stat = "identity", fill = "darkseagreen3") +
  geom_text(aes(label = percent),position = position_stack(vjust = 0.5)) +
  labs(title = "", x = "", y = "Number of Customers") +
  theme_minimal()

# RELIGION
cus_religion <- mergedall$ReligionsDescription
cus_religion <- table(cus_religion)
cus_religion <- data.frame(cus_religion)
head(cus_religion)
top_religion <- cus_religion %>%
  group_by(cus_religion) %>%
  summarise(total_customers = sum(Freq)) %>%
  top_n(5, total_customers) %>%
  arrange(desc(total_customers))
top_religion
top_religion$percent <- percent(round(top_religion$total_customers/sum(top_religion$total_customers),3))
#plot
ggplot(top_religion, aes(x = reorder(cus_religion, total_customers), y = total_customers)) +
  geom_bar(stat = "identity", fill = "darkseagreen3") +
  geom_text(aes(label = percent),position = position_stack(vjust = 0.5)) +
  labs(x = "", y = "Number of Customers") +
  theme_minimal()

# CUSTOMER LOCATION with religion
state_religion <- mergedall[,c("state","ReligionsDescription")]
state_religion_sum <- table(state_religion)
state_religion_sum <- data.frame(state_religion_sum)
state_religion_sum <- state_religion_sum[state_religion_sum$Freq !=0,]
head(state_religion_sum)
# Select the top 10 states and top 10 religions
top_states <- state_religion_sum %>%
  group_by(state) %>%
  summarize(total = sum(Freq)) %>%
  arrange(desc(total)) %>%
  head(10) %>%
  pull(state)
top_religions <- state_religion_sum %>%
  group_by(ReligionsDescription) %>%
  summarize(total = sum(Freq)) %>%
  arrange(desc(total)) %>%
  head(3) %>%
  pull(ReligionsDescription)
# Filter the data and create a new column that groups less frequent religions as "Other"
state_religion_sum <- state_religion_sum %>%
  filter(state %in% top_states & ReligionsDescription %in% top_religions) %>%
  mutate(ReligionsDescription = fct_lump(ReligionsDescription, n = 10))
state_religion_sum_fin <- state_religion_sum %>%
  group_by(state) %>%
  summarize(total = sum(Freq)) %>%
  inner_join(state_religion_sum, by = "state")
state_religion_sum_fin$percent <- percent(round(state_religion_sum_fin$Freq/state_religion_sum_fin$total,2))
# Plot the top 10 religions per state and top 10 states
ggplot(state_religion_sum_fin, aes(x = reorder(state, total), y = Freq ,fill = ReligionsDescription)) + 
  geom_bar(stat = "identity") + 
  geom_text(aes(label = percent), position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Pastel2") +
  labs(x = "", 
       y = "",
       fill = "Religion") +
  theme_minimal()

# CUSTOMER LOCATION with political leanings

unique(mergedall$PartiesDescription)
politic <- mergedall$PartiesDescription
politic_sum <- table(politic)
politic_sum <- data.frame(politic_sum)

unique(mergedall$PartiesDescription)
state_politic <- mergedall[,c("state","PartiesDescription")]
state_politic_sum <- table(state_politic)
state_politic_sum <- data.frame(state_politic_sum)
state_politic_sum <- state_politic_sum[state_politic_sum$Freq !=0,]
head(state_politic_sum)
# Select the top 10 states and top 10 religions
top_statesp <- state_politic_sum %>%
  group_by(state) %>%
  summarize(total = sum(Freq)) %>%
  arrange(desc(total)) %>%
  head(10) %>%
  pull(state)
top_parties <- state_politic_sum %>%
  group_by(PartiesDescription) %>%
  summarize(total = sum(Freq)) %>%
  arrange(desc(total)) %>%
  head(3) %>%
  pull(PartiesDescription)
state_politic_sum <- state_politic_sum %>%
  filter(state %in% top_statesp & PartiesDescription %in% top_parties) %>%
  mutate(PartiesDescription = fct_lump(PartiesDescription, n = 10))
state_politic_sum_fin <- state_politic_sum %>%
  group_by(state) %>%
  summarize(total = sum(Freq)) %>%
  inner_join(state_politic_sum, by = "state")
state_politic_sum_fin$percent <- percent(round(state_politic_sum_fin$Freq/state_politic_sum_fin$total,2))
# Plot
ggplot(state_politic_sum_fin, aes(x = reorder(state, total), y = Freq ,fill = PartiesDescription)) + 
  geom_bar(stat = "identity") + 
  geom_text(aes(label = percent), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values=c("lightblue2","lightgreen","pink")) +
  labs(x = "", 
       y = "",
       fill = "Party") +
  theme_minimal()

# NET WORTH with politics
unique(mergedall$NetWorth)
networth_politic <- mergedall[,c("NetWorth","PartiesDescription")]
networth_politic_sum <- table(networth_politic)
networth_politic_sum <- data.frame(networth_politic_sum)
networth_politic_sum <- networth_politic_sum[networth_politic_sum$Freq !=0,]
colnames(networth_politic_sum)[1] <- "networth"
networth_politic_sum <- left_join(networth_politic_sum,networth_count,by="networth")
networth_politic_sum$percent <- percent(round(networth_politic_sum$Freq.x/networth_politic_sum$Freq.y,2))
networth_politic_sum <- networth_politic_sum[networth_politic_sum$PartiesDescription != "Green",]
head(networth_politic_sum)
# plot
ggplot(networth_politic_sum, aes(x = order, y = Freq.x, fill = PartiesDescription)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=c("lightblue2","lightgreen","pink")) +
  geom_text(aes(label = percent), position = position_stack(vjust = 0.5)) +
  labs(x = "", y = "", fill="Party") +
  scale_x_discrete(labels = networth_count$networth) +
  theme_minimal()

# NET WORTH with religion
unique(mergedall$ReligionsDescription)
networth_religion <- mergedall[,c("NetWorth","ReligionsDescription")]
networth_religion_sum <- table(networth_religion)
networth_religion_sum <- data.frame(networth_religion_sum)
networth_religion_sum <- networth_religion_sum[networth_religion_sum$Freq !=0,]
colnames(networth_religion_sum)[1] <- "networth"
networth_religion_sum <- left_join(networth_religion_sum,networth_count,by="networth")
networth_religion_sum$percent <- percent(round(networth_religion_sum$Freq.x/networth_religion_sum$Freq.y,2))
head(networth_religion_sum)
# top 3 religion
top_religions <- networth_religion_sum %>%
  group_by(ReligionsDescription) %>%
  summarize(total = sum(Freq.x)) %>%
  arrange(desc(total)) %>%
  head(3) %>%
  pull(ReligionsDescription)
networth_religion_sum <- networth_religion_sum %>%
  filter(ReligionsDescription %in% top_religions) %>%
  mutate(ReligionsDescription = fct_lump(ReligionsDescription, n = 10))
# plot
ggplot(networth_religion_sum, aes(x = order, y = Freq.x, fill = ReligionsDescription)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette="Pastel2") +
  #geom_text(aes(label = percent), position = position_stack(vjust = 0.5)) +
  labs(title = "", x = "", y = "",fill="Religion") +
  scale_x_discrete(labels = networth_count$networth) +
  theme_minimal()

# ANALYZING BEHAVIORAL ATTRIBUTES IN TARGET MARKET

# setting database to target market
colnames(mergedall)
mergedtarget <- mergedall %>%
  filter(Age > 35) %>%
  filter(Age <= 95) %>%
  filter(ReligionsDescription == "Jewish" | ReligionsDescription == "Catholic" | ReligionsDescription == "Protestant") %>%
  filter(NetWorth == "$25000-49999" | NetWorth == "$50000-99999" | NetWorth == "$100000-249999" | NetWorth == "$250000-499999")
head(mergedtarget)
str(mergedtarget)

# STORE VISIT PER NETWORTH
networth_visit <- mergedtarget[,c("NetWorth","storeVisitFrequency")]
networth_visit_sum <- aggregate(networth_visit$storeVisitFrequency,list(networth_visit$NetWorth),sum)
networth_visit_mean <- aggregate(networth_visit$storeVisitFrequency,list(networth_visit$NetWorth),mean)

# STORE VISIT PER AGE
age_visit <- mergedtarget[,c("Age","storeVisitFrequency")]
age_visit_sum <- aggregate(age_visit$storeVisitFrequency,list(age_visit$Age),sum)
age_visit_mean <- aggregate(age_visit$storeVisitFrequency,list(age_visit$Age),mean)
age_visit_max <- aggregate(age_visit$storeVisitFrequency,list(age_visit$Age),max)

# SPEND PER NETWORTH
spend_networth <- mergedtarget[,c("NetWorth","y_householdSpend")]
spend_networth$order <- ifelse(spend_networth$NetWorth == "$25000-49999", "1-$25000-49999",
                               ifelse(spend_networth$NetWorth == "$50000-99999", "2-$50000-99999",
                                      ifelse(spend_networth$NetWorth == "$100000-249999","3-$100000-249999",
                                             ifelse(spend_networth$NetWorth == "$250000-499999","4-$250000-499999","0"))))

spend_networth_sum <- aggregate(spend_networth$y_householdSpend,list(spend_networth$networth),sum)
spend_networth_mean <- aggregate(spend_networth$y_householdSpend,list(spend_networth$NetWorth),mean)
spend_networth_max <- aggregate(spend_networth$y_householdSpend,list(spend_networth$NetWorth),max)
spend_networth_all <- left_join(spend_networth_sum,spend_networth_mean,by="Group.1") %>%
  left_join(spend_networth_max, by = "Group.1")
# Make the boxplot
ggplot(spend_networth, aes(x = order, y = y_householdSpend)) +
  geom_boxplot(fill="darkseagreen3") +
  labs(x = "", y = "") +
  #scale_x_discrete(labels = spend_networth$networth) +
  theme_minimal()

# SPEND PER AGE
age_spend <- mergedtarget[,c("Age","y_householdSpend")]
age_spend_sum <- aggregate(age_spend$y_householdSpend,list(age_spend$Age),sum)
age_spend_mean <- aggregate(age_spend$y_householdSpend,list(age_spend$Age),mean)
age_spend_max <- aggregate(age_spend$y_householdSpend,list(age_spend$Age),max)
age_spend_all <- left_join(age_spend_sum,age_spend_mean,by="Group.1") %>%
  left_join(age_spend_max, by = "Group.1")
#plot = max
ggplot(age_spend_all, aes(x = Group.1,y=x)) +
  geom_line(color = "black", alpha = .5) +
  geom_smooth(color = "darkslategray") +
  labs(title = "", x = "", y = "Max Spending") +
  theme_minimal()
#plot = mean
#plot
ggplot(age_spend_all, aes(x = Group.1,y=x.y)) +
  geom_line(color = "black", alpha = .5) +
  geom_smooth(color = "darkslategray") +
  labs(x = "", y = "Average Spending") +
  theme_minimal()

# HOME OFFICE
unique(mergedtarget$HomeOffice)
homeoff <- mergedtarget$HomeOffice
homeoff_sum <- table(homeoff)
homeoff_sum <- data.frame(homeoff_sum)
homeoff_sum$percent <- percent(round(homeoff_sum$Freq/sum(homeoff_sum$Freq),2))
ggplot(homeoff_sum, aes(x = homeoff, y = Freq)) +
  geom_bar(stat = "identity",fill="darkseagreen3") +
  geom_text(aes(label = percent), position = position_stack(vjust = 0.5)) +
  labs(x = "", y = "") +
  theme_minimal()

# COMPUTER OWNER
unique(mergedtarget$ComputerOwnerInHome)
compown <- mergedtarget$ComputerOwnerInHome
compown_sum <- table(compown)
compown_sum <- data.frame(compown_sum)
compown_sum$percent <- percent(round(compown_sum$Freq/sum(compown_sum$Freq),2))
ggplot(compown_sum, aes(x = compown, y = Freq)) +
  geom_bar(stat = "identity", fill="darkseagreen3") +
  geom_text(aes(label = percent), position = position_stack(vjust = 0.5)) +
  labs(x = "", y = "") +
  theme_minimal()

# ANIMAL OWNER
unique(mergedtarget$AnimalOwner)
animal <- mergedtarget$AnimalOwner
animal_sum <- table(animal)
animal_sum <- data.frame(animal_sum)
animal_sum$percent <- percent(round(animal_sum$Freq/sum(animal_sum$Freq),2))
ggplot(animal_sum, aes(x = animal, y = Freq)) +
  geom_bar(stat = "identity",fill="darkseagreen3") +
  geom_text(aes(label = percent), position = position_stack(vjust = 0.5)) +
  labs(x = "", y = "") +
  theme_minimal()

# CHILDREN
unique(mergedtarget$PresenceOfChildrenCode_C)
children <- mergedtarget$PresenceOfChildrenCode_C
children_sum <- table(children)
children_sum <- data.frame(children_sum)
children_sum <- children_sum[children_sum$children != "",]
children_sum$percent <- percent(round(children_sum$Freq/sum(children_sum$Freq),2))
ggplot(children_sum, aes(x = children, y = Freq)) +
  geom_bar(stat = "identity",fill="darkseagreen3") +
  geom_text(aes(label = percent), position = position_stack(vjust = 0.5)) +
  labs(title = "", x = "", y = "") +
  theme_minimal()

# OCCUPATION
unique(mergedtarget$OccupationIndustry)
occupation <- mergedtarget$OccupationIndustry
occupation_sum <- table(occupation)
occupation_sum <- data.frame(occupation_sum)
occupation_sum$percent <- percent(round(occupation_sum$Freq/sum(occupation_sum$Freq),2))
ggplot(occupation_sum, aes(x = Freq, y = reorder(occupation,Freq))) +
  geom_bar(stat = "identity", fill="darkseagreen3") +
  geom_text(aes(x = Freq, y = occupation, label = Freq), hjust = -0.1) +
  labs(x = "", y = "") +
  theme_minimal()

# EDUCATION
unique(mergedtarget$Education)
education <- mergedtarget$Education
education_sum <- table(education)
education_sum <- data.frame(education_sum)
ggplot(education_sum, aes(x = Freq, y = reorder(education,Freq))) +
  geom_bar(stat = "identity", fill="darkseagreen3") +
  geom_text(aes(x = Freq, y = education, label = Freq), hjust = -0.1) +
  labs(title = "", x = "", y = "") +
  theme_minimal()

# MAGAZINE
unique(mergedtarget$FamilyMagazineInHome)
magazine_purch <- mergedtarget[,toclean_magazine]
magazine_sum <- colSums(magazine_purch)
magazine_sum <- data.frame(magazine_sum)
magazine_sum <- magazine_sum %>% rownames_to_column("Type")
magazine_sum$Type <- gsub("MagazineInHome","",magazine_sum$Type)
head(magazine_sum)
ggplot(magazine_sum,aes(x=magazine_sum,y=reorder(Type,magazine_sum))) +
  geom_bar(stat = "identity", fill="darkseagreen3") +
  geom_text(aes(x = magazine_sum, y = Type, label = magazine_sum), hjust = -0.1) +
  labs(x = "", y = "") +
  theme_minimal()

# DONATION ACTIVITY
unique(mergedtarget$AnyDonation)
donation_act <- mergedtarget$AnyDonation
donation_sum <- table(donation_act)
donation_sum <- data.frame(donation_sum)
donation_sum$percent <- percent(round(donation_sum$Freq/sum(donation_sum$Freq),2))
ggplot(donation_sum,aes(x=donation_act,y=Freq)) +
  geom_bar(stat = "identity", fill="darkseagreen3") +
  geom_text(aes(label = percent), position = position_stack(vjust = 0.5)) +
  labs(x = "", y = "") +
  theme_minimal()
# Causes of donors
donation_per <- mergedtarget[mergedtarget$AnyDonation==TRUE,c("tmpID","AnyDonation")]
donation_per <- left_join(donation_per,donations,by="tmpID")
donation_per <- subset(donation_per,select = -c(tmpID,
                                                AnyDonation.x,
                                                ReligiousContributorInHome,
                                                PoliticalContributerInHome,
                                                AnyDonation.y,
                                                DonatestoHealthcare1,
                                                DonatestoInternationalAidCauses1))
donation_per[is.na(donation_per)] <- 0
donation_per <- ifelse(donation_per==TRUE,1,0)
donation_per_sum <- colSums(donation_per)
donation_per_sum <- data.frame(donation_per_sum)
donation_per_sum <- donation_per_sum %>% rownames_to_column("Causes")
donation_per_sum$Causes <- gsub("Donates","",donation_per_sum$Causes)
donation_per_sum$Causes <- gsub("to","",donation_per_sum$Causes)
donation_per_sum$Causes <- gsub("InHome","",donation_per_sum$Causes)
donation_per_sum$Causes <- gsub("To","",donation_per_sum$Causes)
donation_per_sum$Causes <- gsub("Causes","",donation_per_sum$Causes)
donation_per_sum$Causes <- gsub("Cause","",donation_per_sum$Causes)
str(donation_per_sum)
ggplot(donation_per_sum,aes(x=donation_per_sum,y=reorder(Causes,donation_per_sum))) +
  geom_bar(stat = "identity", fill="darkseagreen3") +
  geom_text(aes(x = donation_per_sum, y = Causes, label = donation_per_sum), hjust = -0.1) +
  labs(x = "", y = "") +
  theme_minimal()

# End