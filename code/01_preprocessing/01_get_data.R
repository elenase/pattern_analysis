
#source("code/00_preprocessing/00_packages.R")
#############################################################################################
cat("In this script, data (except spatial data) needed for the project is load: \n
    1) Data integration of proGres sociodempgraphic data and specific needs and risk data \n
    2) Load of UNHCR categories according to UNHCR registration handbook for data cleaning \n
    3) Load of p-code / iso3 code conversion table and subset to existent countries of asylum\n
    4) Base table for grouping categoric raw data into larger categories \n
    5) Saving created data files to 'data/data_processed'")


#############################################################################################
cat("1) Data integration: Inner join of specific needs data and case level data based on CaseNo\n")

original.spneed <- read.csv("data/source_data/progresspecificneedcase.csv", sep = ";")
original.prcase <- read.csv("data/source_data/progrescase.csv")
data.orig <- merge(x=original.prcase, y=original.spneed, by.x = "CaseNo", by.y = "CaseNo")

## remove columns not needed in this analysis 
data.orig[c("X.x", "X.y","cool3", "cool3id", "cool4id", "coal3", "coal3id", "coal4id", "occupation")] <- NULL

#############################################################################################
cat("2) Data Load: UNHCR Categories based of UNHCR registration handbook\n")

unhcr.categories <- read.csv("data/reference_data/unhcr_handbook_categories.csv", sep = ";")


#############################################################################################
cat("3) UNHCR p-codes and iso3 conversion table for countries existent in data\n")

country.codes <- read.csv("data/reference_data/countrycodes.csv")
colnames(country.codes) <- c("P_Code","iso3", "name")

cat("Creation of country codes conversion table for countries of asylum existent in data\n")
all.coal <- data.frame(unique(data.orig$CountryAsylum)) ## all unique country of asylum
colnames(all.coal) <- "P_Code"
country.asylum <- merge(all.coal, country.codes, all.x=TRUE) ## merge with iso3 country codes
country.asylum$iso3 <- as.character(country.asylum$iso3)
country.asylum$iso3[which(country.asylum[,1] == "GCC")] <- "GCC" ## Saudi Arabia and Arabian Emirates
country.asylum$name <- as.character(country.asylum$name)
country.asylum$name[which(country.asylum[,1] == "GCC")] <- "Saudi Arabia and Emirates"
country.asylum <- country.asylum[!(country.asylum$P_Code == "ISR"),]
rm(all.coal)


#############################################################################################
cat("4) Conversion table for grouping and categorizing categorical data\n")

conversion.catdata <- read.csv("data/reference_data/class_conversion_categoric.csv", sep = ";")
conversion.catdata$raw <- as.character(conversion.catdata$raw)
conversion.catdata[conversion.catdata$grouping == "Military", "raw"] <- "0001"   ## sometimes zero in the beginning is removed in Excel
conversion.catdata[conversion.catdata$raw == "110", "raw"] <- "0110"


#############################################################################################
cat("5) Saving resulting script\n")

write.csv(country.asylum, "data/data_processed/country_asylum.csv")
write.csv(data.orig, file = "data/data_processed/data_orig.csv")




