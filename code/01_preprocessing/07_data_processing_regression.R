#############################################################################################
cat("DATA PROCESSING FOR REGRESSION\n")
#############################################################################################
cat("Necessary Data: Activate data load if variables are not existent\n")
# source("code/00_preprocessing/00_packages.R")
# source("code/00_preprocessing/03_data_cleaning.R")
# source("code/00_preprocessing/04_data_processing_maps.R")

# data.cleaned <- read.csv("data/data_processed/data_cleaned.csv")
# data.maps <- read.csv("data/data_processed/data_maps.csv")
# data.analysis <- read.csv("data/data_processed/data_analysis.csv")
data.regression <- data.cleaned
data.regression$X <- NULL
#############################################################################################
cat("Summarizing specific needs and risk columns by risk cetagory according to UNHCR\n")

## Categories of specific risks and needs according to: 
## https://cms.emergency.unhcr.org/documents/11982/43248/UNHCR%2C+Guidance+on+the+Use+of+Standardized+Specific+Needs+Codes+Annex+2+IOM+030-FOM+030-2009/cf93c655-c996-4573-8681-23b3824d058d
# Child at risk
# Unaccompanied or separated child
# Woman at risk
# Older person at risk
# Single parent or caregiver
# Disability
# Serious medical condition
# Family unity
# Specific legal and physical protection needs
# Torture
# SGBV

data.regression$child.risk <- NA
a <- grep("Child_at_risk", colnames(data.regression))
data.regression$child.risk <- rowSums(data.regression[,min(a):max(a)])
data.regression[,min(a):max(a)] <- NULL

data.regression$child.risk.separate <- NA
a <- grep("Unaccompanied_or_separated", colnames(data.regression))
data.regression$child.risk.separate <- rowSums(data.regression[,min(a):max(a)])
data.regression[,min(a):max(a)] <- NULL

data.regression$woman.risk <- NA
a <- grep("Woman_at_risk", colnames(data.regression))
data.regression$woman.risk <- rowSums(data.regression[,min(a):max(a)])
data.regression[,min(a):max(a)] <- NULL

data.regression$elderly.risk <- NA
a <- grep("Older_person", colnames(data.regression))
data.regression$elderly.risk <- rowSums(data.regression[,min(a):max(a)])
data.regression[,min(a):max(a)] <- NULL

data.regression$disability.risk <- NA
a <- grep("Disability", colnames(data.regression))
data.regression$disability.risk <- rowSums(data.regression[,min(a):max(a)])
data.regression[,min(a):max(a)] <- NULL

data.regression$famunit <- NA
a <- grep("Family_unity", colnames(data.regression))
data.regression$famunit <- rowSums(data.regression[,min(a):max(a)])
data.regression[,min(a):max(a)] <- NULL

data.regression$legalprot <- NA
a <- grep("Specific_legal", colnames(data.regression))
data.regression$legalprot <- rowSums(data.regression[,min(a):max(a)])
data.regression[,min(a):max(a)] <- NULL

data.regression$torture.risk <- NA
a <- grep("Torture", colnames(data.regression))
data.regression$torture.risk <- rowSums(data.regression[,min(a):max(a)])
data.regression[,min(a):max(a)] <- NULL

data.regression$sgbv.risk <- NA
a <- grep("SGBV", colnames(data.regression))
data.regression$sgbv.risk <- rowSums(data.regression[,min(a):max(a)])
data.regression[,min(a):max(a)] <- NULL

data.regression$singleparent <- NA
a <- grep("Single_parent", colnames(data.regression))
data.regression$singleparent <- rowSums(data.regression[,min(a):max(a)])
data.regression[,min(a):max(a)] <- NULL

data.regression$seriousmed <- NA
a <- grep("Serious_medical", colnames(data.regression))
b <- grep("Pregnant", colnames(data.regression))
data.regression$seriousmed <- rowSums(data.regression[,c(min(a):max(a),min(b):max(b))])
data.regression[,c(min(a):max(a),min(b):max(b))] <- NULL



#############################################################################################
cat("Selecting sociodemographic variables which will act as independet variables in GWR\n")

## Num_Inds
## gender 
## female.headed 
## Child_0_14
## Work_18_64
## Youth_15_17
## Eldern_65
## AVG_Age
## STDEV_Age
## YearArrival
## RefStatus
## CountryOrigincat 
## dem_religion
## status_span 
## dem_religion

## subset to relevant variables

data.regression$CountryOrigin <- data.cleaned$CountryOrigin
a <- grep("^child.risk$", colnames(data.regression))
b <- grep("^seriousmed$", colnames(data.regression))

data <- cbind(data.regression[,c("CaseNo", "CountryAsylum", "coal1id", "CountryOrigin")],
              data.regression[,c("Num_Inds", "Child_0_14", "Work_18_64", "Youth_15_17", "Eldern_65",
                                 "AVG_Age", "STDEV_Age", "YearArrival", "dem_age")],
              data.maps[,c("female.headed", "status_span", "edu_highestcat",
                           "occupationcat", "CountryOrigincat")], 
              data.regression[,c("dem_religion", "RefStatus")],
              data.regression[,a:b])


## creating dummy variables for categorical variables as numerical data needed
dummies <- dummy.data.frame(data.analysis[,c("gender", "edu_highestcat", "occupationcat", "CountryOrigincat", 
                                    "dem_religioncat", "RefStatus")], all=FALSE, sep = ".dummy.")
data <- cbind(data, dummies)



data[,c("gender","edu_highestcat","occupationcat","CountryOrigincat","dem_religion", "RefStatus")] <- NULL

data.regression <- data
write.csv(data.regression, "data/data_processed/data_regression.csv")
