#############################################################################################
cat("Data Processing: From raw data to numerical information\n 
    1) Grouping of categorical data\n
    2) Extraction of new information\n
    3) Aggregation of temporal data to bins\n
    4) Creation of dummy variables of newly created categories\n
    5) Remove categorical raw data")
## result of this scrips: ("data/data_processed/data_maps.csv")
#############################################################################################
cat("Necessary Data: Activate data load if variables are not existent\n")
data <- data.cleaned

#############################################################################################
cat("1) GrouPING CATECORICAL DATA BASED ON GROUPING REFERENCE TABLE\n")
#############################################################################################
cat("Aggregate month of arrival to seasons\n")


data.frame(prop.table(table(data$Montharrival, useNA = "ifany")))
data$season <- data$Montharrival %l%  conversion.catdata[,c(2,3)]
data.frame(prop.table(table(data$season, useNA = "ifany")))
#############################################################################################
cat("Aggregate country of origin to regional groups\n")

data.frame(prop.table(table(data$CountryOrigin, useNA = "ifany")))
conversion.sub <- conversion.catdata[(conversion.catdata$variable == "CountryOrigincat"), ]
data$CountryOrigincat <- data$CountryOrigin %l%  conversion.sub[,c(2,3)]
data.frame(prop.table(table(data$CountryOrigincat, useNA = "ifany")))
#############################################################################################
cat("Aggregate country of birth of principal applicant to regional groups\n")

prop.table(table(data$dem_birth_country, useNA = "ifany"))
conversion.sub <- conversion.catdata[(conversion.catdata$variable == "dem_birth_countrycat"), ]
data$dem_birth_countrycat <- data$dem_birth_country %l%  conversion.sub[,c(2,3)]
prop.table(table(data$dem_birth_countrycat, useNA = "ifany"))

#############################################################################################
cat("Aggregate education level to education stages\n")

prop.table(table(data$edu_highest, useNA = "ifany"))
data$edu_highestcat <- data$edu_highest %l%  conversion.catdata[,c(2,3)]
prop.table(table(data$edu_highestcat, useNA = "ifany"))

#############################################################################################
cat("Aggregate occupation codes to main occupations\n")

prop.table(table(data$occupationcode, useNA = "ifany"))
conversion.sub <- conversion.catdata[(conversion.catdata$variable == "occupationcat"), ]
data$occupationcat <- data$occupationcode %l%  conversion.sub[,c(2,3)]
prop.table(table(data$occupationcat, useNA = "ifany"))

#############################################################################################
cat("Aggregate ethnicity to main ethnicities\n")

prop.table(table(data$dem_ethn, useNA = "ifany"))
data$dem_ethncat <- data$dem_ethn %l%  conversion.catdata[,c(2,3)]
prop.table(table(data$dem_ethncat, useNA = "ifany"))  

#############################################################################################
cat("Aggregate religions to main religions\n")

prop.table(table(data$dem_religion, useNA = "ifany"))
conversion.sub <- conversion.catdata[(conversion.catdata$variable == "dem_religioncat"), ]
data$dem_religioncat <- data$dem_religion %l%  conversion.sub[,c(2,3)]
prop.table(table(data$dem_religioncat, useNA = "ifany"))

rm(conversion.sub)






#############################################################################################
cat("2) CREATION OF NEW INFORMATION BASED ON EXISTING COLUMNS\n")
#############################################################################################
cat("Calculate span between Year of Arrival and RefugeeStatusDate\n")

data$status_span <- data$RefugeeStatusDate-data$YearArrival
#data <- data[data$status_span >= 0, ] # removes negative values (RefugeeStatusDate should be after Year of Arrival)


#############################################################################################
cat("Internal Case Structure: Calculate percentages of different age classes and dependencies by case size\n")

data$dependency <-  round((data$Child_0_14+data$Eldern_65)/data$Num_Inds*100)
data$Child_0_14_prop <-  round(data$Child_0_14/data$Num_Inds*100) ## child dependency
data$Youth_15_17_prop <-  round(data$Youth_15_17/data$Num_Inds*100) ## youth dependency
data$Work_18_64_prop <-  round(data$Work_18_64/data$Num_Inds*100) 
data$Eldern_65_prop <-  round(data$Eldern_65/data$Num_Inds*100) ## elderly dependency

#############################################################################################
cat("Recode gender information\n")

data$gender <- data$Male+data$Female
data$gender[data$Male==1 & data$Female==1] <- 3  ## mixed gender case
data$gender[data$Male==1 & data$Female==0] <- 1  ## only male case
data$gender[data$Male==0 & data$Female==1] <- 2  ## only female case
data$gender <- as.character(data$gender)
#############################################################################################
cat("Female headed cases\n")
## could be interesting because not common in arabic countries

data$female.headed <- 0
data$female.headed[data$dem_sex ==  "F"] <- 1


#############################################################################################
cat("Convert numerical factor data to integer\n")

for (i in (which( colnames(data)=="Num_Inds" )):which( colnames(data)=="STDEV_Age")) {
  data[,i] <- as.integer(data[,i])
}




#############################################################################################
cat("GROUPING TEMPORAL DATA: Aggregate year of arrival to bins\n") # bins are groups of equal size

data$YearArrivalcat <- as.numeric(data$YearArrival)
data$YearArrivalcat <- bin_data(data$YearArrival, bins=5, binType = "quantile")
conversion.years <- data.frame()
for (i in 1:length(unique(data$YearArrivalcat))) {
  conversion.years[i,1] <- "YearArrivalcat"
  conversion.years[i,2] <- "num.rawdata"
  conversion.years[i,3] <- unique(data$YearArrivalcat)[i]
  conversion.years[i,4] <- i
}
conversion.years[,4] <- as.factor(conversion.years[,4])
colnames(conversion.years) <- c("variable", "raw", "grouping", "lca")

prop.table(table(data$YearArrivalcat, useNA = "ifany"))
data$YearArrivalcat <- data$YearArrivalcat %l%  conversion.years[,c(3,4)]
prop.table(table(data$YearArrivalcat, useNA = "ifany"))


#############################################################################################
cat("Aggregate refugee status to bins\n") # bins are groups of equal size

data$RefugeeStatusDatecat <- as.numeric(data$RefugeeStatusDate)
data$RefugeeStatusDatecat <- bin_data(data$RefugeeStatusDate, bins=5, binType = "quantile")
conversion.yearsref <- data.frame()
for (i in 1:length(unique(data$RefugeeStatusDatecat))) {
  conversion.yearsref[i,1] <- "RefugeeStatusDatecat"
  conversion.yearsref[i,2] <- "num.rawdata"
  conversion.yearsref[i,3] <- unique(data$RefugeeStatusDatecat)[i]
  conversion.yearsref[i,4] <- i
}
conversion.yearsref[,4] <- as.factor(conversion.yearsref[,4])
colnames(conversion.yearsref) <- c("variable", "raw", "grouping", "lca")
prop.table(table(data$RefugeeStatusDatecat, useNA = "ifany"))
data$RefugeeStatusDatecat <- data$RefugeeStatusDatecat %l%  conversion.yearsref[,c(3,4)]
prop.table(table(data$RefugeeStatusDatecat, useNA = "ifany"))

conversion.catdata <- rbind(conversion.catdata, conversion.yearsref, conversion.years)
rm(conversion.yearsref, conversion.years)

#############################################################################################
cat("Convert year data to numbers for maps\n")

data[,"YearArrival"] <- as.numeric(data[,"YearArrival"])
data[,"RefugeeStatusDate"] <- as.numeric(data[,"RefugeeStatusDate"])


#############################################################################################
cat("Generating dummy variable for each categorical variable\n")
#############################################################################################
## numerical data needed for maps

dummies <- dummy.data.frame(data[,c("season", "YearArrivalcat", "CountryOrigincat", "dem_birth_countrycat", 
                               "dem_ethncat", "occupationcat", "edu_highestcat", "dem_religioncat", 
                               "dem_marriage", "RefStatus", "RefugeeStatusDatecat", "gender")], all=FALSE, sep = ".dummy.")

#############################################################################################
cat("Exclude raw data columns which are replaced by generated categories\n")

data$Montharrival <- NULL
data$YearArrival <- NULL
data$CountryOrigin <- NULL
data$edu_highest <- NULL
data$dem_sex <- NULL
data$dem_birth_country <- NULL
data$occupationcode <- NULL
data$dem_ethn <- NULL
data$dem_religion <- NULL
data$RefStatCategory <- NULL
data$only.female.case <- NULL
data$only.male.case <- NULL
data$Male <- NULL
data$Female <- NULL
data$RefugeeStatusDate <- NULL

data <- cbind(data, dummies)

#############################################################################################
cat("Data Processing for map creation finalized\n")

data <- droplevels(data) ##drop unused factor levels
data <- data[,!apply(data,2,function(x) all(x==0))]  ## remove columns with all zeros
data <- data[, sapply(data, function(col) length(unique(col))) > 1] ## remove columns with only one unique attribute


#############################################################################################
cat("Data Mapping: Save resulting script for further lca preprocessing: code/00_preprocessing/05_data_processing_lca\n")

data.analysis.pre <- data ## backup of data for analysis before moving on with processing
write.csv(data.analysis.pre, file = "data/data_processed/data_analysis_pre.csv")


#############################################################################################
cat("Data Mapping: Save script for all mapping actions in this project\n")

data$dem_agegroup <- NULL
data$gender <- NULL
data$YearArrivalcat <- NULL
data$RefugeeStatusDatecat <- NULL

data.maps <- data
write.csv(data, file = "data/data_processed/data_maps.csv")


rm(dummies, x, X.quali, X.quanti)


