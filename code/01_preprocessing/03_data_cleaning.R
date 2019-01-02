########################################################################################
cat("Data Cleaning \n")
## result of this scrips: ("data/data_processed/data_cleaned.csv")
########################################################################################
cat("Necessary Data: Activate data load if variables are not existent\n")

data <- data.orig 



########################################################################################
cat("Data Cleaning of obvious errors \n")

## fill NA's of obvious 'errors' in calculated columns (otherwise it would be removed in next step)
## if Num_Ind == 1 -> STDEV_Age == NA; replace those cells with 0
data$STDEV_Age[data$Num_Inds == 1] <- 0
data$edu_highest <- as.factor(recode(data$edu_highest,"'Informal Educaiton'='Informal Education'"))
## edu <- data.frame(unique(data$edu_highest)) # misspelling in dem_education


########################################################################################
cat("Data Cleaning of formal inconsistencies \n")
cat("Data Cleaning of inconsistent categorical data \n")

## select observations which contain only correct categories (wrong are removed automatically)
## as well as special characters as '-' or 'U'
## based on UNHCR registration handbook: http://www.refworld.org/pdfid/3f967dc14.pdf
##                                  and: http://www.refworld.org/pdfid/46a9e29a2.pdf (ref status, occupationcode)

## inconsistencies between handbook and data codes in edu_highest: 
## Technical/Vocational = Technical or vocational
## (... years ...)  = (... year ...)
## Post University Level = Post university level
## No Education = No education

colnames(unhcr.categories) <- c("VAR", "CODE", "CODE", "DESCRIPTION")
var.code <- rbind(unique(unhcr.categories[2]),unique(unhcr.categories[3]))

for (i in 1:length(data)) {
  colname <- colnames(data[i])
  if (any(colname == unique(unhcr.categories["VAR"]))) {
    data <- data[(data[,i] %in% var.code[,1]),]
  }
}
data <- droplevels(data) ##drop unused factor levels
#summary(data)
rm(colname,var.code, i)

removed1 <- 100-nrow(data)/nrow(data.orig)*100
########################################################################################
cat("Data Cleaning of inconsistent spatial data \n")

## remove data rows where country codes do not match with reference table

data <- data[(data$CountryAsylum %in% country.codes$P_Code),]
data <- data[(data$CountryOrigin %in% country.codes$P_Code),]
data <- data[(data$dem_birth_country %in% country.codes$P_Code),]

data <- na.omit(data)
data <- droplevels(data)

nrow(data[(data$coal1id == "YE016"), ])
data$coal1id <- gsub("YE0", "YEM0", data$coal1id)
nrow(data[(data$coal1id == "YE016"), ]) ## check of all YEM codes are corrected, should be 0



########################################################################################
cat("Data Cleaning of missing data \n")

## let's have a look at how many complete cases we have 
nrows <- nrow(data) # how many rows of data
ncomplete <- sum(complete.cases(data)) # how many complete rows
pct.complete <- (ncomplete/nrows)*100 # shows how much percent of the data are complete
data <- na.omit(data) # remove all incomplete rows (that contain NA's)

rm(nrows,ncomplete,pct.complete)

########################################################################################
cat("Outlier Detection \n")

## Age of PA

ggplot(data, aes(x=dem_age)) +
  geom_histogram(binwidth=1, fill="#24245f", color="white") +
  geom_vline(aes(xintercept=mean(dem_age, na.rm=T)),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1) +
  scale_x_continuous(breaks=seq(0,max(data$dem_age),20), expand = c(0, 0))+
  scale_y_continuous(labels = unit_format(unit = "k",scale = 0.001)) +
  labs(x = "Age of Principal Applicant", y = "Number of Cases",
       title = ("Age distribution before data cleaning"), 
       subtitle = "Data Source: UNHCR proGres data") +
  annotate("text", x = 243, y=1000, color="red", label = paste("Max. age:",max(data$dem_age)))
ggsave("out/data_exploration/dem_age_beforeclean.png", width = 26, height = 12, units = c("cm"), dpi = 100)


n <- nrow(data)
##71.4 years was  average life expectancy at birth of the global population in 2015 according to WHO
##data where age of PA is above 90 is removed

data <- data[!(data$dem_age > 90), ] 

pct.removed.ages <- round(100-nrow(data)/n*100,1)


ggplot(data, aes(x=dem_age)) +
  geom_histogram(binwidth=1, fill="#24245f", color="white") +
  geom_vline(aes(xintercept=mean(dem_age, na.rm=T)),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1) +
  scale_x_continuous(breaks=seq(0,max(data$dem_age),20), expand = c(0, 0))+
  scale_y_continuous(labels = unit_format(unit = "k",scale = 0.001)) +
  labs(x = "Age of Principal Applicant", y = "Number of Cases",
       title = ("Age distribution after data cleaning"), 
       subtitle = "Data Source: UNHCR proGres data") +
  annotate("text", x = 60, y=11000, color="red", label = paste("Data where age > 90 is removed\nPct. removed:",pct.removed.ages,"%"))
ggsave("out/data_exploration/dem_age_afterclean.png", width = 26, height = 12, units = c("cm"), dpi = 100)


## Year of Arrival

ggplot(data, aes(x=YearArrival)) + 
  geom_histogram(binwidth=1, fill="#24245f", color="white") +
labs(x = "Year of Arrival", y = "Number of Cases",
     title = ("Number of cases by Year of Arrival before data cleaning"), 
     subtitle = "Data Source: UNHCR proGres data") +
  scale_y_continuous(labels = unit_format(unit = "k",scale = 0.001))
ggsave("out/data_exploration/year_arrival_beforeclean.png", width = 26, height = 12, units = c("cm"), dpi = 100)

data <- data[!(data$YearArrival < 1950), ]

ggplot(data, aes(x=YearArrival)) + 
  geom_histogram(binwidth=1, fill="#24245f", color="white") +
  labs(x = "Year of Arrival", y = "Number of Cases",
       title = ("Number of cases by Year of Arrival after data cleaning"), 
       subtitle = "Data Source: UNHCR proGres data") +
  scale_y_continuous(labels = unit_format(unit = "k",scale = 0.001))
ggsave("out/data_exploration/year_arrival_afterclean.png", width = 26, height = 12, units = c("cm"), dpi = 100)


#############################################################################################
cat("Exclude columns with location names\n")

drop <- c("cool1", "cool2", "coal1", "coal2")
data <- data[ , !(names(data) %in% drop)]


#############################################################################################
cat("Process duplication in age columns\n")

youth.amount <- sum(data$Youth_15_17)/sum(data$Work_15_64)*100
data$Work_18_64 <- as.integer(data$Work_15_64 - data$Youth_15_17)
data$Work_15_64 <- NULL


#############################################################################################
cat("Data Cleaning: Save resulting script\n")
data.cleaned <- data
write.csv(data.cleaned, file = "data/data_processed/data_cleaned.csv")
#rm(unhcr.categories, drop)

pct <- 100-nrow(data.cleaned)/nrow(data.orig)*100
pct <- round(pct,1)
