#############################################################################################
cat("FROM NUMERICAL TO CATEGORICAL: Data Processing only for Latent Class Analysis:\n
    1) Removing dummy variables\n
    2) Subset data to variables with minimum sample size")
## result of this scrips: ("data/data_processed/data_analysis.csv")
#############################################################################################
cat("Necessary Data: Activate data load if variables are not existent\n")

data.analysis.pre <- read.csv("data/data_processed/data_analysis_pre.csv", sep = ",")
data.analysis.pre$X <- NULL
data.cleaned <- read.csv("data/data_processed/data_cleaned.csv", sep = ",")
data.cleaned$X <- NULL

#############################################################################################
cat("Remove dummy variables for lca as only categorical data will be used\n")

data <- data.analysis.pre

##Montharrival
a <- grep(".dummy.", colnames(data)) 
b <- colnames(data[,a])
data <- data[ , !(names(data) %in% b)]
rm(a,b)

#############################################################################################
cat("Aggregate age of Principal Applicant\n")
## visualization of distribution of unhcr age groups
## -> You can see that most data is in one age group
## -> for LCA equally distributed groups are better
data$dem_agegroup <- data.cleaned$dem_agegroup

ggplot(data, aes(x=dem_agegroup)) + 
  geom_bar(stat="count", width=.1, fill="#24245f", color="white") +
  labs(x = "Age Groups", y = "Number of Cases",
       title = ("Number of cases by age group before data cleaning"), 
       subtitle = "Data Source: UNHCR proGres data") +
  scale_y_continuous(labels = unit_format(unit = "k",scale = 0.001))
ggsave("out/data_exploration/age_groups_beforeclean.png", width = 26, height = 12, units = c("cm"), dpi = 100)

prop.table(table(data$dem_agegroup, useNA = "ifany"))

## grouping age to equally sized age groups
data$agecat <- bin_data(data$dem_age, bins=5, binType = "quantile")
conversion.sub <- data.frame()
for (i in 1:length(unique(data$agecat))) {
  conversion.sub[i,1] <- "agecat"
  conversion.sub[i,2] <- "num.rawdata"
  conversion.sub[i,3] <- unique(data$agecat)[i]
  conversion.sub[i,4] <- i
}
conversion.sub[,4] <- as.factor(conversion.sub[,4])
colnames(conversion.sub) <- c("variable", "raw", "grouping", "lca")


prop.table(table(data$agecat, useNA = "ifany"))
data$agecat <- data$agecat %l%  conversion.sub[,c(3,4)]
prop.table(table(data$agecat, useNA = "ifany"))

## visualization of distribution of unhcr age groups
ggplot(data, aes(x=agecat)) + 
  geom_bar(stat="count", width=.1, fill="#24245f", color="white") +
  labs(x = "Age Groups", y = "Number of Cases",
       title = ("Number of cases by age group before data cleaning"), 
       subtitle = "Data Source: UNHCR proGres data") +
  scale_y_continuous(labels = unit_format(unit = "k",scale = 0.001))
ggsave("out/data_exploration/age_groups_afterclean.png", width = 26, height = 12, units = c("cm"), dpi = 100)


conversion.catdata <- rbind(conversion.catdata, conversion.sub)
rm(conversion.sub)

data$dem_agegroup <- NULL
# #############################################################################################
# cat("Convert years to factor\n")
# data[,"YearArrival"] <- as.factor(data[,"YearArrival"])
# data[,"RefugeeStatusDate"] <- as.factor(data[,"RefugeeStatusDate"])

#############################################################################################
cat("Reorder data: Demographic variables before specific needs variables\n")
a <- grep("^Child_at_risk$", colnames(data))
b <- grep("^Woman_at_risk_Woman_at_risk_unspecified$", colnames(data))
data <- data[,c(1:(a-1),(b+1):length(data),a:b)]



#############################################################################################
cat("Exclude numerical data which is replaced by categories\n")
## was needed in maps as numerical data processed in maps
data$dem_age <- NULL
data$Child_0_14 <- NULL
data$Youth_15_17 <- NULL
data$Work_18_64 <- NULL
data$Eldern_65 <- NULL

#############################################################################################
cat("Recode binary zero to positive value\n")
data$female.headed[data$female.headed ==  0] <- 2

#############################################################################################
cat("Subset data to columns with minimum sample size\n")

## minimum sample size (col with all zero are removed in this step as well)
## usually 0.05 or 0.01 are used
e = 0.01
N = nrow(data.cleaned)
n=N/(1+N*(e^2))
n <- round(n)

## subset of sparse data where sum of numerical columns is more than minimum sample size
nobs <- data.frame()
for (i in 1:length(data)) {
  nobs[i,1] <- colnames(data[i])
  nobs[i,2] <- length(which(data[,i] != 0))
  nobs[i,3] <- length(which(data[,i] != 0)) > n
}
colnames(nobs) <- c("variable", "n rows not zero", "> 0.01")

e = 0.02
N = nrow(data.cleaned)
n=N/(1+N*(e^2))
n <- round(n)

for (i in 1:length(data)) {
  nobs[i,4] <- length(which(data[,i] != 0)) > n
}
colnames(nobs) <- c("variable", "n rows not zero", "> 0.01", "> 0.02")

e = 0.05
N = nrow(data.cleaned)
n=N/(1+N*(e^2))
n <- round(n)

for (i in 1:length(data)) {
  nobs[i,5] <- length(which(data[,i] != 0)) > n
}
colnames(nobs) <- c("variable", "n rows not zero", "> 0.01", "> 0.02", "> 0.05")

z <- ztable(nobs) ## package: ztable
z

## reduce data for lca to variables where minimum sample size for 0.5 level
data.analysis <- data[1]
for (i in 2:length(data)) {
  if(nobs[i,5] == TRUE){
    data.analysis <- cbind(data.analysis, data[,i])
    
  }
  
}
colnames(data.analysis) <- nobs[,1][nobs[,5] == TRUE]



## backup processed data for further preprocessing of data for lca
data.analysis <- droplevels(data.analysis) ##drop unused factor levels
data <- data.analysis

rm(a, b, e, N, n, z, df, nobs)

############################################################################
cat("Remove columns with less than two unique categories \n")

data.analysis <- data.analysis[, sapply(data.analysis, function(col) length(unique(col))) > 1]
write.csv(data.analysis, file = "data/data_processed/data_analysis.csv")


### check how many risks have rowsum 0
test <- data.analysis[31:length(data.analysis)]
r.sums <- data.frame(rowSums(test))
n.rowsums <- count(r.sums==0)
rm(r.sums, n.rowsums)
