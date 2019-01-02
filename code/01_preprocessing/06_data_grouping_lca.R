#############################################################################################
cat("Data Grouping and recoding for LCA: Positive integers are needed for LCA\n
    1) Recoding categorical data to positive integer\n
    2) Grouping numeric sociodemographic data to equally sized groups and recode to positive integers\n
    3) Grouping sparse data of specific needs and risks to meaningful groups")
## result of this scrips:
# data.lca <- read.csv("data/data_processed/data_lca.csv")
#############################################################################################

#data.analysis <- read.csv("data/data_processed/data_analysis_pre.csv")
#data.analysis[,c("X.1", "X")] <- NULL
## Grouping schema:
## 1) Catecoric data is already grouped -> only recoding in positive integer is nesseccary
## 2) Specific needs and risks data is sparse data and will be grouped by meaningful manual breaks
## (Equally sized groups woudn't be possible, because most data is 0 or 1)
## 3) Numeric demographic data will be grouped into equally sized groups
#### https://stats.stackexchange.com/questions/108010/lca-number-of-parameters-degrees-of-freedom
## -> That means that for each of your variables, poLCA will assume each value is a unique category, 
## and that there are as many possible categories (outcomes) as the highest value in the variable. 

data <- data.analysis
conversion.catdata.lca <- conversion.catdata
conversion.lca <- unique(conversion.catdata.lca[, c(3,4)])
conversion.lca$lca <- as.numeric(conversion.lca$lca)

#############################################################################################
cat("Categories which are already coded for LCA\n")
data.lca.coded <- data.frame(cbind(data$gender, data$female.headed, data$YearArrivalcat, data$RefugeeStatusDatecat, data$agecat))
colnames(data.lca.coded) <- c("gender", "female.headed", "YearArrivalcat", "RefugeeStatusDatecat", "agecat")
data[,c("gender", "female.headed", "YearArrivalcat", "RefugeeStatusDatecat", "agecat")] <- NULL



#############################################################################################
cat("Split data in numeric and categoric variables\n")
X.quanti <- splitmix(data)$X.quanti  ## (splitmix is from package 'PCAmixdata')
X.quali <- splitmix(data)$X.quali



#############################################################################################
cat("Recode categorical data for LCA\n")

a <- grep("^coal2id$", colnames(X.quali))
X.quali.recoded <- X.quali[,(a+1):length(X.quali)] ## remove spatial variables

for (i in 1:length(X.quali.recoded)) {
  prop.table(table(X.quali.recoded[,i], useNA = "ifany"))
  a <- colnames(X.quali.recoded[i])
  X.quali.recoded[,i] <- X.quali.recoded[,i] %l%  conversion.lca[, c(1,2)]
  prop.table(table(X.quali.recoded[,i], useNA = "ifany"))
}

rm(conversion.lca)


#############################################################################################
cat("Categorize demographic numerical variables to equally sized groups\n")
a <- grep("^Child_at_risk$", colnames(X.quanti))
X.quanti.grouped <- X.quanti


for (i in 1:(a-1)) {
  
    X.quanti.grouped[,i] <- data.frame(bin_data(X.quanti.grouped[,i], bins=5, binType = "quantile"))
  
    if (nrow(count(unique(X.quanti.grouped[,i]))) > 1) {
        conversion.sub <- data.frame()

        for (j in 1:(length(unique(X.quanti.grouped[,i])))) {
            conversion.sub[j,1] <- colnames(X.quanti.grouped[i])
            conversion.sub[j,2] <- "num.rawdata"
            conversion.sub[j,3] <- unique(X.quanti.grouped[,i])[j]
            conversion.sub[j,4] <- j
            }
  
  colnames(conversion.sub) <- c("variable", "raw", "grouping", "lca")
  prop.table(table(X.quanti.grouped[,i], useNA = "ifany"))
  X.quanti.grouped[,i] <- X.quanti.grouped[,i] %l%  conversion.sub[,c(3,4)]
  prop.table(table(X.quanti.grouped[,i], useNA = "ifany"))
  conversion.catdata.lca <- rbind(conversion.catdata.lca, conversion.sub)

    } else {
        X.quanti.grouped[,i] <- NULL
        a <- a-1
      }
}


#############################################################################################
cat("Categorize sparse numerical variables to meaningful groups\n")

for (i in a:length(X.quanti.grouped)) {
    X.quanti.grouped[,i] <- cut(X.quanti.grouped[,i],
                                breaks = c(-Inf,1,2,4,6,Inf),
                                labels = c(1,2,3,4,5),
                                right=FALSE)  
    b <- colnames(X.quanti.grouped[i])
    brks <- c("[0]","[1]","[2,3]","[4,5]","[6,Inf]")
    lbs <- c(1,2,3,4,5)
    conversion.sub <- data.frame()
    for (j in 1:length(unique(X.quanti.grouped[,i]))) {
        conversion.sub[j,1] <- b
        conversion.sub[j,2] <- "num.rawdata"
        conversion.sub[j,3] <- brks[j]
        conversion.sub[j,4] <- lbs[j]
    }
    colnames(conversion.sub) <- c("variable", "raw", "grouping", "lca")
    conversion.catdata.lca <- rbind(conversion.catdata.lca, conversion.sub)
}


#############################################################################################
cat("Combine data to final dataframe for LCA\n")
a <- grep("^coal2id$", colnames(X.quali))
data.lca <- cbind(X.quali[,1:a], data.lca.coded, X.quali.recoded, X.quanti.grouped)
data.lca <- droplevels(data.lca) ##drop unused factor levels
conversion.data <- conversion.catdata.lca
conversion.data$lca <- as.numeric(conversion.data$lca)

write.csv(data.lca, file = "data/data_processed/data_lca.csv")
write.csv(conversion.data, file = "data/data_processed/conversion_data.csv")

rm(a, b, brks, i, j, lbs, X.quali,  X.quanti, X.quali.recoded, data.lca.coded, X.quanti.grouped)
rm(conversion.sub, conversion.catdata.lca)





