######################################################################################
cat("Latent class analysis: Class Evaluation\n")

#rm(list=setdiff(ls(), c("conversion.back")))
install.packages("lookup")
library(lookup)

######################################################################################
cat("Load relevant data\n")

lca.fit <- read.csv("data/lca/lca_fit.csv", sep = ",")
lca.fit[c("X", "X.1")] <- NULL
class.data <- read.csv("data/lca/lca_class_data_16.csv", sep = ",")
class.data[c("X", "X.1")] <- NULL
lca.data.input <- read.csv("data/lca/data_lca_input.csv", sep = ",")
lca.data.input[c("X", "X.1")] <- NULL
lca.data.complete <- read.csv("data/lca/data_lca_complete.csv", sep = ",")
lca.data.complete[c("X", "X.1")] <- NULL
conversion.back <- read.csv("data/data_processed/conversion_back.csv", sep = ",")
conversion.back[c("X", "X.1")] <- NULL

lc.probs.long <- read.csv("data/lca/lca_probs_long_16.csv", sep = ",")
lc.probs.long$X <- NULL

######################################################################################
cat("Claculate highest values for each row and column by variable attribute\n")

### User input ###
prob.start <- "Pr.1." ## column name of first class-conditional probability column
colname.variable <- "L1"
#class.maxvalues <- 1 ## first class to evaluate
################

variables <- unique(lc.probs.long[,colname.variable])
len.dataset <- length(lc.probs.long) 


for (k in grep(prob.start, colnames(lc.probs.long), fixed = TRUE):len.dataset) {
  lc.probs.long[len.dataset+k-grep(prob.start, colnames(lc.probs.long), fixed = TRUE)+1] <- 0
  colnames(lc.probs.long)[len.dataset+k-grep(prob.start, colnames(lc.probs.long), fixed = TRUE)+1] <- paste0("classcomp",k-grep(prob.start, colnames(lc.probs.long), fixed = TRUE)+1)
  for (i in 1:length(variables)) {
    var <- variables[i]
    for (j in 1:nrow(lc.probs.long)) {
      if (lc.probs.long[j,colname.variable]==var){
        
        lc.probs.long[j,len.dataset+k-grep(prob.start, colnames(lc.probs.long), fixed = TRUE)+1] <- 
          lc.probs.long[j,k]/mean(lc.probs.long[,k][lc.probs.long[,colname.variable] == var])
      }
    }
  }
}

rm(i,j,k)



###########################################################################################
cat("Extract strongest characteristics for each class\n")


### User input ###
n.maxvalue <- 15  # number of strongest variables to extract
maxvalue.rel.lowerlimit = 0 # threshold for lowest probability
###########

len.dataset.mod = length(lc.probs.long)
maxvalues <- data.frame()
class.list <- list()

for (class.maxvalues in 1:length(unique(lc.probs.long$Var1))) { # loop through classes

    for (i in 1:n.maxvalue){
        maxvalues[i,1] <- 0
        maxvalues[i,2] <- 0
        maxvalues[i,3] <- 0
        maxvalues[i,4] <- 0
        }

  
  for (spalte in (len.dataset+1):len.dataset.mod){
    for (zeile in 1:nrow(lc.probs.long)){
      if (!is.na(lc.probs.long[zeile,spalte]) & lc.probs.long[zeile,1]==(unique(lc.probs.long$Var1))[class.maxvalues]){
        maxvalue.rel <- lc.probs.long[zeile,spalte-(len.dataset.mod-len.dataset)]/rowMeans(lc.probs.long[zeile,3:len.dataset], na.rm=TRUE)
        if (lc.probs.long[zeile,spalte] > maxvalues[n.maxvalue,1] & maxvalue.rel > maxvalue.rel.lowerlimit){
          m = n.maxvalue + 1
          repeat{
            m=m-1
            if(lc.probs.long[zeile,spalte] < maxvalues[m,1] | m == 1){
              maxvalues = rbind(maxvalues[1:m,],maxvalues[1:1,],maxvalues[(m+2):n.maxvalue,])
              maxvalues[m+1,1] <- lc.probs.long[zeile,spalte]
              maxvalues[m+1,2] <- lc.probs.long[zeile,spalte-(len.dataset.mod-len.dataset)]/rowMeans(lc.probs.long[zeile,3:len.dataset], na.rm=TRUE)
              maxvalues[m+1,3] <- spalte - (len.dataset.mod-len.dataset+2)
              maxvalues[m+1,4] <- as.character(lc.probs.long[zeile,colname.variable])
              break
            }
          }
        }
      }
    }
  }

  
  maxvalues <- maxvalues[!(maxvalues$V3 == 0), ]
  
  # ## V1 = value
  # ## V2 = mean
  # ## V3 = lca attribute
  # ## var = variable

  #################################################
  maxvalues.bind <- maxvalues

  maxvalues.bind$grouping <- as.character("i")
  maxvalues.bind$V3 <- as.character(maxvalues.bind$V3)
  conversion.back$variable <- as.character(conversion.back$variable)
  maxvalues.bind$class <- paste0("class_",class.maxvalues)

  for (i in 1:nrow(maxvalues.bind)) {
    var <- (maxvalues.bind[i,"V4"])
    df <- data.frame(conversion.back[(conversion.back$variable==var), ], stringsAsFactors=FALSE)
    df <- df[,c("lca", "grouping")]
    df$lca <- as.character(df$lca)
    df <- unique(df)
    df$grouping <- as.character(df$grouping)
    maxvalues.bind[i,"grouping"] <- maxvalues.bind[i,"V3"] %l%  df[,c(1,2)]
  }

  class.list[[class.maxvalues]] <- maxvalues.bind
}
  
  

class.maxvalues <- 14
## most characterizing variable attrbutes ordered by distance to mean
df <- class.list[[class.maxvalues]][order(-class.list[[class.maxvalues]][["V2"]]),] 

result_lca <- do.call("rbind", class.list)

write.csv(result_lca, file = "data/lca/result_lca_complete_16.csv")


