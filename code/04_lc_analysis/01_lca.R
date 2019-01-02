############################################################################
cat("Latent Class Analysis\n")
############################################################################
#data.gg <- read.csv("data/lca/result_lca_complete_16.csv")

data.lca.numeric <- read.csv("data/data_processed/data_lca_numeric.csv")
data.lca.numeric$X <- NULL
data <- data.lca.numeric
#write.csv(data.lca.numeric, file = "data/lca/data_lca_input.csv") ## subset of input data without spatial variables
#write.csv(data.lca, file = "data/lca/data_lca_complete.csv") ## entire input data with spatial variables

############################################################################
cat("1) Build formula \n")


f<-with(data, cbind(gender,
                     female.headed,
                     agecat,
                     dem_marriage,
                     RefStatus,
                     season,
                     edu_highestcat,
                     occupationcat,
                     dem_ethncat,
                     Num_Inds,
                     AVG_Age,
                     STDEV_Age,
                     status_span,
                     dependency,
                     Youth_15_17_prop,
                     Work_18_64_prop,
                     Child_at_risk,
                     Child_at_risk_carer,
                     Child_at_risk_parent,
                     Child_at_risk_spouse,
                     Child_at_risk_engaged_in_other_forms_of_child_labour,
                     Child_at_risk_engaged_in_worst_forms_of_child_labour,
                     Child_at_risk_of_not_attending_school,
                     Child_at_risk_with_special_education_needs,
                     Child_at_risk_Teenage_pregnancy,
                     Disability,
                     Disability_Visual_impairment_including_blindness_,
                     Disability_Hearing_Impairment_including_deafness_,
                     Disability_Mental_disability_moderate,
                     Disability_Mental_disability_severe,
                     Disability_Physical_disability_moderate,
                     Disability_Physical_disability_severe,
                     Disability_Speech_impairment_disability,
                     Older_person_at_risk,
                     Older_person_at_risk_unable_to_care_for_self,
                     Older_person_at_risk_with_children,
                     Older_person_at_risk_Unaccompanied_older_person,
                     Family_unity,
                     Family_unity_reunification_required,
                     Family_unity_Tracing_required,
                     Specific_legal_and_physical_protection_needs,
                     Specific_legal_and_physical_protection_needs_Formerly_associated_with_armed_forces_or_groups,
                     Specific_legal_and_physical_protection_needs_Violence_abuse_or_neglect,
                     Specific_legal_and_physical_protection_needs_Detained_held_in_country_of_asylum,
                     Specific_legal_and_physical_protection_needs_Detained_held_in_country_of_origin,
                     Specific_legal_and_physical_protection_needs_Mixed_marriage,
                     Specific_legal_and_physical_protection_needs_Marginalized_from_society_or_community,
                     Specific_legal_and_physical_protection_needs_No_legal_documentation,
                     Specific_legal_and_physical_protection_needs_At_risk_of_removal,
                     Specific_legal_and_physical_protection_needs_At_risk_due_to_profile,
                     Unaccompanied_or_separated_child,
                     Unaccompanied_or_separated_child_Separated_child,
                     Unaccompanied_or_separated_child_Unaccompanied_child,
                     Serious_medical_condition,
                     Serious_medical_condition_Critical_medical,
                     Serious_medical_condition_Chronic_illness,
                     Serious_medical_condition_Difficult_pregnancy,
                     Serious_medical_condition_Mental_illness,
                     Serious_medical_condition_Other_medical_condition,
                     Single_parent,
                     Single_parent_Single_HR_caregiver,
                     Single_parent_Single_HR_parent,
                     SGBV,
                     SGBV_Exposure_to_SGBV,
                     SGBV_Exposure_to_SGBV_in_country_of_origin,
                     Torture,
                     Torture_Psych_and_or_physical_impairment_due_to_torture,
                     Torture_Witness_of_violence_to_other,
                     Woman_at_risk,
                     Woman_at_risk_Single_female_household_representative,
                     Woman_at_risk_Lactating_at_risk,
                     Woman_at_risk_Single_woman,
                     Woman_at_risk_Woman_unaccompanied_by_adult_male_family_member,
                     Woman_at_risk_Woman_at_risk_unspecified) ~ 1)




## see: http://statistics.ohlsen-web.de/latent-class-analysis-polca/
## changed loop to go through different number of classes

############################################################################
cat("Creation of dataframe for fitting parameter to compare LC models \n")
lca.fit <- data.frame()
lca.fit[1,1] <- 0
lca.fit[1,2] <- 0
lca.fit[1,3] <- 0
lca.fit[1,4] <- 0
lca.fit[1,5] <- 0
lca.fit[1,6] <- 0
lca.fit[1,7] <- 0
lca.fit[1,8] <- 0
lca.fit[1,9] <- 0
lca.fit[1,10] <- 0
lca.fit[1,11] <- 0
lca.fit[1,12] <- 0
colnames(lca.fit) <- c("n.class", "n.cases", "n.obs", "max.llik", "X^2", "AIC", "BIC", "df", "G^2", "maxiter", "numiter", "time")


############################################################################
cat("2) Identify best model and best number of classes\n")
## here 16 classes are maximum as computational power at its limit (32 GB Memory)

for(i in 1:16){
  cat(paste("Number classes:", i, " \n"))
  lc <- poLCA(f, data, nclass=i, maxiter=3000, tol=1e-5, na.rm=FALSE,  
              nrep=10, verbose=TRUE, calc.se=TRUE)
  
  lca.fit[i,1] <- i
  lca.fit[i,2] <- lc$N
  lca.fit[i,3] <- lc$Nobs
  lca.fit[i,4] <- lc$llik
  lca.fit[i,5] <- lc$Chisq 
  lca.fit[i,6] <- lc$aic
  lca.fit[i,7] <- lc$bic
  lca.fit[i,8] <- lc$resid.df
  lca.fit[i,9] <- lc$Gsq
  lca.fit[i,10] <- lc$maxiter
  lca.fit[i,11] <- lc$numiter
  lca.fit[i,12] <- lc$time
  
  cat(paste("Number iterations:", lc$numiter, " \n"))
  cat(paste("AIC:", lc$aic, " \n"))
  cat(paste("BIC", lc$bic, " \n"))
  cat("Saving current parameter and outputs\n")
  write.csv(lca.fit, file = "data/lca/data_lca_fit_prev.csv") ## fitting parameter

  df <- data.frame(lc$predclass)   # creation of class data 
  class.data <- cbind(data.lca, lc$predclass)
  filepath <- paste0("data/lca/lca_class_data_",i,".csv")
  write.csv(class.data, file = filepath)
  
  # class-conditional probabilities (pijrk) of LC clustering in long format
  lc.probs <- melt(lc$probs)
  lc.probs.long <- dcast(lc.probs, Var1 + L1 ~ Var2)
  filepath <- paste0("data/lca/lca_probs_long_",i,".csv")
  write.csv(lc.probs.long, file = filepath)
}
############################################################################
############################################################################
cat("Visualization of fitting paremeter AIC and BIC\n")

class.data <- read.csv("data/lca/lca_class_data.csv", sep = ",")
lca.fit <- read.csv("data/lca/data_lca_fit_prev.csv", sep = ",")

lca.fit[("X")] <- NULL
lca.fit.melt <- melt(lca.fit[,c(1,6)], id.vars = "n.class")

## aic 
p <- ggplot(data=lca.fit.melt, aes(x=n.class, y=value, group=variable, colour=variable)) +
     geom_line(size=0.8, color="green") +
     geom_point(color="green")+
     labs(x = "Number of Classes", y = "Value of Fitting Parameter", color='',
          title = ("Aikaike's Information Criterion in Latent Class Analysis")) +
     scale_y_continuous(labels = unit_format(unit = "k",scale = 0.001)) +
     scale_x_continuous(limits=c(min(lca.fit.melt$n.class), max(lca.fit.melt$n.class)), 
                     labels=factor(c(min(lca.fit.melt$n.class): max(lca.fit.melt$n.class))),
                     breaks=c(min(lca.fit.melt$n.class): max(lca.fit.melt$n.class)))+
     theme_gdocs()
p
ggsave("out/lca/model_fitting_aic.png", width = 26, height = 8, units = c("cm"), dpi = 300)

## bic
lca.fit.melt <- melt(lca.fit[,c(1,7)], id.vars = "n.class")

## aic and bic
p <- ggplot(data=lca.fit.melt, aes(x=n.class, y=value, group=variable, colour=variable)) +
  geom_line(size=0.8, color="blue") +
  geom_point(color="blue")+
  labs(x = "Number of Classes", y = "Value of Fitting Parameter", color='',
       title = ("Bayes' Information Criterion in Latent Class Analysis")) +
  scale_y_continuous(labels = unit_format(unit = "k",scale = 0.001)) +
  scale_x_continuous(limits=c(min(lca.fit.melt$n.class), max(lca.fit.melt$n.class)), 
                     labels=factor(c(min(lca.fit.melt$n.class): max(lca.fit.melt$n.class))),
                     breaks=c(min(lca.fit.melt$n.class): max(lca.fit.melt$n.class)))+
  theme_gdocs()
p
ggsave("out/lca/model_fitting_bic.png", width = 26, height = 8, units = c("cm"), dpi = 300)



## log likelihood
lca.fit.melt <- melt(lca.fit[,c(1,4)], id.vars = "n.class")

## fitting parameter
p <- ggplot(data=lca.fit.melt, aes(x=n.class, y=value, group=variable, colour=variable)) +
  geom_line(size=0.8, color="red") +
  geom_point(color="red")+
  labs(x = "Number of Classes", y = "Value of Fitting Parameter", color='',
       title = ("Log-Likelihood in Latent Class Analysis")) +
  scale_y_continuous(labels = unit_format(unit = "k",scale = 0.001)) +
  scale_x_continuous(limits=c(min(lca.fit.melt$n.class), max(lca.fit.melt$n.class)), 
                     labels=factor(c(min(lca.fit.melt$n.class): max(lca.fit.melt$n.class))),
                     breaks=c(min(lca.fit.melt$n.class): max(lca.fit.melt$n.class)))+
  theme_gdocs()
p
ggsave("out/lca/model_fitting_llik.png", width = 26, height = 8, units = c("cm"), dpi = 300)

## running time 
p <- ggplot(data=lca.fit, aes(x=n.class, y=time)) +
  geom_bar(size=0.8, stat="identity", fill="#4366b0")+
  labs(x = "Number of Classes", y = "Running time [h]", color='',
       title = ("Running time of Latent Class Model"))+
  scale_x_continuous(breaks = round(seq(min(lca.fit$n.class), max(lca.fit$n.class), by = 1),1))+
  theme_gdocs()
p
ggsave("out/lca/model_fitting_runtime.png", width = 26, height = 8, units = c("cm"), dpi = 300)


#### calculation of amount of parameter calculated in lca
g=0
for (i in 1:length(data.lca.numeric)){
  f <- nrow(unique(data.lca.numeric[i]))
  g <- g+f
}

############################################################################
cat("Prepare and save additional outputs\n")

#### Categories based on: https://jonathantemplin.com/files/clustering/psyc993_14.pdf
## Model-based measures of “fit:”
### "max.llik" <- maximum log-likelihood
### "X^2" <- Chi-square goodness of fit
#
##Model comparison measures:
### "AIC" <- Akaike Information Criterion
### "BIC" <- Bayesian Information Criterion (also A.K.A. Schwarz’s Criterion)
#
##Distributional comparisons:
### "df" <- residual degrees of freedom
### "G^2" <- Likelihood ratio/deviance statistic


## change order of column for output table
lca.fit <- lca.fit[,c(1,2,3,8,5,6,4,7,9,10,11,12)]

z=ztable(lca.fit)
cgroup=c(" ", "Model-based measures of 'fit'","Model comparison measures","Distributional comparison","Computational Information")
n.cgroup=c(3,2,2,2,3)
z=addcgroup(z,cgroup=cgroup,n.cgroup=n.cgroup)

#z=addRowColor(z,c(6),"pink")
z=addColColor(z,c(7),"alizarin")
z=addColColor(z,c(8),"cerulean")
#z=addCellColor(z,rows=c(6),cols=c(6,7),"orange")
z


## class proportions
#class.size <- rbind(lc$P, lc$P.se)
#colnames(class.size) <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16")
#write.csv(class.size, file = "data/lca/lca_class_size.csv")
class.size <- read.csv("data/lca/lca_class_size.csv", sep = ",")
class.size$X <- NULL
colnames(class.size) <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16")

q=ztable((class.size))
q=ztable(q,caption="Classes",align="ccccccc")

# ## starting values
# probs.start<-lc$probs.start
# saveRDS(lc$probs.start,"data/lca/lca_starting_values.RData")

############################################################################
############################################################################
