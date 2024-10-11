########################################################################################################
# The Effectiveness of Social-Emotional Learning Interventions for Children in Early Childhood Education: A Meta-Analysis 
########################################################################################################
# Authors: J. Wang, S. Byun, X. Zhao, S. Chapman
# Revised: Sept. 2024

# This file analyzes the included studies in the PreK SEL research synthesis, 
# including preparing the data for analysis and meta-regressions as well as 
# producing tables and figures for the manuscript.

########################################################################################################
# Initial Set-up
########################################################################################################
# Clear Variables
rm(list=ls(all=TRUE))

# Load packages
test<-require(plyr)   #rename()
if (test == FALSE) {
  install.packages("plyr")
  require(plyr)
}
rm(test)

library(plyr)

packages <- c("googledrive","googlesheets4", "clubSandwich","dplyr", "janitor","fastDummies", "flextable", "ggplot2", "metafor", "officer", "plotly", "robumeta", "tableone", "weightr")   

not_installed <- packages[!(packages %in% installed.packages()[ , "Package"])]    # Extract not installed packages
if(length(not_installed)) install.packages(not_installed)             

invisible(lapply(packages, library, character.only = TRUE))

rm(not_installed, packages)

########################################################################################################
# Load data
########################################################################################################
# set up to load from Google

#gs4_auth()  ##authorize google sheets access

gs4_auth(scopes = "https://www.googleapis.com/auth/spreadsheets.readonly")   ##need to reauthorized 

# load findings and studies
sheet_id <- "1mzaNGwTVCQJE6NMy9k91IIvkAszJ3wR-plQblIWzU18"  
  #make the sheet public first or share with the email of R

studies<-read_sheet(sheet_id,sheet="Study Coding") # includes separate effect sizes for each finding from a study

findings <-read_sheet(sheet_id,sheet = "Outcome Coding")   # includes study-level codes

########################################################################################################
# Clean data
########################################################################################################
# remove any empty rows & columns
findings <- remove_empty(findings, which = c("rows", "cols"))

studies <- remove_empty(studies, which = c("rows", "cols"))


# limit to relevant columns
studies <- studies[c("Covidence Code", "APA", "Year", "Intervention", "Study No.", 
                     "Peer reviewed", "Targeted Population", 
                     "Study Design", "Child Age", "Child Age Coding", 
                     "Duration (beginning of the intervention to post-test; 12 weeks+)", 
                     "Duration coding", "Country", "Sample Description")]

findings <- findings[c("Covidence Code", "APA", "Year", "Intervention", 
                       "StudyNo", "TxClusterN", "CtrlClusterN", "ClusterType", 
                       "TxN", "CtrlN", "TotalN",
                       "Outcome_Category", 
                       "Outcome_Detail", 
                       "Outcome", "Measure", "Measure_Personnel", "Type of Measure",
                       "Type of ES (1=unadjusted m/sd; 2=adjusted m/sd; 3=study ES; 4=beta; 5=other)", 
                       "Study Effect Size", "Reverse Coded (lower scores are better)")]

# merge dataframes
library(dplyr)

full <- findings %>% inner_join(studies, by = c('Covidence Code'='Covidence Code','Year'='Year', 'Intervention'='Intervention'))

View(full)
View(studies)
View(findings)

# drop if ES is missing
full <- subset(full, is.na(full$"Study Effect Size") == FALSE)

# reformat columns as necessary
num <- c("TxClusterN", "CtrlClusterN", "TxN", "CtrlN", "TotalN", "Study Effect Size")

full[num] <- lapply(full[num], function(x) as.numeric(as.character(x)))

rm(num)

# rename variables
full <-full%>% rename("Code"="Covidence Code", "Citation"="APA.x", 
                      "sample_desc" = "Sample Description","Program"= "Intervention", "uni_target" = "Targeted Population",
                      "Journal"="Peer reviewed","Studydesign"="Study Design","Agegroup"="Child Age Coding", 
                      "EStype"="Type of ES (1=unadjusted m/sd; 2=adjusted m/sd; 3=study ES; 4=beta; 5=other)",
                      "Reverse"="Reverse Coded (lower scores are better)", "ES"="Study Effect Size",
                      "duration"="Duration coding", "personnel"="Measure_Personnel")


########################################################################################################
# Prep data
########################################################################################################
################################################################
# Create unique identifiers (ES, study, program)
################################################################
full$ESId <- as.numeric(rownames(full))

full$programID<-as.numeric(as.factor(paste(full$Program,full$Code)))  
        #code (individual study)+program (one study may contain multiple "programs")

table (full$programID)

################################################################
# Create dummies and categorical variables
################################################################
#sample size
full$LargeSample <-0
hist(full$TotalN, main="Hist of Sample size")
full$LargeSample[which(full$`TotalN`>=300)] <- 1   #300 is consider to be a large sample size in RCT
table(full$`TotalN`, full$LargeSample, useNA = "ifany")   #useNA: how missing (NA) values should be handled. ifany: NA values will be included in the table only if there are any missing values in the data
full$SmallSample <-0
full$SmallSample[which(full$`TotalN`<300)] <- 1  

#duration
full$LongDuration<-0
full$LongDuration[which(full$duration==2)] <- 1
table(full$`duration`, full$LongDuration, useNA = "ifany")
full$ShortDuration<-0
full$ShortDuration[which(full$duration==1)] <- 1

#study design
table (full$Studydesign)  #Quasi: 3+32, randomized:6+359: create a dummy: randomized vs. non-randomized
full$randomized <- 0
full$randomized[which(full$Studydesign >=3)] <-1
full$quasi <- 0
full$quasi[which(full$Studydesign <= 2)] <- 1
table (full$randomized, full$quasi, useNA = "ifany")

#assignment level
full$clustered <- 0
full$clustered[which(full$Studydesign==2 | full$Studydesign==4)] <-1
full$individual <- 0
full$individual[which(full$Studydesign==1 | full$Studydesign==3)] <-1
table (full$clustered, full$individual, useNA = "ifany")

#universal vs. targeted
full$targeted <- 0
full$targeted[which(full$uni_target==1)] <- 1
full$universal <- 0
full$universal[which(full$uni_target==0)] <- 1
table (full$targeted, full$universal, useNA = "ifany")


#published vs. unpublished
full$published<-0
full$published[which(full$Journal==1)] <-1
full$unpublished<-0
full$unpublished[which(full$Journal==0)] <-1

table (full$published, full$unpublished, useNA = "ifany")

#outcome main category
full$dv_pp<-0
full$dv_pp[which(full$Outcome_Category=="Positive Teacher Practice")]<-1

full$dv_ew<-0
full$dv_ew[which(full$Outcome_Category=="Emotional Well-being")]<-1

full$dv_pb<-0
full$dv_pb[which(full$Outcome_Category=="Problem Behaviors")]<-1

full$dv_sr<-0
full$dv_sr[which(full$Outcome_Category=="Social Relationship")]<-1

full$dv_af<-0
full$dv_af[which(full$Outcome_Category=="Academic Functioning")]<-1

#measure personnel
full$tch_report <- 0
full$tch_report[which(full$personnel=="Teacher-report")] <- 1

full$prt_report <- 0
full$prt_report[which(full$personnel=="Parent-report")] <- 1

full$chd_report <- 0
full$chd_report[which(full$personnel=="Child-Report")] <- 1

full$ind_obs <- 0
full$ind_obs[which(full$personnel=="Independent observer")] <- 1
full$ind_obs[which(full$personnel=="trained staff")] <- 1


full$researcher_report <- 0
full$researcher_report[which(full$personnel=="Observation/Researcher")] <- 1

full$standardized <- 0
full$standardized[which(full$personnel=="Standardized test")] <- 1


##check correlations
# Select the relevant columns from your 'full' data frame
#subset_full <- full[, c("LargeSample", "LongDuration", "randomized", "clustered", "targeted", "published", "dv_pp", "dv_ew", "dv_pb", "dv_sr", "dv_af", "tch_report", "prt_report", "chd_report", "ind_obs", "researcher_report", "standardized")]

# Compute the Spearman correlation matrix
#cor_matrix <- cor(subset_full, method = "spearman")

# View the correlation matrix
#print(cor_matrix)

##Create subdata for each outcomes##
acad <- subset(full, full$Outcome_Category=="Academic Functioning")

saveRDS(acad, file="academic functioning.rds") 

prob <- subset(full, full$Outcome_Category=="Problem Behaviors")

saveRDS(prob, file="problem behaviors.rds") 

popr <- subset(full, full$Outcome_Category=="Positive Teacher Practice")

saveRDS(popr, file="positive teacher practice.rds") 

emow <- subset(full, full$Outcome_Category=="Emotional Well-being")

saveRDS(emow, file="emotional well-being.rds") 

social <- subset(full, full$Outcome_Category=="Social Relationship")

saveRDS(social, file="social relationship.rds") 

################################################################
# Create centered variables
################################################################

# alphabetize by program
full <- full[order(full$programID, full$Outcome_Category, full$Outcome_Detail),]

# create total clusters variables
full$Clusters_Total <- full$TxClusterN+full$CtrlClusterN

# reverse coded ES
table (full$Reverse)
full$ES<-ifelse(full$Reverse==1,0-full$ES, full$ES)

#center
vars <-c("LargeSample", "SmallSample", "LongDuration", "ShortDuration", "randomized", "quasi",
         "clustered", "individual", "targeted", "universal", "published","unpublished", "dv_pp", "dv_ew", "dv_sr",
         "dv_pb", "dv_af", "tch_report", "prt_report", "chd_report", "ind_obs", "standardized", "researcher_report")

centered<-paste(vars,".c", sep="")

full[centered]<-as.data.frame(lapply(full[vars],function(x) x-mean(x)))

################################################################
# Calculate variance
################################################################
#calculate standard errors
full$se<-sqrt((full$TotalN/(full$TxN*full$CtrlN))+((full$ES*full$ES)/(2*(full$TxN+full$CtrlN))))

#calculate variance
full$var<-full$se*full$se


#################################################################################
# Exploratory Analyses
#################################################################################

#################################################################################
# Descriptive Statistics
#################################################################################
library(tableone)

# identify variables for descriptive tables (study-level and outcome-level)
vars_study <- c("Country", "Program",  "LargeSample",
                "LongDuration", "randomized", "quasi", "clustered", "individual",  
                "targeted", "universal", "unpublished", "published")

vars_outcome <- c("dv_pp", "dv_ew", "dv_sr",
                  "dv_pb", "dv_af", "tch_report", "prt_report", "chd_report", 
                  "ind_obs", "standardized", "researcher_report")

# To make this work, you will need a data frame that is at the study-level for study-level 
# variables (such as research design) you may have already created this (see above, with study-level ESs), but if you didn't, here is an easy way:

# 1) make df with *only* the study-level variables of interest and studyIDs in it
# study <- unique(df[c("Authors", "Year", "StudyID")])

study_level<-full[c("programID", vars_study)]

# 2) remove duplicated rows
study_level <- unique(study_level)

study_level <- study_level %>% 
  group_by(programID) %>% 
  filter(LargeSample==max(LargeSample)) %>%
  ungroup()     #only keep one row if the program have different sample sizes for different outcomes

# 3) make sure it is the correct number of rows (should be same number of studies you have)
length(study_level$programID)==length(unique(full$programID))

# don't skip step 3 - depending on your data structure, some moderators can be
# study-level in one review, but outcome-level in another

# create the table "chunks"
table_study_full <- as.data.frame(print(CreateTableOne(vars = vars_study, data = study_level, factorVars = vars_study, includeNA = TRUE), showAllLevels = TRUE))
table_outcome_full <- as.data.frame(print(CreateTableOne(vars = vars_outcome, data = full, includeNA = TRUE, factorVars = vars_outcome), showAllLevels = TRUE))

rm(vars_study, vars_outcome)

#################################################################################
# Null Model
#################################################################################

V_list <- impute_covariance_matrix(vi = full$var,  #known correlation vector
                                   cluster = full$programID, #ProgramID
                                   r = 0.70) #assumed correlation  #do a sensitivity analysis

MVnull <- rma.mv(yi=ES, #effect size
                 V = V_list, #variance (tHIS IS WHAt CHANGES FROM HEmodel)
                 random = ~1 | programID/ESId, #nesting structure
                 test= "t", #use t-tests
                 data=full, #define data
                 method="REML") #estimate variances using REML
MVnull

#t-tests of each covariate #
MVnull.coef <- coef_test(MVnull,#estimation model above
                         cluster=full$programID, #define cluster IDs
                         vcov = "CR2") #estimation method (CR2 is best) #robust standard error
MVnull.coef

# estimate a covariance matrix
  #academic functioning
acad <- subset(full, full$Outcome_Category=="Academic Functioning")

V_list <- impute_covariance_matrix(vi = acad$var,  #known correlation vector
                                   cluster = acad$programID, #ProgramID
                                   r = 0.70) #assumed correlation 

acadnull <- rma.mv(yi=acad$ES, #effect size
                 V = V_list, #variance (tHIS IS WHAt CHANGES FROM HEmodel)
                 random = ~1 | programID/ESId, #nesting structure
                 test= "t", #use t-tests
                 data=acad, #define data
                 method="REML") #estimate variances using REML
acadnull

#t-tests of each covariate #
acad_coef_null <- coef_test(acadnull,#estimation model above
                            cluster=acad$programID, #define cluster IDs
                            vcov = "CR2") #estimation method (CR2 is best)

acad_coef_null

#problem behaviors
prob <- subset(full, full$Outcome_Category=="Problem Behaviors")

V_list <- impute_covariance_matrix(vi = prob$var,  #known correlation vector
                                   cluster = prob$programID, #ProgramID
                                   r = 0.70) #assumed correlation 

probnull <- rma.mv(yi=prob$ES, #effect size
                   V = V_list, #variance (tHIS IS WHAt CHANGES FROM HEmodel)
                   random = ~1 | programID/ESId, #nesting structure
                   test= "t", #use t-tests
                   data=prob, #define data
                   method="REML") #estimate variances using REML
probnull

#t-tests of each covariate #
prob_coef_null <- coef_test(probnull,#estimation model above
                            cluster=prob$programID, #define cluster IDs
                            vcov = "CR2") #estimation method (CR2 is best)

prob_coef_null

#positive teacher practice
popr <- subset(full, full$Outcome_Category=="Positive Teacher Practice")

V_list <- impute_covariance_matrix(vi = popr$var,  #known correlation vector
                                   cluster = popr$programID, #ProgramID
                                   r = 0.70) #assumed correlation 

poprnull <- rma.mv(yi=popr$ES, #effect size
                   V = V_list, #variance (tHIS IS WHAt CHANGES FROM HEmodel)
                   random = ~1 | programID/ESId, #nesting structure
                   test= "t", #use t-tests
                   data=popr, #define data
                   method="REML") #estimate variances using REML
poprnull

#t-tests of each covariate #
popr_coef_null <- coef_test(poprnull,#estimation model above
                            cluster=popr$programID, #define cluster IDs
                            vcov = "CR2") #estimation method (CR2 is best)

popr_coef_null

#emotional wellbeing
emow <- subset(full, full$Outcome_Category=="Emotional Well-being")

V_list <- impute_covariance_matrix(vi = emow$var,  #known correlation vector
                                   cluster = emow$programID, #ProgramID
                                   r = 0.70) #assumed correlation 

emownull <- rma.mv(yi=emow$ES, #effect size
                   V = V_list, #variance (tHIS IS WHAt CHANGES FROM HEmodel)
                   random = ~1 | programID/ESId, #nesting structure
                   test= "t", #use t-tests
                   data=emow, #define data
                   method="REML") #estimate variances using REML
emownull

#t-tests of each covariate #
emow_coef_null <- coef_test(emownull,#estimation model above
                            cluster=emow$programID, #define cluster IDs
                            vcov = "CR2") #estimation method (CR2 is best)

emow_coef_null

#Social Relationship
social <- subset(full, full$Outcome_Category=="Social Relationship")

V_list <- impute_covariance_matrix(vi = social$var,  #known correlation vector
                                   cluster = social$programID, #ProgramID
                                   r = 0.70) #assumed correlation 

socialnull <- rma.mv(yi=social$ES, #effect size
                   V = V_list, #variance (tHIS IS WHAt CHANGES FROM HEmodel)
                   random = ~1 | programID/ESId, #nesting structure
                   test= "t", #use t-tests
                   data=social, #define data
                   method="REML") #estimate variances using REML
socialnull

#t-tests of each covariate #
social_coef_null <- coef_test(socialnull,#estimation model above
                            cluster=social$programID, #define cluster IDs
                            vcov = "CR2") #estimation method (CR2 is best)

social_coef_null

#################################################################################
# Metaregression
#################################################################################
# use centered versions
formula <- reformulate(termlabels = c(centered))

V_list <- impute_covariance_matrix(vi = full$var,  #known correlation vector
                                   cluster = full$programID, #study ID
                                   r = 0.70) #assumed correlation 

MVfull <- rma.mv(yi=full$ES, #effect size
                 V = V_list, #variance (tHIS IS WHAt CHANGES FROM HEmodel)
                 mods = formula, #ADD COVS HERE
                 random = ~1 | programID/ESId, #nesting structure
                 test= "t", #use t-tests
                 data=full, #define data
                 method="REML") #estimate variances using REML
MVfull

#t-tests of each covariate #
MVfull.coef <- coef_test(MVfull,#estimation model above
                         cluster=full$programID, #define cluster IDs
                         vcov = "CR2") #estimation method (CR2 is best)
MVfull.coef

#meta-regression for each outcomes
formula <- reformulate(termlabels = c("Outcome_Category", centered, "-1"))

V_list <- impute_covariance_matrix(vi = full$var,  #known correlation vector
                                   cluster = full$programID, #ProgramID
                                   r = 0.70) #assumed correlation 

MVoutcome <- rma.mv(yi=full$ES, #effect size
                 V = V_list, #variance (tHIS IS WHAt CHANGES FROM HEmodel)
                 mods = formula, #ADD COVS HERE
                 random = ~1 | programID/ESId, #nesting structure
                 test= "t", #use t-tests
                 data=full, #define data
                 method="REML") #estimate variances using REML
MVoutcome
summary(MVoutcome)

#t-tests of each covariate #
MVoutcome.coef <- coef_test(MVoutcome,#estimation model above
                         cluster=full$programID, #define cluster IDs
                         vcov = "CR2") #estimation method (CR2 is best)
MVoutcome.coef


# Forest plot
#library(robumeta)

#setwd("C:/Users/wangj/OneDrive - Johns Hopkins/Conferences/SRCD 2025")

# Run intercept only model.
# MV_intercept <- robu(formula = ES ~ 1, data = full[which(is.na(full$var)==FALSE),], 
#                    studynum = programID, var.eff.size = var, 
#                    rho = 0.7, small = TRUE)

# Create forest plot.
#png(filename = "forestplot.png", height = 45, width = 7.5, units = "in", res = 100)
#forest.robu(MV_intercept, es.lab = "ESId", study.lab = "programID")
#dev.off()


#################################################################################
# Calculating Marginal Means
#################################################################################
# re-run model for each moderator to get marginal means for each #

# set up table to store results
means <- data.frame(moderator = character(0), group = character(0), beta = numeric(0), SE = numeric(0), 
                    tstat = numeric(0), df_Satt = numeric(0), p_Satt = numeric(0))

### moderators for means need to be factors...so if true binary, just recode the dummy, if multi-level, need to replace with a multi-level factor variable instead of multiple dummies.

# # make factor moderators for means
full$LargeSample.f <- as.factor(ifelse(full$TotalN>=300, "Large Sample", "Small Sample"))
full$Duration.f <- as.factor(ifelse(full$duration==2, "Long Duration", "Short Duration"))
full$published.f <- as.factor(ifelse(full$Journal==1, "Peer Reviewed", "Report"))
full$Universal.f <- as.factor(ifelse(full$uni_target==1, "Universal", "Targeted"))

full$Personnal.f <- NA
full$Personnal.f[which(full$tch_report==1)] <- "Teacher-report"
full$Personnal.f[which(full$prt_report==1)] <- "Parent-report"
full$Personnal.f[which(full$chd_report==1)] <- "Child-report"
full$Personnal.f[which(full$ind_obs==1)] <- "Independent Observer"
full$Personnal.f[which(full$researcher_report==1)] <- "Researcher-report"
full$Personnal.f[which(full$standardized==1)] <- "Standardized test"

full$Studydesign.f <- NA
full$Studydesign.f[which(full$Studydesign==1)] <- "Student Quasi-Experimental"
full$Studydesign.f[which(full$Studydesign==2)] <- "Cluster Quasi-Experimental"
full$Studydesign.f[which(full$Studydesign==3)] <- "Student Randomized"
full$Studydesign.f[which(full$Studydesign==4)] <- "Cluster Randomized"

mods <- c("LargeSample.f", "Duration.f", "published.f",
          "Universal.f", "Personnal.f",
          "Studydesign.f","Outcome_Category")

#full model
V_list <- impute_covariance_matrix(vi = full$var,  #known correlation vector
                                   cluster = full$programID, #ProgramID
                                   r = 0.70) #assumed correlation 

for(i in 1:length(mods)){
  # i <- 7
  formula <- reformulate(termlabels = c(mods[i], centered, "-1"))   # Worth knowing - if you duplicate terms, it keeps the first one
  mod_means <- rma.mv(yi=full$ES, #effect size
                      V = V_list, #variance (tHIS IS WHAt CHANGES FROM HEmodel)
                      mods = formula, #ADD COVS HERE
                      random = ~1 | programID/ESId, #nesting structure
                      test= "t", #use t-tests
                      data=full, #define data
                      method="REML") #estimate variances using REML
  
  coef_mod_means <- as.data.frame(coef_test(mod_means,#estimation model above
                                            cluster=full$programID, #define cluster IDs
                                            vcov = "CR2")) #estimation method (CR2 is best)
  
  # limit to relevant rows (the means you are interested in)
  coef_mod_means$moderator <- mods[i]
  coef_mod_means$group <- rownames(coef_mod_means)
  rownames(coef_mod_means) <- c()
  coef_mod_means <- subset(coef_mod_means, substr(start = 1, stop = nchar(mods[i]), x = coef_mod_means$group)== mods[i])
  coef_mod_means$group <- substr(x = coef_mod_means$group, start = nchar(mods[i])+1, stop = nchar(coef_mod_means$group))
  means <- dplyr::bind_rows(means, coef_mod_means)
}
means


#publication bias , selection modeling
MVfull_y <- MVfull$ES
MVfull_v <- MVfull$var
weightfunct(effect = full$ES, v = full$var)
weightfunct(full$ES, full$var, steps = c(.05, 1))



#################################################################################
# Percent Variance Explained
#################################################################################
##% explained by model
# Variation of first (smaller) model
tot_var_null <- sum(MVnull$sigma2) #total variation in effect sizes (omega2 + tau2)

# Variation of second (larger) model
tot_var_covs <- sum(MVfull$sigma2) #total variation in effect sizes after covariates

# difference between the two
perc_var_explained <- 100*(1-tot_var_covs/tot_var_null)
print(perc_var_explained) #R2 for model 

#################################################################################
# Heterogeneity
#################################################################################
# 95% prediction intervals
print(PI_upper <- round(MVfull$b[1] + (1.96*sqrt(MVfull$sigma2[1] + MVfull$sigma2[2])), 2))
print(PI_lower <- round(MVfull$b[1] - (1.96*sqrt(MVfull$sigma2[1] + MVfull$sigma2[2])), 2))


#################################################################################
# Descriptives Table
#################################################################################
table_study_full$Category <- row.names(table_study_full)
rownames(table_study_full) <- c()
table_study_full <- table_study_full[c("Category", "level", "Overall")]
table_study_full$Category[which(substr(table_study_full$Category, 1, 1)=="X")] <- NA
table_study_full$Category <- gsub(pattern = "\\.", replacement = "", x = table_study_full$Category)
table_study_full$Category[which(table_study_full$Category=="n")] <- "Total Studies"
table_study_full$Category[which(table_study_full$Category=="PubType")] <- "Publication Type"
table_study_full$level[which(table_study_full$level=="1")] <- "Yes"
table_study_full$level[which(table_study_full$level=="0")] <- "No"
# fill in blank columns (to improve merged cells later)
for(i in 1:length(table_study_full$Category)) {
  if(is.na(table_study_full$Category[i])) {
    table_study_full$Category[i] <- table_study_full$Category[i-1]
  }
}

table_outcome_full$Category <- row.names(table_outcome_full)
rownames(table_outcome_full) <- c()
table_outcome_full <- table_outcome_full[c("Category", "level", "Overall")]
table_outcome_full$Category[which(substr(table_outcome_full$Category, 1, 1)=="X")] <- NA
table_outcome_full$Category <- gsub(pattern = "\\.", replacement = "", x = table_outcome_full$Category)
table_outcome_full$Category[which(table_outcome_full$Category=="n")] <- "Total Effect Sizes"
table_outcome_full$level[which(table_outcome_full$level=="1")] <- "Yes"
table_outcome_full$level[which(table_outcome_full$level=="0")] <- "No"
# fill in blank columns (to improve merged cells later)
for(i in 1:length(table_outcome_full$Category)) {
  if(is.na(table_outcome_full$Category[i])) {
    table_outcome_full$Category[i] <- table_outcome_full$Category[i-1]
  }
}

# MetaRegression Table

# Marginal Means Table
means <- plyr::rename(means, c("tstat" = "t", "p_Satt" = "p", "df_Satt" = "df", "beta" = "ES"))

#########################
#########################
#########################
# add in table of k's and n's
for(i in 1:length(mods)) {
  # i <- 3
  mod <- mods[i]
  temp <- as.data.frame(table(full[mod]))
  temp$moderator <- mod
  names(temp)[1] <- "group"
  temp <- plyr::rename(temp, c("Freq" = "n"))
  
  temp2 <- unique(full[c("programID", mod)])
  temp2 <- as.data.frame(table(temp2[mod]))
  temp2$moderator <- mod
  names(temp2)[1] <- "group"
  temp2 <- plyr::rename(temp2, c("Freq" = "k"))
  
  temp <- merge(x = temp, y = temp2, by = c("moderator", "group"), all = TRUE)
  
  if(exists("counts")==TRUE) {
    counts <- bind_rows(counts, temp)
  }
  if(exists("counts")==FALSE) {
    counts <-temp
  }
  
  rm(temp, temp2)
}
means$order <- as.numeric(rownames(means))
means <- merge(x = counts, y = means, by = c("moderator", "group"), all = TRUE)
means <- means[order(means$order),]
means <- means[c("moderator", "group", "k", "n", "ES", "SE", "t", "df", "p")]


#################################################################################
# Saving Output   
#################################################################################
myreport<-read_docx()

# Descriptives Table
myreport <- body_add_par(x = myreport, value = "Table: Descriptive Statistics", style = "Normal")
descriptives_study <- flextable(head(table_study_full, n=nrow(table_study_full)))
descriptives_study <- add_header_lines(descriptives_study, values = c("Study Level"), top = FALSE)
descriptives_study <- theme_vanilla(descriptives_study)
descriptives_study <- merge_v(descriptives_study, j = c("Category"))
descriptives_study <- set_table_properties(descriptives_study, width = 1, layout = "autofit")
myreport <- body_add_flextable(x = myreport, descriptives_study, keepnext = FALSE)
myreport <- body_add_par(x = myreport, value = " ", style = "Normal")

descriptives_outcome <- flextable(head(table_outcome_full, n=nrow(table_outcome_full)))
descriptives_outcome <- delete_part(descriptives_outcome, part = "header")
descriptives_outcome <- add_header_lines(descriptives_outcome, values = c("Outcome Level"))
descriptives_outcome <- theme_vanilla(descriptives_outcome)
descriptives_outcome <- merge_v(descriptives_outcome, j = c("Category"))
descriptives_outcome <- set_table_properties(descriptives_outcome, width = 1, layout = "autofit")
myreport <- body_add_flextable(x = myreport, descriptives_outcome, keepnext = FALSE)
myreport <- body_add_par(x = myreport, value = "", style = "Normal")

# MetaRegression Table
#Null Models
model_null <- flextable(head(MVnull.coef, n=nrow(MVnull.coef)))
colkeys <- c("beta", "SE", "tstat", "df_Satt")
model_null <- colformat_double(model_null,  j = colkeys, digits = 2)
model_null <- colformat_double(model_null,  j = c("p_Satt"), digits = 3)
#model_null <- autofit(model_null)
model_null <- add_header_lines(model_null, values = c("Null Model"), top = FALSE)
model_null <- theme_vanilla(model_null)
model_null <- set_table_properties(model_null, width = 1, layout = "autofit")

myreport <- body_add_par(x = myreport, value = "Table: Model Results", style = "Normal")
myreport <- body_add_flextable(x = myreport, model_null, keepnext = FALSE)
#myreport <- body_add_par(x = myreport, value = "", style = "Normal")


#Full Model
model_full <- flextable(head(MVfull.coef, n=nrow(MVfull.coef)))
model_full <- colformat_double(model_full,  j = colkeys, digits = 2)
model_full <- colformat_double(model_full,  j = c("p_Satt"), digits = 3)
#model_full <- autofit(model_full)
model_full <- delete_part(model_full, part = "header")
model_full <- add_header_lines(model_full, values = c("Meta-Regression"))
model_full <- theme_vanilla(model_full)
model_full <- set_table_properties(model_full, width = 1, layout = "autofit")

myreport <- body_add_flextable(x = myreport, model_full, keepnext = FALSE)
myreport <- body_add_par(x = myreport, value = "", style = "Normal")

##meta-regression for each outcome
model_outcome <- flextable(head(MVoutcome.coef, n=nrow(MVoutcome.coef)))
model_outcome <- colformat_double(model_outcome,  j = colkeys, digits = 2)
model_outcome <- colformat_double(model_outcome,  j = c("p_Satt"), digits = 3)

model_outcome <- delete_part(model_outcome, part = "header")
model_outcome <- add_header_lines(model_outcome, values = c("Meta-Regression_Each outcome"))
model_outcome <- theme_vanilla(model_outcome)
model_outcome <- set_table_properties(model_outcome, width = 1, layout = "autofit")

myreport <- body_add_flextable(x = myreport, model_outcome, keepnext = FALSE)
myreport <- body_add_par(x = myreport, value = "", style = "Normal")

# Marginal Means Table
marginalmeans <- flextable(head(means, n=nrow(means)))
colkeys <- c("moderator", "group", "k", "n", "ES", "SE", "t", "df", "p")
marginalmeans <- colformat_double(marginalmeans,  j = colkeys, digits = 2)
marginalmeans <- colformat_double(marginalmeans,  j = c("p"), digits = 3)
rm(colkeys)
marginalmeans <- theme_vanilla(marginalmeans)
marginalmeans <- merge_v(marginalmeans, j = c("moderator"))
tablenote <- c("Note. k=number of studies; n = number of outcomes; ES=effect size; SE=standard error; df=degrees of freedom")
marginalmeans <- add_footer_lines(marginalmeans, tablenote, )
marginalmeans <- set_table_properties(marginalmeans, width = 1, layout = "autofit")

myreport <- body_add_par(x = myreport, value = "Table: Marginal Means", style = "Normal")
myreport <- body_add_flextable(x = myreport, marginalmeans, keepnext = FALSE)

# Heterogeneity Assessment
# 95% PI
myreport <- body_add_par(x = myreport, value = paste("95% PI: ", PI_lower, " to ", PI_upper, sep = ""), style = "Normal")
myreport <- body_add_par(x = myreport, value = "", style = "Normal")

# Percent variance explained
myreport <- body_add_par(x = myreport, value = paste("Percent Variance Explained: ", round(perc_var_explained, 2), "%", sep = ""), style = "Normal")
myreport <- body_add_par(x = myreport, value = "", style = "Normal")

# Write to word doc
file = paste("TableResultsFull.docx", sep = "")
print(myreport, file)

#total participant
ParticipantN<-full %>% group_by(programID)%>% filter(TotalN==max(TotalN))
ParticipantN<-ParticipantN %>% distinct(programID, TotalN,.keep_all=TRUE)
participantN<-sum(ParticipantN$TotalN)
participantN

##########################################################################
#Academic Functioning Meta-Regression
##########################################################################
setwd("C:/Users/wangj/OneDrive - Johns Hopkins/Conferences/SRCD 2025")
rm(list=ls(all=TRUE))
acad<-readRDS("academic functioning.rds")

#calculate standard errors
acad$se<-sqrt((acad$TotalN/(acad$TxN*acad$CtrlN))+((acad$ES*acad$ES)/(2*(acad$TxN+acad$CtrlN))))

#calculate variance
acad$var<-acad$se*acad$se

#meta-regression for acad
##null
V_list <- impute_covariance_matrix(vi = acad$var,  #known correlation vector
                                   cluster = acad$programID, #ProgramID
                                   r = 0.70) #assumed correlation 

acadnull <- rma.mv(yi=acad$ES, #effect size
                   V = V_list, #variance (tHIS IS WHAt CHANGES FROM HEmodel)
                   random = ~1 | programID/ESId, #nesting structure
                   test= "t", #use t-tests
                   data=acad, #define data
                   method="REML") #estimate variances using REML
acadnull
#t-tests of each covariate #
acad_coef_null <- coef_test(acadnull,#estimation model above
                            cluster=acad$programID, #define cluster IDs
                            vcov = "CR2") #estimation method (CR2 is best)

acad_coef_null

##full
vars <-c("LargeSample", "SmallSample", "LongDuration", "ShortDuration", "randomized", "quasi",
         "clustered", "individual", "targeted", "universal", "published","unpublished", "dv_pp", "dv_ew", "dv_sr",
         "dv_pb", "dv_af", "tch_report", "prt_report", "chd_report", "ind_obs", "standardized", "researcher_report")

centered<-paste(vars,".c", sep="")

acad[centered]<-as.data.frame(lapply(acad[vars],function(x) x-mean(x)))

formula <- reformulate(termlabels = c(centered))

V_list <- impute_covariance_matrix(vi = acad$var,  #known correlation vector
                                   cluster = acad$programID, #ProgramID
                                   r = 0.70) #assumed correlation 

MVacad <- rma.mv(yi=acad$ES, #effect size
                 V = V_list, #variance (tHIS IS WHAt CHANGES FROM HEmodel)
                 mods = formula, #ADD COVS HERE
                 random = ~1 | programID/ESId, #nesting structure
                 test= "t", #use t-tests
                 data=acad, #define data
                 method="REML") #estimate variances using REML
MVacad

#t-tests of each covariate #
MVacad.coef <- coef_test(MVacad,#estimation model above
                         cluster=acad$programID, #define cluster IDs
                         vcov = "CR2") #estimation method (CR2 is best)
MVacad.coef

#academic functioning_marginal means

means <- data.frame(moderator = character(0), group = character(0), beta = numeric(0), SE = numeric(0), 
                        tstat = numeric(0), df_Satt = numeric(0), p_Satt = numeric(0))

## make factor moderators for means
acad$LargeSample.f <- as.factor(ifelse(acad$TotalN>=300, "Large Sample", "Small Sample"))
acad$Duration.f <- as.factor(ifelse(acad$duration==2, "Long Duration", "Short Duration"))
acad$published.f <- as.factor(ifelse(acad$Journal==1, "Peer Reviewed", "Report"))
acad$Universal.f <- as.factor(ifelse(acad$uni_target==1, "Universal", "Targeted"))

acad$Personnal.f <- NA
acad$Personnal.f[which(acad$tch_report==1)] <- "Teacher-report"
acad$Personnal.f[which(acad$prt_report==1)] <- "Parent-report"
acad$Personnal.f[which(acad$chd_report==1)] <- "Child-report"
acad$Personnal.f[which(acad$ind_obs==1)] <- "Independent Observer"
acad$Personnal.f[which(acad$researcher_report==1)] <- "Researcher-report"
acad$Personnal.f[which(acad$standardized==1)] <- "Standardized test"

acad$Studydesign.f <- NA
acad$Studydesign.f[which(acad$Studydesign==1)] <- "Student Quasi-Experimental"
acad$Studydesign.f[which(acad$Studydesign==2)] <- "Cluster Quasi-Experimental"
acad$Studydesign.f[which(acad$Studydesign==3)] <- "Student Randomized"
acad$Studydesign.f[which(acad$Studydesign==4)] <- "Cluster Randomized"

mods <- c("LargeSample.f", "Duration.f", "published.f",
          "Universal.f", "Personnal.f",
          "Studydesign.f")

V_list <- impute_covariance_matrix(vi = acad$var,  #known correlation vector
                                   cluster = acad$programID, #ProgramID
                                   r = 0.70) #assumed correlation 

for(i in 1:length(mods)){
  # i <- 7
  formula <- reformulate(termlabels = c(mods[i], centered, "-1"))   # Worth knowing - if you duplicate terms, it keeps the first one
  
  mod_means <- rma.mv(yi=acad$ES, #effect size
                          V = V_list, #variance (tHIS IS WHAt CHANGES FROM HEmodel)
                          mods = formula, #ADD COVS HERE
                          random = ~1 | programID/ESId, #nesting structure
                          test= "t", #use t-tests
                          data=acad, #define data
                          method="REML") #estimate variances using REML
  
  coef_mod_means <- as.data.frame(coef_test(mod_means,#estimation model above
                                                cluster=acad$programID, #define cluster IDs
                                                vcov = "CR2")) #estimation method (CR2 is best)
  
  # limit to relevant rows (the means you are interested in)
  coef_mod_means$moderator <- mods[i]
  coef_mod_means$group <- rownames(coef_mod_means)
  rownames(coef_mod_means) <- c()
  coef_mod_means <- subset(coef_mod_means, substr(start = 1, stop = nchar(mods[i]), x = coef_mod_means$group)== mods[i])
  coef_mod_means$group <- substr(x = coef_mod_means$group, start = nchar(mods[i])+1, stop = nchar(coef_mod_means$group))
  means <- dplyr::bind_rows(means, coef_mod_means)
}
means

##acad marginal table
means <- plyr::rename(means, c("tstat" = "t", "p_Satt" = "p", "df_Satt" = "df", "beta" = "ES"))

# add in table of k's and n's
for(i in 1:length(mods)) {
  # i <- 3
  mod <- mods[i]
  temp <- as.data.frame(table(acad[mod]))
  temp$moderator <- mod
  names(temp)[1] <- "group"
  temp <- plyr::rename(temp, c("Freq" = "n"))
  
  temp2 <- unique(acad[c("programID", mod)])
  temp2 <- as.data.frame(table(temp2[mod]))
  temp2$moderator <- mod
  names(temp2)[1] <- "group"
  temp2 <- plyr::rename(temp2, c("Freq" = "k"))
  
  temp <- merge(x = temp, y = temp2, by = c("moderator", "group"), all = TRUE)
  
  if(exists("counts")==TRUE) {
    counts <- bind_rows(counts, temp)
  }
  if(exists("counts")==FALSE) {
    counts <-temp
  }
  
  rm(temp, temp2)
}
means$order <- as.numeric(rownames(means))
means <- merge(x = counts, y = means, by = c("moderator", "group"), all = TRUE)
means <- means[order(means$order),]
means <- means[c("moderator", "group", "k", "n", "ES", "SE", "t", "df", "p")]

#################################################################################
# Saving Output   
#################################################################################
myreport<-read_docx()

# MetaRegression Table
#Null Models
model_null <- flextable(head(acad_coef_null, n=nrow(acad_coef_null)))
colkeys <- c("beta", "SE", "tstat", "df_Satt")
model_null <- colformat_double(model_null,  j = colkeys, digits = 2)
model_null <- colformat_double(model_null,  j = c("p_Satt"), digits = 3)
#model_null <- autofit(model_null)
model_null <- add_header_lines(model_null, values = c("Null Model"), top = FALSE)
model_null <- theme_vanilla(model_null)
model_null <- set_table_properties(model_null, width = 1, layout = "autofit")

myreport <- body_add_par(x = myreport, value = "Table: Model Results", style = "Normal")
myreport <- body_add_flextable(x = myreport, model_null, keepnext = FALSE)

model_acad <- flextable(head(MVacad.coef, n=nrow(MVacad.coef)))
model_acad <- colformat_double(model_acad,  j = colkeys, digits = 2)
model_acad <- colformat_double(model_acad,  j = c("p_Satt"), digits = 3)
#model_acad <- autofit(model_acad)
model_acad <- delete_part(model_acad, part = "header")
model_acad <- add_header_lines(model_acad, values = c("Meta-Regression_Academic Functioning"))
model_acad <- theme_vanilla(model_acad)
model_acad <- set_table_properties(model_acad, width = 1, layout = "autofit")

myreport <- body_add_flextable(x = myreport, model_acad, keepnext = FALSE)
myreport <- body_add_par(x = myreport, value = "", style = "Normal")

# Marginal Means Table
marginalacadmeans <- flextable(head(means, n=nrow(means)))
colkeys <- c("moderator", "group", "k", "n", "ES", "SE", "t", "df", "p")
marginalacadmeans <- colformat_double(marginalacadmeans,  j = colkeys, digits = 2)
marginalacadmeans <- colformat_double(marginalacadmeans,  j = c("p"), digits = 3)
rm(colkeys)
marginalacadmeans <- theme_vanilla(marginalacadmeans)
marginalacadmeans <- merge_v(marginalacadmeans, j = c("moderator"))
tablenote <- c("Note. k=number of studies; n = number of outcomes; ES=effect size; SE=standard error; df=degrees of freedom")
marginalacadmeans <- add_footer_lines(marginalacadmeans, tablenote, )
marginalacadmeans <- set_table_properties(marginalacadmeans, width = 1, layout = "autofit")

myreport <- body_add_par(x = myreport, value = "Table: acad Marginal Means", style = "Normal")
myreport <- body_add_flextable(x = myreport, marginalacadmeans, keepnext = FALSE)

# Write to word doc
file = paste("TableResultsacad.docx", sep = "")
print(myreport, file)

##########################################################################
#Problem Behaviors Meta-Regression
##########################################################################
setwd("C:/Users/wangj/OneDrive - Johns Hopkins/Conferences/SRCD 2025")
rm(list=ls(all=TRUE))
prob<-readRDS("problem behaviors.rds")

#calculate standard errors
prob$se<-sqrt((prob$TotalN/(prob$TxN*prob$CtrlN))+((prob$ES*prob$ES)/(2*(prob$TxN+prob$CtrlN))))

#calculate variance
prob$var<-prob$se*prob$se

#meta-regression for prob
##null
V_list <- impute_covariance_matrix(vi = prob$var,  #known correlation vector
                                   cluster = prob$programID, #ProgramID
                                   r = 0.70) #assumed correlation 
probnull <- rma.mv(yi=prob$ES, #effect size
                   V = V_list, #variance (tHIS IS WHAt CHANGES FROM HEmodel)
                   random = ~1 | programID/ESId, #nesting structure
                   test= "t", #use t-tests
                   data=prob, #define data
                   method="REML") #estimate variances using REML
probnull
#t-tests of each covariate #
prob_coef_null <- coef_test(probnull,#estimation model above
                            cluster=prob$programID, #define cluster IDs
                            vcov = "CR2") #estimation method (CR2 is best)

prob_coef_null


##full
vars <-c("LargeSample", "SmallSample", "LongDuration", "ShortDuration", "randomized", "quasi",
         "clustered", "individual", "targeted", "universal", "published","unpublished", "dv_pp", "dv_ew", "dv_sr",
         "dv_pb", "dv_af", "tch_report", "prt_report", "chd_report", "ind_obs", "standardized", "researcher_report")

centered<-paste(vars,".c", sep="")

prob[centered]<-as.data.frame(lapply(prob[vars],function(x) x-mean(x)))

formula <- reformulate(termlabels = c(centered))

MVprob <- rma.mv(yi=prob$ES, #effect size
                 V = V_list, #variance (tHIS IS WHAt CHANGES FROM HEmodel)
                 mods = formula, #ADD COVS HERE
                 random = ~1 | programID/ESId, #nesting structure
                 test= "t", #use t-tests
                 data=prob, #define data
                 method="REML") #estimate variances using REML
MVprob

#t-tests of each covariate #
MVprob.coef <- coef_test(MVprob,#estimation model above
                         cluster=prob$programID, #define cluster IDs
                         vcov = "CR2") #estimation method (CR2 is best)
MVprob.coef

#prob_marginal means

means <- data.frame(moderator = character(0), group = character(0), beta = numeric(0), SE = numeric(0), 
                    tstat = numeric(0), df_Satt = numeric(0), p_Satt = numeric(0))


prob$LargeSample.f <- as.factor(ifelse(prob$TotalN>=300, "Large Sample", "Small Sample"))
prob$Duration.f <- as.factor(ifelse(prob$duration==2, "Long Duration", "Short Duration"))
prob$published.f <- as.factor(ifelse(prob$Journal==1, "Peer Reviewed", "Report"))
prob$Universal.f <- as.factor(ifelse(prob$uni_target==1, "Universal", "Targeted"))

prob$Personnal.f <- NA
prob$Personnal.f[which(prob$tch_report==1)] <- "Teacher-report"
prob$Personnal.f[which(prob$prt_report==1)] <- "Parent-report"
prob$Personnal.f[which(prob$chd_report==1)] <- "Child-report"
prob$Personnal.f[which(prob$ind_obs==1)] <- "Independent Observer"
prob$Personnal.f[which(prob$researcher_report==1)] <- "Researcher-report"
prob$Personnal.f[which(prob$standardized==1)] <- "Standardized test"

prob$Studydesign.f <- NA
prob$Studydesign.f[which(prob$Studydesign==1)] <- "Student Quasi-Experimental"
prob$Studydesign.f[which(prob$Studydesign==2)] <- "Cluster Quasi-Experimental"
prob$Studydesign.f[which(prob$Studydesign==3)] <- "Student Randomized"
prob$Studydesign.f[which(prob$Studydesign==4)] <- "Cluster Randomized"


mods <- c("LargeSample.f", "Duration.f", "published.f",
          "Universal.f", "Personnal.f",
          "Studydesign.f")

V_list <- impute_covariance_matrix(vi = prob$var,  #known correlation vector
                                   cluster = prob$programID, #ProgramID
                                   r = 0.70) #assumed correlation 

for(i in 1:length(mods)){
  # i <- 7
  formula <- reformulate(termlabels = c(mods[i], centered, "-1"))   # Worth knowing - if you duplicate terms, it keeps the first one
  mod_means <- rma.mv(yi=prob$ES, #effect size
                          V = V_list, #variance (tHIS IS WHAt CHANGES FROM HEmodel)
                          mods = formula, #ADD COVS HERE
                          random = ~1 | programID/ESId, #nesting structure
                          test= "t", #use t-tests
                          data=prob, #define data
                          method="REML") #estimate variances using REML
  
  coef_mod_means <- as.data.frame(coef_test(mod_means,#estimation model above
                                                cluster=prob$programID, #define cluster IDs
                                                vcov = "CR2")) #estimation method (CR2 is best)
  
  # limit to relevant rows (the means you are interested in)
  coef_mod_means$moderator <- mods[i]
  coef_mod_means$group <- rownames(coef_mod_means)
  rownames(coef_mod_means) <- c()
  coef_mod_means <- subset(coef_mod_means, substr(start = 1, stop = nchar(mods[i]), x = coef_mod_means$group)== mods[i])
  coef_mod_means$group <- substr(x = coef_mod_means$group, start = nchar(mods[i])+1, stop = nchar(coef_mod_means$group))
  means <- dplyr::bind_rows(means, coef_mod_means)
}
means


##prob marginal table
means <- plyr::rename(means, c("tstat" = "t", "p_Satt" = "p", "df_Satt" = "df", "beta" = "ES"))

# add in table of k's and n's
for(i in 1:length(mods)) {
  # i <- 3
  mod <- mods[i]
  temp <- as.data.frame(table(prob[mod]))
  temp$moderator <- mod
  names(temp)[1] <- "group"
  temp <- plyr::rename(temp, c("Freq" = "n"))
  
  temp2 <- unique(prob[c("programID", mod)])
  temp2 <- as.data.frame(table(temp2[mod]))
  temp2$moderator <- mod
  names(temp2)[1] <- "group"
  temp2 <- plyr::rename(temp2, c("Freq" = "k"))
  
  temp <- merge(x = temp, y = temp2, by = c("moderator", "group"), all = TRUE)
  
  if(exists("counts")==TRUE) {
    counts <- bind_rows(counts, temp)
  }
  if(exists("counts")==FALSE) {
    counts <-temp
  }
  
  rm(temp, temp2)
}
means$order <- as.numeric(rownames(means))
means <- merge(x = counts, y = means, by = c("moderator", "group"), all = TRUE)
means <- means[order(means$order),]
means <- means[c("moderator", "group", "k", "n", "ES", "SE", "t", "df", "p")]

#################################################################################
# Saving Output   
#################################################################################
myreport<-read_docx()

# MetaRegression Table
#Null Models
model_null <- flextable(head(prob_coef_null, n=nrow(prob_coef_null)))
colkeys <- c("beta", "SE", "tstat", "df_Satt")
model_null <- colformat_double(model_null,  j = colkeys, digits = 2)
model_null <- colformat_double(model_null,  j = c("p_Satt"), digits = 3)
#model_null <- autofit(model_null)
model_null <- add_header_lines(model_null, values = c("Null Model"), top = FALSE)
model_null <- theme_vanilla(model_null)
model_null <- set_table_properties(model_null, width = 1, layout = "autofit")

myreport <- body_add_par(x = myreport, value = "Table: Model Results", style = "Normal")
myreport <- body_add_flextable(x = myreport, model_null, keepnext = FALSE)

model_prob <- flextable(head(MVprob.coef, n=nrow(MVprob.coef)))
model_prob<- colformat_double(model_prob,  j = colkeys, digits = 2)
model_prob <- colformat_double(model_prob,  j = c("p_Satt"), digits = 3)

model_prob <- delete_part(model_prob, part = "header")
model_prob <- add_header_lines(model_prob, values = c("Meta-Regression_Problem Behavior"))
model_prob <- theme_vanilla(model_prob)
model_prob <- set_table_properties(model_prob, width = 1, layout = "autofit")

myreport <- body_add_flextable(x = myreport, model_prob, keepnext = FALSE)
myreport <- body_add_par(x = myreport, value = "", style = "Normal")

# Marginal Means Table
marginalmeans <- flextable(head(means, n=nrow(means)))
colkeys <- c("moderator", "group", "k", "n", "ES", "SE", "t", "df", "p")
marginalmeans <- colformat_double(marginalmeans,  j = colkeys, digits = 2)
marginalmeans <- colformat_double(marginalmeans,  j = c("p"), digits = 3)
rm(colkeys)
marginalmeans <- theme_vanilla(marginalmeans)
marginalmeans <- merge_v(marginalmeans, j = c("moderator"))
tablenote <- c("Note. k=number of studies; n = number of outcomes; ES=effect size; SE=standard error; df=degrees of freedom")
marginalmeans <- add_footer_lines(marginalmeans, tablenote, )
marginalmeans <- set_table_properties(marginalmeans, width = 1, layout = "autofit")

myreport <- body_add_par(x = myreport, value = "Table: prob Marginal Means", style = "Normal")
myreport <- body_add_flextable(x = myreport, marginalmeans, keepnext = FALSE)

# Write to word doc
file = paste("TableResultsprob.docx", sep = "")
print(myreport, file)



##########################################################################
#Positive Teacher Practice Meta-Regression
##########################################################################
setwd("C:/Users/wangj/OneDrive - Johns Hopkins/Conferences/SRCD 2025")
rm(list=ls(all=TRUE))
full<-readRDS("positive teacher practice.rds")

#calculate standard errors
full$se<-sqrt((full$TotalN/(full$TxN*full$CtrlN))+((full$ES*full$ES)/(2*(full$TxN+full$CtrlN))))

#calculate variance
full$var<-full$se*full$se

#meta-regression for prob
##null
V_list <- impute_covariance_matrix(vi = full$var,  #known correlation vector
                                   cluster = full$programID, #ProgramID
                                   r = 0.70) #assumed correlation 
fullnull <- rma.mv(yi=full$ES, #effect size
                   V = V_list, #variance (tHIS IS WHAt CHANGES FROM HEmodel)
                   random = ~1 | programID/ESId, #nesting structure
                   test= "t", #use t-tests
                   data=full, #define data
                   method="REML") #estimate variances using REML
fullnull
#t-tests of each covariate #
full_coef_null <- coef_test(fullnull,#estimation model above
                            cluster=full$programID, #define cluster IDs
                            vcov = "CR2") #estimation method (CR2 is best)

full_coef_null


##full
vars <-c("LargeSample", "SmallSample", "LongDuration", "ShortDuration", "randomized", "quasi",
         "clustered", "individual", "targeted", "universal", "published","unpublished", "dv_pp", "dv_ew", "dv_sr",
         "dv_pb", "dv_af", "tch_report", "prt_report", "chd_report", "ind_obs", "standardized", "researcher_report")

centered<-paste(vars,".c", sep="")

full[centered]<-as.data.frame(lapply(full[vars],function(x) x-mean(x)))

formula <- reformulate(termlabels = c(centered))

MVfull <- rma.mv(yi=full$ES, #effect size
                 V = V_list, #variance (tHIS IS WHAt CHANGES FROM HEmodel)
                 mods = formula, #ADD COVS HERE
                 random = ~1 | programID/ESId, #nesting structure
                 test= "t", #use t-tests
                 data=full, #define data
                 method="REML") #estimate variances using REML
MVfull

#t-tests of each covariate #
MVfull.coef <- coef_test(MVfull,#estimation model above
                         cluster=full$programID, #define cluster IDs
                         vcov = "CR2") #estimation method (CR2 is best)
MVfull.coef

#popr_marginal means

means <- data.frame(moderator = character(0), group = character(0), beta = numeric(0), SE = numeric(0), 
                    tstat = numeric(0), df_Satt = numeric(0), p_Satt = numeric(0))


full$LargeSample.f <- as.factor(ifelse(full$TotalN>=300, "Large Sample", "Small Sample"))
full$Duration.f <- as.factor(ifelse(full$duration==2, "Long Duration", "Short Duration"))
full$published.f <- as.factor(ifelse(full$Journal==1, "Peer Reviewed", "Report"))
full$Universal.f <- as.factor(ifelse(full$uni_target==1, "Universal", "Targeted"))

full$Personnal.f <- NA
full$Personnal.f[which(full$tch_report==1)] <- "Teacher-report"
full$Personnal.f[which(full$prt_report==1)] <- "Parent-report"
full$Personnal.f[which(full$chd_report==1)] <- "Child-report"
full$Personnal.f[which(full$ind_obs==1)] <- "Independent Observer"
full$Personnal.f[which(full$researcher_report==1)] <- "Researcher-report"
full$Personnal.f[which(full$standardized==1)] <- "Standardized test"

full$Studydesign.f <- NA
full$Studydesign.f[which(full$Studydesign==1)] <- "Student Quasi-Experimental"
full$Studydesign.f[which(full$Studydesign==2)] <- "Cluster Quasi-Experimental"
full$Studydesign.f[which(full$Studydesign==3)] <- "Student Randomized"
full$Studydesign.f[which(full$Studydesign==4)] <- "Cluster Randomized"


mods <- c("LargeSample.f", "Duration.f", "published.f",
          "Universal.f", "Personnal.f",
          "Studydesign.f")

V_list <- impute_covariance_matrix(vi = full$var,  #known correlation vector
                                   cluster = full$programID, #ProgramID
                                   r = 0.70) #assumed correlation 

for(i in 1:length(mods)){
  # i <- 7
  formula <- reformulate(termlabels = c(mods[i], centered, "-1"))   # Worth knowing - if you duplicate terms, it keeps the first one
  mod_means <- rma.mv(yi=full$ES, #effect size
                      V = V_list, #variance (tHIS IS WHAt CHANGES FROM HEmodel)
                      mods = formula, #ADD COVS HERE
                      random = ~1 | programID/ESId, #nesting structure
                      test= "t", #use t-tests
                      data=full, #define data
                      method="REML") #estimate variances using REML
  
  coef_mod_means <- as.data.frame(coef_test(mod_means,#estimation model above
                                            cluster=full$programID, #define cluster IDs
                                            vcov = "CR2")) #estimation method (CR2 is best)
  
  # limit to relevant rows (the means you are interested in)
  coef_mod_means$moderator <- mods[i]
  coef_mod_means$group <- rownames(coef_mod_means)
  rownames(coef_mod_means) <- c()
  coef_mod_means <- subset(coef_mod_means, substr(start = 1, stop = nchar(mods[i]), x = coef_mod_means$group)== mods[i])
  coef_mod_means$group <- substr(x = coef_mod_means$group, start = nchar(mods[i])+1, stop = nchar(coef_mod_means$group))
  means <- dplyr::bind_rows(means, coef_mod_means)
}
means


##popr marginal table
means <- plyr::rename(means, c("tstat" = "t", "p_Satt" = "p", "df_Satt" = "df", "beta" = "ES"))

# add in table of k's and n's
for(i in 1:length(mods)) {
  # i <- 3
  mod <- mods[i]
  temp <- as.data.frame(table(full[mod]))
  temp$moderator <- mod
  names(temp)[1] <- "group"
  temp <- plyr::rename(temp, c("Freq" = "n"))
  
  temp2 <- unique(full[c("programID", mod)])
  temp2 <- as.data.frame(table(temp2[mod]))
  temp2$moderator <- mod
  names(temp2)[1] <- "group"
  temp2 <- plyr::rename(temp2, c("Freq" = "k"))
  
  temp <- merge(x = temp, y = temp2, by = c("moderator", "group"), all = TRUE)
  
  if(exists("counts")==TRUE) {
    counts <- bind_rows(counts, temp)
  }
  if(exists("counts")==FALSE) {
    counts <-temp
  }
  
  rm(temp, temp2)
}
means$order <- as.numeric(rownames(means))
means <- merge(x = counts, y = means, by = c("moderator", "group"), all = TRUE)
means <- means[order(means$order),]
means <- means[c("moderator", "group", "k", "n", "ES", "SE", "t", "df", "p")]

#################################################################################
# Saving Output   
#################################################################################
myreport<-read_docx()

# MetaRegression Table
#Null Models
model_null <- flextable(head(full_coef_null, n=nrow(full_coef_null)))
colkeys <- c("beta", "SE", "tstat", "df_Satt")
model_null <- colformat_double(model_null,  j = colkeys, digits = 2)
model_null <- colformat_double(model_null,  j = c("p_Satt"), digits = 3)
#model_null <- autofit(model_null)
model_null <- add_header_lines(model_null, values = c("Null Model"), top = FALSE)
model_null <- theme_vanilla(model_null)
model_null <- set_table_properties(model_null, width = 1, layout = "autofit")

myreport <- body_add_par(x = myreport, value = "Table: Model Results", style = "Normal")
myreport <- body_add_flextable(x = myreport, model_null, keepnext = FALSE)

model_full <- flextable(head(MVfull.coef, n=nrow(MVfull.coef)))
model_full<- colformat_double(model_full,  j = colkeys, digits = 2)
model_full <- colformat_double(model_full,  j = c("p_Satt"), digits = 3)

model_full <- delete_part(model_full, part = "header")
model_full <- add_header_lines(model_full, values = c("Meta-Regression_Positive Teacher Practice"))
model_full <- theme_vanilla(model_full)
model_full <- set_table_properties(model_full, width = 1, layout = "autofit")

myreport <- body_add_flextable(x = myreport, model_full, keepnext = FALSE)
myreport <- body_add_par(x = myreport, value = "", style = "Normal")

# Marginal Means Table
marginalmeans <- flextable(head(means, n=nrow(means)))
colkeys <- c("moderator", "group", "k", "n", "ES", "SE", "t", "df", "p")
marginalmeans <- colformat_double(marginalmeans,  j = colkeys, digits = 2)
marginalmeans <- colformat_double(marginalmeans,  j = c("p"), digits = 3)
rm(colkeys)
marginalmeans <- theme_vanilla(marginalmeans)
marginalmeans <- merge_v(marginalmeans, j = c("moderator"))
tablenote <- c("Note. k=number of studies; n = number of outcomes; ES=effect size; SE=standard error; df=degrees of freedom")
marginalmeans <- add_footer_lines(marginalmeans, tablenote, )
marginalmeans <- set_table_properties(marginalmeans, width = 1, layout = "autofit")

myreport <- body_add_par(x = myreport, value = "Table: popr Marginal Means", style = "Normal")
myreport <- body_add_flextable(x = myreport, marginalmeans, keepnext = FALSE)

# Write to word doc
file = paste("TableResultspopr.docx", sep = "")
print(myreport, file)

##########################################################################
#Emotional Well-being Meta-Regression
##########################################################################
setwd("C:/Users/wangj/OneDrive - Johns Hopkins/Conferences/SRCD 2025")
rm(list=ls(all=TRUE))
full<-readRDS("emotional well-being.rds")

#calculate standard errors
full$se<-sqrt((full$TotalN/(full$TxN*full$CtrlN))+((full$ES*full$ES)/(2*(full$TxN+full$CtrlN))))

#calculate variance
full$var<-full$se*full$se

#meta-regression for prob
##null
V_list <- impute_covariance_matrix(vi = full$var,  #known correlation vector
                                   cluster = full$programID, #ProgramID
                                   r = 0.70) #assumed correlation 
fullnull <- rma.mv(yi=full$ES, #effect size
                   V = V_list, #variance (tHIS IS WHAt CHANGES FROM HEmodel)
                   random = ~1 | programID/ESId, #nesting structure
                   test= "t", #use t-tests
                   data=full, #define data
                   method="REML") #estimate variances using REML
fullnull
#t-tests of each covariate #
full_coef_null <- coef_test(fullnull,#estimation model above
                            cluster=full$programID, #define cluster IDs
                            vcov = "CR2") #estimation method (CR2 is best)

full_coef_null


##full
vars <-c("LargeSample", "SmallSample", "LongDuration", "ShortDuration", "randomized", "quasi",
         "clustered", "individual", "targeted", "universal", "published","unpublished", "dv_pp", "dv_ew", "dv_sr",
         "dv_pb", "dv_af", "tch_report", "prt_report", "chd_report", "ind_obs", "standardized", "researcher_report")

centered<-paste(vars,".c", sep="")

full[centered]<-as.data.frame(lapply(full[vars],function(x) x-mean(x)))

formula <- reformulate(termlabels = c(centered))

MVfull <- rma.mv(yi=full$ES, #effect size
                 V = V_list, #variance (tHIS IS WHAt CHANGES FROM HEmodel)
                 mods = formula, #ADD COVS HERE
                 random = ~1 | programID/ESId, #nesting structure
                 test= "t", #use t-tests
                 data=full, #define data
                 method="REML") #estimate variances using REML
MVfull

#t-tests of each covariate #
MVfull.coef <- coef_test(MVfull,#estimation model above
                         cluster=full$programID, #define cluster IDs
                         vcov = "CR2") #estimation method (CR2 is best)
MVfull.coef

#emow_marginal means

means <- data.frame(moderator = character(0), group = character(0), beta = numeric(0), SE = numeric(0), 
                    tstat = numeric(0), df_Satt = numeric(0), p_Satt = numeric(0))


full$LargeSample.f <- as.factor(ifelse(full$TotalN>=300, "Large Sample", "Small Sample"))
full$Duration.f <- as.factor(ifelse(full$duration==2, "Long Duration", "Short Duration"))
full$published.f <- as.factor(ifelse(full$Journal==1, "Peer Reviewed", "Report"))
full$Universal.f <- as.factor(ifelse(full$uni_target==1, "Universal", "Targeted"))

full$Personnal.f <- NA
full$Personnal.f[which(full$tch_report==1)] <- "Teacher-report"
full$Personnal.f[which(full$prt_report==1)] <- "Parent-report"
full$Personnal.f[which(full$chd_report==1)] <- "Child-report"
full$Personnal.f[which(full$ind_obs==1)] <- "Independent Observer"
full$Personnal.f[which(full$researcher_report==1)] <- "Researcher-report"
full$Personnal.f[which(full$standardized==1)] <- "Standardized test"

full$Studydesign.f <- NA
full$Studydesign.f[which(full$Studydesign==1)] <- "Student Quasi-Experimental"
full$Studydesign.f[which(full$Studydesign==2)] <- "Cluster Quasi-Experimental"
full$Studydesign.f[which(full$Studydesign==3)] <- "Student Randomized"
full$Studydesign.f[which(full$Studydesign==4)] <- "Cluster Randomized"


mods <- c("LargeSample.f", "Duration.f", "published.f",
          "Universal.f", "Personnal.f",
          "Studydesign.f")

V_list <- impute_covariance_matrix(vi = full$var,  #known correlation vector
                                   cluster = full$programID, #ProgramID
                                   r = 0.70) #assumed correlation 

for(i in 1:length(mods)){
  # i <- 7
  formula <- reformulate(termlabels = c(mods[i], centered, "-1"))   # Worth knowing - if you duplicate terms, it keeps the first one
  mod_means <- rma.mv(yi=full$ES, #effect size
                      V = V_list, #variance (tHIS IS WHAt CHANGES FROM HEmodel)
                      mods = formula, #ADD COVS HERE
                      random = ~1 | programID/ESId, #nesting structure
                      test= "t", #use t-tests
                      data=full, #define data
                      method="REML") #estimate variances using REML
  
  coef_mod_means <- as.data.frame(coef_test(mod_means,#estimation model above
                                            cluster=full$programID, #define cluster IDs
                                            vcov = "CR2")) #estimation method (CR2 is best)
  
  # limit to relevant rows (the means you are interested in)
  coef_mod_means$moderator <- mods[i]
  coef_mod_means$group <- rownames(coef_mod_means)
  rownames(coef_mod_means) <- c()
  coef_mod_means <- subset(coef_mod_means, substr(start = 1, stop = nchar(mods[i]), x = coef_mod_means$group)== mods[i])
  coef_mod_means$group <- substr(x = coef_mod_means$group, start = nchar(mods[i])+1, stop = nchar(coef_mod_means$group))
  means <- dplyr::bind_rows(means, coef_mod_means)
}
means


##popr marginal table
means <- plyr::rename(means, c("tstat" = "t", "p_Satt" = "p", "df_Satt" = "df", "beta" = "ES"))

# add in table of k's and n's
for(i in 1:length(mods)) {
  # i <- 3
  mod <- mods[i]
  temp <- as.data.frame(table(full[mod]))
  temp$moderator <- mod
  names(temp)[1] <- "group"
  temp <- plyr::rename(temp, c("Freq" = "n"))
  
  temp2 <- unique(full[c("programID", mod)])
  temp2 <- as.data.frame(table(temp2[mod]))
  temp2$moderator <- mod
  names(temp2)[1] <- "group"
  temp2 <- plyr::rename(temp2, c("Freq" = "k"))
  
  temp <- merge(x = temp, y = temp2, by = c("moderator", "group"), all = TRUE)
  
  if(exists("counts")==TRUE) {
    counts <- bind_rows(counts, temp)
  }
  if(exists("counts")==FALSE) {
    counts <-temp
  }
  
  rm(temp, temp2)
}
means$order <- as.numeric(rownames(means))
means <- merge(x = counts, y = means, by = c("moderator", "group"), all = TRUE)
means <- means[order(means$order),]
means <- means[c("moderator", "group", "k", "n", "ES", "SE", "t", "df", "p")]

#################################################################################
# Saving Output   
#################################################################################
myreport<-read_docx()

# MetaRegression Table
#Null Models
model_null <- flextable(head(full_coef_null, n=nrow(full_coef_null)))
colkeys <- c("beta", "SE", "tstat", "df_Satt")
model_null <- colformat_double(model_null,  j = colkeys, digits = 2)
model_null <- colformat_double(model_null,  j = c("p_Satt"), digits = 3)
#model_null <- autofit(model_null)
model_null <- add_header_lines(model_null, values = c("Null Model"), top = FALSE)
model_null <- theme_vanilla(model_null)
model_null <- set_table_properties(model_null, width = 1, layout = "autofit")

myreport <- body_add_par(x = myreport, value = "Table: Model Results", style = "Normal")
myreport <- body_add_flextable(x = myreport, model_null, keepnext = FALSE)

model_full <- flextable(head(MVfull.coef, n=nrow(MVfull.coef)))
model_full<- colformat_double(model_full,  j = colkeys, digits = 2)
model_full <- colformat_double(model_full,  j = c("p_Satt"), digits = 3)

model_full <- delete_part(model_full, part = "header")
model_full <- add_header_lines(model_full, values = c("Meta-Regression_Emotional Well-being"))
model_full <- theme_vanilla(model_full)
model_full <- set_table_properties(model_full, width = 1, layout = "autofit")

myreport <- body_add_flextable(x = myreport, model_full, keepnext = FALSE)
myreport <- body_add_par(x = myreport, value = "", style = "Normal")

# Marginal Means Table
marginalmeans <- flextable(head(means, n=nrow(means)))
colkeys <- c("moderator", "group", "k", "n", "ES", "SE", "t", "df", "p")
marginalmeans <- colformat_double(marginalmeans,  j = colkeys, digits = 2)
marginalmeans <- colformat_double(marginalmeans,  j = c("p"), digits = 3)
rm(colkeys)
marginalmeans <- theme_vanilla(marginalmeans)
marginalmeans <- merge_v(marginalmeans, j = c("moderator"))
tablenote <- c("Note. k=number of studies; n = number of outcomes; ES=effect size; SE=standard error; df=degrees of freedom")
marginalmeans <- add_footer_lines(marginalmeans, tablenote, )
marginalmeans <- set_table_properties(marginalmeans, width = 1, layout = "autofit")

myreport <- body_add_par(x = myreport, value = "Table: emow Marginal Means", style = "Normal")
myreport <- body_add_flextable(x = myreport, marginalmeans, keepnext = FALSE)

# Write to word doc
file = paste("TableResultsemow.docx", sep = "")
print(myreport, file)

##########################################################################
#Social Relationship Meta-Regression
##########################################################################
setwd("C:/Users/wangj/OneDrive - Johns Hopkins/Conferences/SRCD 2025")
rm(list=ls(all=TRUE))
full<-readRDS("social relationship.rds")

#calculate standard errors
full$se<-sqrt((full$TotalN/(full$TxN*full$CtrlN))+((full$ES*full$ES)/(2*(full$TxN+full$CtrlN))))

#calculate variance
full$var<-full$se*full$se

#meta-regression for prob
##null
V_list <- impute_covariance_matrix(vi = full$var,  #known correlation vector
                                   cluster = full$programID, #ProgramID
                                   r = 0.70) #assumed correlation 
fullnull <- rma.mv(yi=full$ES, #effect size
                   V = V_list, #variance (tHIS IS WHAt CHANGES FROM HEmodel)
                   random = ~1 | programID/ESId, #nesting structure
                   test= "t", #use t-tests
                   data=full, #define data
                   method="REML") #estimate variances using REML
fullnull
#t-tests of each covariate #
full_coef_null <- coef_test(fullnull,#estimation model above
                            cluster=full$programID, #define cluster IDs
                            vcov = "CR2") #estimation method (CR2 is best)

full_coef_null


##full
vars <-c("LargeSample", "SmallSample", "LongDuration", "ShortDuration", "randomized", "quasi",
         "clustered", "individual", "targeted", "universal", "published","unpublished", "dv_pp", "dv_ew", "dv_sr",
         "dv_pb", "dv_af", "tch_report", "prt_report", "chd_report", "ind_obs", "standardized", "researcher_report")

centered<-paste(vars,".c", sep="")

full[centered]<-as.data.frame(lapply(full[vars],function(x) x-mean(x)))

formula <- reformulate(termlabels = c(centered))

MVfull <- rma.mv(yi=full$ES, #effect size
                 V = V_list, #variance (tHIS IS WHAt CHANGES FROM HEmodel)
                 mods = formula, #ADD COVS HERE
                 random = ~1 | programID/ESId, #nesting structure
                 test= "t", #use t-tests
                 data=full, #define data
                 method="REML") #estimate variances using REML
MVfull

#t-tests of each covariate #
MVfull.coef <- coef_test(MVfull,#estimation model above
                         cluster=full$programID, #define cluster IDs
                         vcov = "CR2") #estimation method (CR2 is best)
MVfull.coef

#social_marginal means

means <- data.frame(moderator = character(0), group = character(0), beta = numeric(0), SE = numeric(0), 
                    tstat = numeric(0), df_Satt = numeric(0), p_Satt = numeric(0))


full$LargeSample.f <- as.factor(ifelse(full$TotalN>=300, "Large Sample", "Small Sample"))
full$Duration.f <- as.factor(ifelse(full$duration==2, "Long Duration", "Short Duration"))
full$published.f <- as.factor(ifelse(full$Journal==1, "Peer Reviewed", "Report"))
full$Universal.f <- as.factor(ifelse(full$uni_target==1, "Universal", "Targeted"))

full$Personnal.f <- NA
full$Personnal.f[which(full$tch_report==1)] <- "Teacher-report"
full$Personnal.f[which(full$prt_report==1)] <- "Parent-report"
full$Personnal.f[which(full$chd_report==1)] <- "Child-report"
full$Personnal.f[which(full$ind_obs==1)] <- "Independent Observer"
full$Personnal.f[which(full$researcher_report==1)] <- "Researcher-report"
full$Personnal.f[which(full$standardized==1)] <- "Standardized test"

full$Studydesign.f <- NA
full$Studydesign.f[which(full$Studydesign==1)] <- "Student Quasi-Experimental"
full$Studydesign.f[which(full$Studydesign==2)] <- "Cluster Quasi-Experimental"
full$Studydesign.f[which(full$Studydesign==3)] <- "Student Randomized"
full$Studydesign.f[which(full$Studydesign==4)] <- "Cluster Randomized"


mods <- c("LargeSample.f", "Duration.f", "published.f",
          "Universal.f", "Personnal.f",
          "Studydesign.f")

V_list <- impute_covariance_matrix(vi = full$var,  #known correlation vector
                                   cluster = full$programID, #ProgramID
                                   r = 0.70) #assumed correlation 

for(i in 1:length(mods)){
  # i <- 7
  formula <- reformulate(termlabels = c(mods[i], centered, "-1"))   # Worth knowing - if you duplicate terms, it keeps the first one
  mod_means <- rma.mv(yi=full$ES, #effect size
                      V = V_list, #variance (tHIS IS WHAt CHANGES FROM HEmodel)
                      mods = formula, #ADD COVS HERE
                      random = ~1 | programID/ESId, #nesting structure
                      test= "t", #use t-tests
                      data=full, #define data
                      method="REML") #estimate variances using REML
  
  coef_mod_means <- as.data.frame(coef_test(mod_means,#estimation model above
                                            cluster=full$programID, #define cluster IDs
                                            vcov = "CR2")) #estimation method (CR2 is best)
  
  # limit to relevant rows (the means you are interested in)
  coef_mod_means$moderator <- mods[i]
  coef_mod_means$group <- rownames(coef_mod_means)
  rownames(coef_mod_means) <- c()
  coef_mod_means <- subset(coef_mod_means, substr(start = 1, stop = nchar(mods[i]), x = coef_mod_means$group)== mods[i])
  coef_mod_means$group <- substr(x = coef_mod_means$group, start = nchar(mods[i])+1, stop = nchar(coef_mod_means$group))
  means <- dplyr::bind_rows(means, coef_mod_means)
}
means


##social marginal table
means <- plyr::rename(means, c("tstat" = "t", "p_Satt" = "p", "df_Satt" = "df", "beta" = "ES"))

# add in table of k's and n's
for(i in 1:length(mods)) {
  # i <- 3
  mod <- mods[i]
  temp <- as.data.frame(table(full[mod]))
  temp$moderator <- mod
  names(temp)[1] <- "group"
  temp <- plyr::rename(temp, c("Freq" = "n"))
  
  temp2 <- unique(full[c("programID", mod)])
  temp2 <- as.data.frame(table(temp2[mod]))
  temp2$moderator <- mod
  names(temp2)[1] <- "group"
  temp2 <- plyr::rename(temp2, c("Freq" = "k"))
  
  temp <- merge(x = temp, y = temp2, by = c("moderator", "group"), all = TRUE)
  
  if(exists("counts")==TRUE) {
    counts <- bind_rows(counts, temp)
  }
  if(exists("counts")==FALSE) {
    counts <-temp
  }
  
  rm(temp, temp2)
}
means$order <- as.numeric(rownames(means))
means <- merge(x = counts, y = means, by = c("moderator", "group"), all = TRUE)
means <- means[order(means$order),]
means <- means[c("moderator", "group", "k", "n", "ES", "SE", "t", "df", "p")]

#################################################################################
# Saving Output   
#################################################################################
myreport<-read_docx()

# MetaRegression Table
#Null Models
model_null <- flextable(head(full_coef_null, n=nrow(full_coef_null)))
colkeys <- c("beta", "SE", "tstat", "df_Satt")
model_null <- colformat_double(model_null,  j = colkeys, digits = 2)
model_null <- colformat_double(model_null,  j = c("p_Satt"), digits = 3)
#model_null <- autofit(model_null)
model_null <- add_header_lines(model_null, values = c("Null Model"), top = FALSE)
model_null <- theme_vanilla(model_null)
model_null <- set_table_properties(model_null, width = 1, layout = "autofit")

myreport <- body_add_par(x = myreport, value = "Table: Model Results", style = "Normal")
myreport <- body_add_flextable(x = myreport, model_null, keepnext = FALSE)

model_full <- flextable(head(MVfull.coef, n=nrow(MVfull.coef)))
model_full<- colformat_double(model_full,  j = colkeys, digits = 2)
model_full <- colformat_double(model_full,  j = c("p_Satt"), digits = 3)

model_full <- delete_part(model_full, part = "header")
model_full <- add_header_lines(model_full, values = c("Meta-Regression_Social Relationship"))
model_full <- theme_vanilla(model_full)
model_full <- set_table_properties(model_full, width = 1, layout = "autofit")

myreport <- body_add_flextable(x = myreport, model_full, keepnext = FALSE)
myreport <- body_add_par(x = myreport, value = "", style = "Normal")

# Marginal Means Table
marginalmeans <- flextable(head(means, n=nrow(means)))
colkeys <- c("moderator", "group", "k", "n", "ES", "SE", "t", "df", "p")
marginalmeans <- colformat_double(marginalmeans,  j = colkeys, digits = 2)
marginalmeans <- colformat_double(marginalmeans,  j = c("p"), digits = 3)
rm(colkeys)
marginalmeans <- theme_vanilla(marginalmeans)
marginalmeans <- merge_v(marginalmeans, j = c("moderator"))
tablenote <- c("Note. k=number of studies; n = number of outcomes; ES=effect size; SE=standard error; df=degrees of freedom")
marginalmeans <- add_footer_lines(marginalmeans, tablenote, )
marginalmeans <- set_table_properties(marginalmeans, width = 1, layout = "autofit")

myreport <- body_add_par(x = myreport, value = "Table: social Marginal Means", style = "Normal")
myreport <- body_add_flextable(x = myreport, marginalmeans, keepnext = FALSE)

# Write to word doc
file = paste("TableResultssocial.docx", sep = "")
print(myreport, file)