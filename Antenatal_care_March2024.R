library(haven)
library(psych)
library("arsenal")
library(mice)
library(VIM)

path<- "O:\\medewerkers\\227021 Enthoven, C.A\\Postdoc\\Data"
setwd("O:\\medewerkers\\227021 Enthoven, C.A\\Postdoc\\Antenatal care") 

D1<-read_sav(paste(path,"FETALPERIOD-ALLGENERALDATA_24102022.sav",sep="/"))
D1$income <- as.factor(ifelse(D1$INCOME<7, 3, ifelse(D1$INCOME>6 & D1$INCOME<11, 2, ifelse(D1$INCOME>10, 1, NA)))) # 1= high, 2= mid, 3=low
D1$parity <- as.factor(ifelse(D1$PARITY==0, 1, ifelse(D1$PARITY==1, 2, ifelse(D1$PARITY==2, 3, ifelse(D1$PARITY<=3, 4, NA))))) # 1= 0, 2= 1, 3= 2, 4= >=3
D1$maritalstatus <- as.factor(ifelse(D1$MAR_ST2==1 | D1$MAR_ST2==2, 1, ifelse(D1$MAR_ST2==3, 2, NA))) # 1 = married or cohabiting, 2 = single
D1$educ <- as.factor(ifelse(D1$EDUCM<3, 3, ifelse(D1$EDUCM==3, 2, ifelse(D1$EDUCM==4, 2, ifelse(D1$EDUCM==5, 1, NA))))) # high, mid, low
D1$BMI <- D1$BMI_1 # BMI at intake

D2 <-read_sav(paste(path,"20170611_AgeMotherBirthChild.sav",sep="/"))
D2 <- D2[!duplicated(D2$IDM),] # remove duplicates
data <- merge(D1, D2, by = "IDM", all.x = T) 
data$ageconception <- data$agemother_birthchild - data$GESTBIRPREG/52 # age birth - gestational age in weeks / 52 weeks in a year

D4<-read_spss(paste("O:\\medewerkers\\227021 Enthoven, C.A\\Postdoc\\Data\\GR1001-A_22112016.sav",sep="/"))
data <- merge(data, D4, by.x = "IDM", by.y = "idm", all.x = T)
data$intention <- as.factor(ifelse(data$a0500101==0 & is.na(data$a0500301), NA, ifelse(data$a0500101==0 & data$a0500301==1, 2, ifelse(data$a0500101==0 & data$a0500301>1, 3, 
                    ifelse(data$a0500101==1, 1, NA)))))  # 1=planned, 2=unplanned & pleased, 3=unplanned and ambivalent
data$recognition <- data$a0400101_cleaned

D5<-read_spss(paste(path,"GR1003-D2-20_01072012.sav",sep="/")) # depression, anxiety and psychosis vignettes
data <- merge(data, D5, by = "IDM", all.x = T)

D6<-read_spss(paste(path,"GR1003-E_01072012.sav",sep="/")) # eating disorders
data <- merge(data, D6, by = "IDM", all.x = T)

D7<-read_spss(paste(path,"GR1003-F_01072012.sav",sep="/")) # addiction vignettes
data <- merge(data, D7, by = "IDM", all.x = T)

data$anydisorder <- as.factor(ifelse(data$depression_ever==0 & data$anxiety_ever==0 & data$ED_ever==0 & data$psychosis_ever==0 & data$addiction_ever==0, 0,
                                    ifelse((data$depression_ever==1 & data$depression_pastY==0) | (data$anxiety_ever==1 & data$anxiety_pastY==0) | (data$ED_ever==1 & (data$e0100203==0 | data$e0200203==0)) | 
                                      (data$psychosis_ever==1 & data$psychosis_pastY==0) | (data$addiction_ever==1 & data$addiction_pastY==0), 1,
                                            ifelse(data$depression_pastY==1 | data$anxiety_pastY==1 | data$e0100203==1 | data$e0200203==1 | data$psychosis_pastY==1 | data$addiction_pastY==1, 2, NA))))
                                          # 0= no disorder, 1= history of disorder, 2= current disorder
D8<-read_spss(paste(path,"GR1005-D_22112016.sav",sep="/"))
data <- merge(data, D8, by = "IDM",  all.x = T)
data$job <- as.factor(ifelse(data$d0100105 == 1 | data$d0100105 == 2, 1, ifelse(is.na(data$d0100105), NA, 2))) #  1= paid job 2= no paid job

D9<-read_spss(paste(path,"COGNITIONPARENT_28052013_APM_IBF_VDR_outliersexcluded.sav",sep="/")) # cognitie moeder
D9 <- D9[!duplicated(D9$IDM),] # remove duplicates
data <- merge(data, D9, by = 'IDM', all.x = T, all.y = F)
data$cognition <- data$APM_IQ

D10<-read_spss(paste(path,"GR1005-E_22112016.sav",sep="/"))
data <- merge(data, D10, by = "IDM",  all.x = T)
data$housing <- as.factor(ifelse(data$e0600105 == 1, 1, ifelse(is.na(data$e0600105), NA, 2))) # 1= own home 2= rented home

D11<-read_spss(paste(path,"Statusscores_Dionne.sav",sep="/"))
data <- merge(data, D11, by = "IDM",  all.x = T)
data$deprivation <- ifelse(data$COHORT1==1 | data$COHORT1==2, data$statusscore02, ifelse(data$COHORT1==3, data$statusscore_0206middeling, ifelse(data$COHORT1==4 | data$COHORT1==5, data$statusscore06, NA)))

D12<-read_spss(paste(path,"20230814_gestational_age_first_visit.sav",sep="/"))
data <- merge(data, D12, by.x = "IDM", by.y = "idm",  all.x = T)
data$firstvisit <- data$gest.1final

D13<-read_spss(paste(path,"PREGNANCYMENSTRUALCYCLE_2302201.sav",sep="/"))
data <- merge(data, D13, by = "IDM",  all.x = T)
data$cycle <- as.factor(ifelse(data$cyclus=='regelmatig - LM onzeker' | data$cyclus=='regelmatig - LM zeker', 1, 0)) # 1= regular cycle, 0= no regular cycle or unknown

D14<-read_spss(paste(path,"GR1001-J_22112016.sav",sep="/"))
data <- merge(data, D14, by = "IDM",  all.x = T)
data$language <- data$j1100101 + data$j1100201 + data$j1100301 # 1= not at all, 2= a little, 3= reasonable, 4= quite good, 5=  better
data$ethni <- as.factor(ifelse(data$ETHNMv2==1, 1, ifelse(data$ETHNMv2!=1 & data$generama==2, 2, ifelse(data$ETHNMv2!=1 & data$generama==1, 3, NA)))) #1=Dutch, 2=second generation, 3=first generation

## save data ##
#save(data, file="data.RData")
#load("data.RData")

data1 <- data[!is.na(data$firstvisit),] # exclude all participants without outcome variable N=4204
duplicates <- data1$MOTHER.x[duplicated(data1$MOTHER.x)] # 8 participants are twice in the dataset
data2 <- data1[!duplicated(data1$MOTHER.x, fromLast = TRUE), ] # Keep only the last occurrence of each duplicated value N=4196

df <- data2[c("ageconception", "ethni", "educ", "job", "anydisorder", "cognition", "language", "maritalstatus", "intention", "income", "housing", "parity","deprivation",
               "recognition", "firstvisit", "cycle", "GESTINT", "APGAR5", "BMI")]

## save data ##
#save(df, file="df.RData")
#load("df.RData")

pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(df,2,pMiss)

########################################################################################################################
###########################################      Imputation      #######################################################
########################################################################################################################
library(mice)
library(VIM)

## Show complete cases
Data_complete <-df[complete.cases(df),]
dim(Data_complete) #N=1214
#md.pattern(df)

### IMPUTATION set methods
ini <- mice(df, maxit = 0)
meth <- ini$meth

### IMPUTATION
imputed.data <- mice(df, m=50, maxit=100, meth=meth, seed=500)
summary(imputed.data)

########### SAVE FILE ################
#saveRDS(imputed.data, "O:\\medewerkers\\227021 Enthoven, C.A\\Postdoc\\Antenatal care\\imputed.data.rds")
########### OPEN FILE #################
#imputed.data <- readRDS("O:\\medewerkers\\227021 Enthoven, C.A\\Postdoc\\Antenatal care\\imputed.data.rds")

# Create categories in imputed dataset #
long <- complete(imputed.data, action='long', include=T)

long$earlyrecog <- as.factor(ifelse(long$recognition<7, 1, ifelse(long$recognition>=7, 0, NA)))

long$age <- as.factor(ifelse(long$ageconception<20, 1, ifelse(long$ageconception>19 & long$ageconception<25, 2, ifelse(long$ageconception>24 & long$ageconception<30, 3,
                                              ifelse(long$ageconception>29 & long$ageconception<35, 4, 5))))) # <20, 20-25, 25-30, 30-35, >= 35 years
long$cognitioncat <- as.factor(ifelse(long$cognition<70, 3, ifelse(long$cognition>=70 & long$cognition<85, 2, 1)))  # =>85, 70-85, <70
long$languagecat <- as.factor(ifelse(long$language<10, 3, ifelse(long$language>9 & long$language<15, 2, ifelse(long$language==15, 1, NA)))) #1= good, 2=reasonable, 3= not good
long$deprivationcat <- cut(long$deprivation, breaks = quantile(long$deprivation, probs = c(0,1/3, 2/3,1), na.rm = TRUE), labels = c(3, 2, 1), include.lowest = T) #1= low depr 2=medium depr 3=high deprivation
long$deprivationcat <- factor(long$deprivationcat, c(1,2,3))
imputed.data2 <- as.mids(long)

########################################################################################################################
###########################################     Demographics     #######################################################
########################################################################################################################

df$latevisit <-as.factor(ifelse(df$firstvisit<15, 0, 1)) #0= within 14 weeks, 1=15 weeks or later
df$earlyrecog <- as.factor(ifelse(df$recognition<7, 1, ifelse(df$recognition>=7, 0, NA)))
df$age <- as.factor(ifelse(df$ageconception<20, 1, ifelse(df$ageconception>19 & df$ageconception<25, 2, ifelse(df$ageconception>24 & df$ageconception<30, 3,
                      ifelse(df$ageconception>29 & df$ageconception<35, 4, 5))))) 

df$cognitioncat <- as.factor(ifelse(df$cognition<70, 3, ifelse(df$cognition>=70 & df$cognition<85, 2, 1)))# =>85, 70-85, <70

df$languagecat <- as.factor(ifelse(df$language<10, 3, ifelse(df$language>9 & df$language<15, 2, ifelse(df$language==15, 1, NA)))) #1= good, 2=reasonable, 3=not good

df$deprivationcat <- cut(df$deprivation, breaks = quantile(df$deprivation, probs = c(0,1/3, 2/3,1), na.rm = TRUE), labels = c(3, 2, 1), include.lowest = T) #1= low, 2= medium, 3= high deprivation
df$deprivationcat <- factor(df$deprivationcat, c(1,2,3))

describe(df$ageconception)
describe(df$firstvisit)
describe(df$recognition)

library("arsenal")
my_controls <- tableby.control(test=T, total=T, ordered.stats= c("Nmiss", "countpct"), cat.stats = c("Nmiss", "countpct"), digits = 2, digits.p=2)
table_one <- tableby(latevisit ~ age + ethni + maritalstatus + intention + anydisorder + languagecat + parity + educ + job + income + housing + deprivationcat + 
                       cognitioncat + earlyrecog + cycle, data= df, control = my_controls)

labels(table_one) <- c(age="Age (years)", ethni="Migration background", maritalstatus="Relationship status", intention="Pregnancy Intention", anydisorder="Psychopathology", 
                       languagecat="Dutch language ability", parity="Parity",educ="Educational attainment",job="Employment", income="Household income", housing="Housing",  
                       deprivationcat="Neighborhood deprivation", cognitioncat="Cognitive ability (IQ)", earlyrecog="Early pregnancy recognition", cycle= "Menstrual cycle")
summary(table_one)

write2word(table_one, "table1.docx") 

### check difference between those with regular or not regular (or unkown) cycle
library("arsenal")
my_controls <- tableby.control(test=T, total=T, ordered.stats= c("Nmiss", "countpct"), cat.stats = c("Nmiss", "countpct"), digits = 2, digits.p=2)
table_one <- tableby(cycle ~ latevisit + age + ethni + maritalstatus + intention + anydisorder + languagecat + parity + educ + job + income + housing + deprivationcat + 
                       cognitioncat + earlyrecog + cycle, data= df, control = my_controls)

labels(table_one) <- c(age="Age (years)", ethni="Migration background", maritalstatus="Relationship status", intention="Pregnancy Intention", anydisorder="Psychopathology", 
                       languagecat="Dutch language ability", parity="Parity",educ="Educational attainment",job="Employment", income="Household income", housing="Housing",  
                       deprivationcat="Neighborhood deprivation", cognitioncat="Cognitive ability (IQ)", earlyrecog="Early pregnancy recognition")
summary(table_one)

write2word(table_one, "table1.docx") 

########################################################################################################################
##############                               Test positivity assumption                              ###################
########################################################################################################################

# remove those with missing data on the intervention
df.ps <- df[!is.na(df$earlyrecog),]
# estimate model with early pregnancy recognition (intervention) as outcome and all covariates as predictors
model.positivity <- glm(earlyrecog ~  + age + ethni + educ + anydisorder + cognitioncat + languagecat + deprivationcat + 
                          job + income + intention + housing + parity + maritalstatus, data=df.ps, family=binomial())
# save probabilities
df.ps$ps <- predict(model.positivity, df.ps, type="response")
# show probabilities for everyone, and for those with and without early pregnancy recognition
summary(df.ps$ps) # min=0.217 and max=0.986
summary(df.ps$ps[df.ps$earlyrecog==0]) # min=0.304 and max=0.954
summary(df.ps$ps[df.ps$earlyrecog==1]) # min=0.217 and max=0.986


########################################################################################################################
##############             Association between recognition and antenatal care initiation             ###################
########################################################################################################################

model1 <- with(imputed.data2, lm(firstvisit ~ earlyrecog))
x1 <- summary(pool(model1), conf.int = TRUE)
outname1 <- "O:/medewerkers/227021 Enthoven, C.A/Postdoc/Antenatal care/recog_initation.txt"
write.table(x1, outname1, col.names = TRUE, row.names = TRUE, quote = FALSE, sep = "\t")
model2 <- with(imputed.data2, lm(firstvisit ~ earlyrecog + age + ethni + educ + anydisorder + cognitioncat + languagecat + deprivationcat + 
                                 job + income + intention + housing + parity + maritalstatus))
x2 <- summary(pool(model2), conf.int = TRUE)
outname2 <- "O:/medewerkers/227021 Enthoven, C.A/Postdoc/Antenatal care/recog_initation_adj.txt"
write.table(x2, outname2, col.names = TRUE, row.names = TRUE, quote = FALSE, sep = "\t")

# excluding those without regular cylce
imputed.data_sens <- filter(imputed.data2, cycle==1)

model1 <- with(imputed.data_sens, lm(firstvisit ~ earlyrecog))
x1 <- summary(pool(model1), conf.int = TRUE)
outname1 <- "O:/medewerkers/227021 Enthoven, C.A/Postdoc/Antenatal care/recog_initation_sens.txt"
write.table(x1, outname1, col.names = TRUE, row.names = TRUE, quote = FALSE, sep = "\t")
model2 <- with(imputed.data_sens, lm(firstvisit ~ earlyrecog + age + ethni + educ + anydisorder + cognitioncat + languagecat + deprivationcat + 
                                   job + income + intention + housing + parity + maritalstatus))
x2 <- summary(pool(model2), conf.int = TRUE)
outname2 <- "O:/medewerkers/227021 Enthoven, C.A/Postdoc/Antenatal care/recog_initation_adj_sens.txt"
write.table(x2, outname2, col.names = TRUE, row.names = TRUE, quote = FALSE, sep = "\t")


########################################################################################################################
##                                                 Hypothetical intervention                                          ##
########################################################################################################################
#Step 1: Run function change_inequality.R saved in a different file.
#Step 2: Run function diff_ineq2.R saved in a different file.
library(mice)
library(boot)
library(broom)

# Set reference to the value with an average earliest antenatal care visit
long2 <- complete(imputed.data2, action='long', include=T)

long2$age2 <- as.factor(ifelse(long2$age==4, 1, ifelse(long2$age==1, 2, ifelse(long2$age==2, 3, ifelse(long2$age==3, 4, long2$age))))) # 30-35, <20, 20-25, 25-30, >= 35 years
long2$anydisorder2 <- as.factor(ifelse(long2$anydisorder==0, 3, ifelse(long2$anydisorder==1 ,1, ifelse(long2$anydisorder==2, 2, NA)))) # 3= no disorder, 1= history of disorder, 2= current disorder
imputed.data3 <- as.mids(long2)


# Define a function for the analysis
run_analysis <- function(ineq_group, covars, filename) {
  output.list <- list()
  
  for (imp in 1:50) {
    ds <- complete(imputed.data3, imp)
    
    ineq_fun_args <- list(
      dat = ds, inds = 1:nrow(ds), outcome = "firstvisit", mediator = "recognition",
      ineq_group = ineq_group, covars = covars, interaction = TRUE,
      set_mediator_to = list("recognition<=6")
    )
    
    boot_out <- boot(data = ds, statistic = ineq_fun, outcome = "firstvisit", mediator = "recognition", 
                     ineq_group = ineq_group, covars = covars, interaction = TRUE, 
                     set_mediator_to = ineq_fun_args$set_mediator_to, R = 1000)
    
    output <- tidy(boot_out, conf.int = TRUE)
    
    output.list[[paste0(ineq_group, imp)]] <- output
  }
  
  assign(paste0("output.list.", ineq_group), output.list, envir = .GlobalEnv)
  
  write.csv(output.list, file = paste0(filename, ".csv"), row.names = TRUE)
  
  list_names <- ineq_group
  
  for (list_name in list_names) {
    output_list <- get(paste0("output.list.", list_name))
    
    # Get the number of levels in the ineq_group variable
    num_levels <- length(unique(ds[[ineq_group]]))
    
    # Generate term names dynamically based on the number of levels
    term_names <- c(paste0("pre_1 vs ", 2:num_levels), paste0("post_1 vs ", 2:num_levels), paste0("diff_post_1 vs ", 2:(num_levels)))
    
    pooled_result <- data.frame(
      term = term_names,
      statistic = numeric(length(term_names)),
      conf.low = numeric(length(term_names)),
      conf.high = numeric(length(term_names))
    )
    
        for (i in 1:nrow(pooled_result)) {
      term <- pooled_result$term[i]
      pooled_result$statistic[i] <- mean(unlist(sapply(output_list, function(x) as.data.frame(x)[x$term == term, "statistic"])), na.rm = TRUE)*-1
      pooled_result$conf.low[i] <- mean(unlist(sapply(output_list, function(x) as.data.frame(x)[x$term == term, "conf.high"])), na.rm = TRUE)*-1
      pooled_result$conf.high[i] <- mean(unlist(sapply(output_list, function(x) as.data.frame(x)[x$term == term, "conf.low"])), na.rm = TRUE)*-1

    }
    
    write.csv(pooled_result, file = paste0("pooled.result.", list_name, ".csv"), row.names = FALSE)
  }
}

# Variables of interest
variables_of_interest <- c("age2", "ethni", "educ", "anydisorder2", "cognitioncat", "languagecat", "deprivationcat", 
                           "job", "income", "intention", "housing", "parity", "maritalstatus")

# Run the analyses for each variable
for (variable in variables_of_interest) {
  covars <- setdiff(variables_of_interest, variable)
  run_analysis(variable, covars, variable)
}

#####################################################################################################################################
##                                  Hypothetical intervention - sensitivity analyses                                               ##
#####################################################################################################################################
#Step 1: Run function change_inequality.R saved in a different file.
#Step 2: Run function diff_ineq2.R saved in a different file.
library(mice)
library(boot)
library(broom)

# Exclude those with irregular or unknown cycle
imputed.data4 <- filter(imputed.data3, cycle==1)
imp1 <- complete(imputed.data4,1)

# Define a function for the analysis
run_analysis <- function(ineq_group, covars, filename) {
  output.list <- list()
  
  for (imp in 1:50) {
    ds <- complete(imputed.data4, imp)
    
    ineq_fun_args <- list(
      dat = ds, inds = 1:nrow(ds), outcome = "firstvisit", mediator = "recognition",
      ineq_group = ineq_group, covars = covars, interaction = TRUE,
      set_mediator_to = list("recognition<=6")
    )
    
    boot_out <- boot(data = ds, statistic = ineq_fun, outcome = "firstvisit", mediator = "recognition", 
                     ineq_group = ineq_group, covars = covars, interaction = TRUE, 
                     set_mediator_to = ineq_fun_args$set_mediator_to, R = 1000)
    
    output <- tidy(boot_out, conf.int = TRUE)
    
    output.list[[paste0(ineq_group, imp)]] <- output
  }
  
  assign(paste0("output.list.", ineq_group), output.list, envir = .GlobalEnv)
  
  write.csv(output.list, file = paste0(filename, "sens.csv"), row.names = TRUE)
  
  list_names <- ineq_group
  
  for (list_name in list_names) {
    output_list <- get(paste0("output.list.", list_name))
    
    # Get the number of levels in the ineq_group variable
    num_levels <- length(unique(ds[[ineq_group]]))
    
    # Generate term names dynamically based on the number of levels
    term_names <- c(paste0("pre_1 vs ", 2:num_levels), paste0("post_1 vs ", 2:num_levels), paste0("diff_post_1 vs ", 2:(num_levels)))
    
    pooled_result <- data.frame(
      term = term_names,
      statistic = numeric(length(term_names)),
      conf.low = numeric(length(term_names)),
      conf.high = numeric(length(term_names))
    )
    
    for (i in 1:nrow(pooled_result)) {
      term <- pooled_result$term[i]
      pooled_result$statistic[i] <- mean(unlist(sapply(output_list, function(x) as.data.frame(x)[x$term == term, "statistic"])), na.rm = TRUE)*-1
      pooled_result$conf.low[i] <- mean(unlist(sapply(output_list, function(x) as.data.frame(x)[x$term == term, "conf.high"])), na.rm = TRUE)*-1
      pooled_result$conf.high[i] <- mean(unlist(sapply(output_list, function(x) as.data.frame(x)[x$term == term, "conf.low"])), na.rm = TRUE)*-1
      
    }
    
    write.csv(pooled_result, file = paste0("pooled.result.sens.", list_name, ".csv"), row.names = FALSE)
  }
}

# Variables of interest
variables_of_interest <- c("age2", "ethni", "educ", "anydisorder2", "cognitioncat", "languagecat", "deprivationcat", 
                           "job", "income", "intention", "housing", "parity", "maritalstatus")

# Run the analyses for each variable
for (variable in variables_of_interest) {
  covars <- setdiff(variables_of_interest, variable)
  run_analysis(variable, covars, variable)
}

########################################################################################################################
#####################################                   Figure                    ######################################
########################################################################################################################
library(readxl)
library(ggplot2)

data_figure <- read_excel("Data_figuur.xlsx")
data_figure$Predictors <- factor(data_figure$Predictors, levels = unique(data_figure$Predictors))

plot <- ggplot(data_figure, aes(y = Predictors, x = Beta, xmin = Conf.low, xmax = Conf.high, color = Intervention, fill = Intervention)) +
  geom_linerange(size = 3, position = position_dodge(width = 0.8)) +
  geom_point(size = 2, shape = 21, colour = "white", stroke = 0.5, position = position_dodge(width = 0.8)) +
  labs(title = "",
       x = "Differences in timing of antenatal care entry (beta-coefficients and 95% CI)",
       y = "") +
  scale_y_discrete(limits = rev(levels(data_figure$Predictors))) +  # Reverse the order of levels on the y-axis
  scale_fill_manual(values = c("#000000", "#000000"),
                    labels = c("Before" = "Before intervention", "After" = "After intervention"),
                    breaks = c("Before", "After")) +
  scale_color_manual(values = c("Before" = "#00009B", "After" = "#56B4E9"),
                     labels = c("Before" = "Before intervention", "After" = "After intervention"),
                     breaks = c("Before", "After")) +  theme_minimal() +
  theme(
    legend.title = element_blank(),
    legend.position = "top",
    legend.key = element_rect(fill = "white")
  )
plot
ggsave("Plot.tiff", plot = plot, bg = "transparent")

