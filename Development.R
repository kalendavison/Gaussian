# Clear global environment
rm(list = ls())

# Installation of necessary packages
install_github('goldingn/gpe')
install.packages("arm")
install.packages('lme4')

# Loading of necessary packages
library(devtools)
library(arm)
library(gpe)
library(lme4)
?lme4
?gp
?display

# Set working directory
setwd("/Users/kalendavison/Desktop/Applied Statistical Programming/GitHub/Gaussian")
setwd("/Users/Ian Davis/Documents/GitHub/Gaussian")
setwd("/Users/noahbardash/Documents/GitHub/Gaussian")

#function takes in as arguments state number and desired sample size. It returns a data set 
# of random observations of the specified state and size. Plot = 1 includes plots; plots = 0 has no plots.

sample_selector = function(state_number, sample_n, plots){ 
  vote_data = read.delim("votingdata.dat") # Read in dataset from .dat file
  vote_data = na.exclude(vote_data) # Remove all entries with missing data
  
  vote_data <- vote_data[!(vote_data$stt==12),] # Removal of Hawaii entry from dataset
  vote_data$stt <- ifelse(vote_data$stt > 2, vote_data$stt - 1, vote_data$stt) # Recode stt value for states alphabetically after AK
  vote_data$stt <- ifelse(vote_data$stt > 12, vote_data$stt - 1, vote_data$stt)
  
  group = vote_data[vote_data$stt == state_number, 1:9]
  sample_data = group[sample(1:length(group$stt), sample_n),]
  sample_data = sample_data[,c("rvote", "eth", "sex", "edu")]
  
  gp_output<-gp(formula = rvote~rbf(columns = c("sex", "edu", "eth")), data = sample_data, family = binomial)
  gp_predictions<-predict(gp_output, sample_data, type="response") 
  
  eth = c(rep(1,10), rep(2,10), rep(3,10), rep(4,10))
  sex = c(rep((c(rep(1,5), rep(2,5))), 4))
  edu = rep(1:5, 8)
  fake.dataset = data.frame(eth, sex, edu)
  gp_predict<-predict(gp_output, fake.dataset, type="response") 
  demographic.predictions = data.frame(gp_predict, fake.dataset)
  
  gp_predictions = as.data.frame(table(gp_predictions)) 
  gp_predictions = gp_predictions[order(gp_predictions$Freq),]
  
  var1 = sample_data$eth
  sample_data$var1 = as.factor(var1)
  var2 = sample_data$sex
  sample_data$var2 = as.factor(var2)
  var3 = sample_data$edu
  sample_data$var3 = as.factor(var3)
  
  glmer_output = glmer(formula = rvote ~ (1|var1) + (1|var2) + (1|var3), data = sample_data, family = binomial) 
  glmer_predictions = predict(glmer_output, newdata = sample_data, type="response")
  glmer_predictions = as.data.frame(table(glmer_predictions)) 
  glmer_predictions = glmer_predictions[order(glmer_predictions$Freq),] 
  
  ordered = demographic.predictions[order(demographic.predictions$gp_predict),] #order gp by fake.dataset to later add to master comparison
  comparison = data.frame(gp_predictions$gp_predictions, glmer_predictions$glmer_predictions, ordered) #to be cleaned to make sense
  comparison = comparison[order(comparison$gp_predictions),] #order by gp_predictions to match methods
  comparison$gp_predictions.gp_predictions = NULL #no longer necessary because added predictions_mass
  comparison = comparison[order(comparison$glmer_predictions.glmer_predictions),] #reorder 
  comparison$glmer = comparison$glmer_predictions.glmer_predictions #rename for sense
  comparison$glmer_predictions.glmer_predictions = NULL #no longer needed (just renamed)
  comparison$gp = comparison$gp_predict #rename for sense
  comparison$gp_predict = NULL #no longer needed (just renamed)
  comparison$glmer = as.vector(comparison$glmer) #change from a factor to numeric for plotting purposes
  
  if (plots == 1){
  par(mfrow=c(2,2))
  plot(seq(from = 0, to = .5, by = .0125), seq(from = 0, to = .5, by = .0125), xlab = "GP", ylab = "Glmer", type = "n", 
       main = "Predictions compared by Ethnicity") #by ethnicity
  points(comparison$gp[comparison$eth == 1], comparison$glmer[comparison$eth == 1], col = "red", pch = 19)
  abline(lm(comparison$glmer[comparison$eth == 1] ~ comparison$gp[comparison$eth == 1]), col="red") # slope = 0.09
  points(comparison$gp[comparison$eth == 2], comparison$glmer[comparison$eth == 2], col = "yellow", pch = 19)
  abline(lm(comparison$glmer[comparison$eth == 2] ~ comparison$gp[comparison$eth == 2]), col="yellow") # slope = 0.70
  points(comparison$gp[comparison$eth == 3], comparison$glmer[comparison$eth == 3], col = "green", pch = 19)
  abline(lm(comparison$glmer[comparison$eth == 3] ~ comparison$gp[comparison$eth == 3]), col="green") # slope = 0.85
  points(comparison$gp[comparison$eth == 4], comparison$glmer[comparison$eth == 4], col = "black", pch = 19) 
  abline(lm(comparison$glmer[comparison$eth == 4] ~ comparison$gp[comparison$eth == 4]), col="black") # slope = -0.03
  
  fit<-lm(comparison$glmer ~ comparison$gp) #slope = 0.73
  abline(fit, col="blue")
  
  plot(seq(from = 0, to = .5, by = .0125), seq(from = 0, to = .5, by = .0125), type = "n", xlab= "GP", ylab = "Glmer", 
       main = "Predictions compared by Sex") #by sex
  points(comparison$gp[comparison$sex == 1], comparison$glmer[comparison$sex == 1], col = "blue", pch = 19)
  abline(lm(comparison$glmer[comparison$sex==1] ~ comparison$gp[comparison$sex==1]), col="blue") # slope = 0.55
  points(comparison$gp[comparison$sex == 2], comparison$glmer[comparison$sex == 2], col = "pink", pch = 19)
  abline(lm(comparison$glmer[comparison$sex==2] ~ comparison$gp[comparison$sex==2]), col="pink") # slope = 0.91
  
  fit<-lm(comparison$glmer ~ comparison$gp) #slope = 0.73
  abline(fit, col="black")
  
  plot(seq(from = 0, to = .5, by = .0125), seq(from = 0, to = .5, by = .0125), type = "n", xlab= "GP", ylab = "Glmer", 
       main = "Predictions compared by Education") #by sex
  points(comparison$gp[comparison$edu == 1], comparison$glmer[comparison$edu == 1], col = "purple", pch = 19)
  abline(lm(comparison$glmer[comparison$edu==1] ~ comparison$gp[comparison$edu==1]), col="purple") # slope = 0.47
  points(comparison$gp[comparison$edu == 2], comparison$glmer[comparison$edu == 2], col = "blue", pch = 19)
  abline(lm(comparison$glmer[comparison$edu==2] ~ comparison$gp[comparison$edu==2]), col="blue") # slope = 0.75
  points(comparison$gp[comparison$edu == 3], comparison$glmer[comparison$edu == 3], col = "green", pch = 19)
  abline(lm(comparison$glmer[comparison$edu==3] ~ comparison$gp[comparison$edu==3]), col="green") # slope = 1.07
  points(comparison$gp[comparison$edu == 4], comparison$glmer[comparison$edu == 4], col = "yellow", pch = 19)
  abline(lm(comparison$glmer[comparison$edu==4] ~ comparison$gp[comparison$edu==4]), col="yellow") # slope = 0.65
  points(comparison$gp[comparison$edu == 5], comparison$glmer[comparison$edu == 5], col = "red", pch = 19)
  abline(lm(comparison$glmer[comparison$edu==5] ~ comparison$gp[comparison$edu==5]), col="red") # slope = 1.53
  
  fit<-lm(comparison$glmer ~ comparison$gp) #slope = 0.73
  abline(fit, col="black")
  print(fit$coefficients[[2]])
  return(comparison) 
  }
  else {
    return(comparison)
  }
}

sample_selector(state_number = 20, sample_n = 2000, plots = 0) #Mass without plots 
sample_selector(state_number = 20, sample_n = 2000, plots = 1) #Mass with plots, returns regression coefficient


# Reading in & processing of data
vote_data = read.delim("votingdata.dat") # Read in dataset from .dat file
vote_data = na.exclude(vote_data) # Remove all entries with missing data

# Clean dataset: state 2 (AK) has no entries and state 12 (HI) only has one. No statistically useful data here
vote_data <- vote_data[!(vote_data$stt==12),] # Removal of Hawaii entry from dataset
vote_data$stt <- ifelse(vote_data$stt > 2, vote_data$stt - 1, vote_data$stt) # Recode stt value for states alphabetically after AK
vote_data$stt <- ifelse(vote_data$stt > 12, vote_data$stt - 1, vote_data$stt) # Recode stt values for states alphabetically after HI
# 48 contiguous states + DC now numbered 1-49 in alphabetical order

# Creation of dummy variables for each state
state.f<-factor(vote_data$stt)
dummies<-model.matrix(~state.f)
vote_data<-cbind(vote_data, dummies) # Attach dummy matrix to vote_data

# Creation of dummy variables for each race
vote_data$white<-ifelse(vote_data$eth==1, c(1), c(0))
vote_data$black<-ifelse(vote_data$eth==2, c(1), c(0))
vote_data$hisp<-ifelse(vote_data$eth==3, c(1), c(0))
vote_data$api<-ifelse(vote_data$eth==4, c(1), c(0))

# Creation of dummy variables for each sex 
vote_data$male <- ifelse(vote_data$sex==1, c(1), c(0))
vote_data$female <- ifelse(vote_data$sex==2, c(1), c(0))

# Creation of dummy variables for each ethnicity
vote_data$noHS <- ifelse(vote_data$edu==1, c(1), c(0))
vote_data$HSgrad <- ifelse(vote_data$edu==2, c(1), c(0))
vote_data$somecollege <- ifelse(vote_data$edu==3, c(1), c(0))
vote_data$bachelors <- ifelse(vote_data$edu==4, c(1), c(0))
vote_data$adv_degree <- ifelse(vote_data$edu==5, c(1), c(0))

# ((possibly working with these variables later))
vote_data$mar<-ifelse(vote_data$mar==1, c(1), c(0)) # Recode married to 0 1
vote_data$kid<-ifelse(vote_data$kid==1, c(1), c(0)) # Recode kid to 0 1 


