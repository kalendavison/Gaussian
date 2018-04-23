rm(list = ls())

setwd("/Users/kalendavison/Desktop/Applied Statistical Programming/GitHub/Gaussian")
setwd("/Users/noahbardash/Documents/GitHub/Gaussian")

library(devtools)
library(arm)
library(gpe)
library(lme4)

sample_selector = function(state_numbers, sample_n, plots){
  vote_data = read.delim("votingdata.dat") # Read in dataset from .dat file
  vote_data = na.exclude(vote_data) # Remove all entries with missing data
  
  vote_data <- vote_data[!(vote_data$stt==12),] # Removal of Hawaii entry from dataset
  vote_data$stt <- ifelse(vote_data$stt > 12, vote_data$stt - 1, vote_data$stt)
  vote_data$stt <- ifelse(vote_data$stt > 2, vote_data$stt - 1, vote_data$stt) # Recode stt value for states alphabetically after AK
  
  state_data = vote_data[vote_data$stt == state_numbers, c(1,2,3,6,7)]
  sample_data = state_data[sample(1:length(state_data$stt), 1000),]
  sample_data = unique(sample_data[,c("stt", "eth", "sex", "edu")])
  
  gp_output<-gp(formula = rvote~rbf(columns = c("stt", "sex", "edu", "eth"), l = c(1, 1.7, .2, 2.9)), data = state_data, family = binomial)
  gp<-predict(gp_output, sample_data, type="response")

  state_data$stt = as.factor(state_data$stt)
  state_data$eth = as.factor(state_data$eth)
  state_data$sex = as.factor(state_data$sex)
  state_data$edu = as.factor(state_data$edu)
  
  glmer_output = glmer(formula = rvote ~ (1|stt) + (1|eth) + (1|sex) + (1|edu), data = state_data, family = binomial) 
  glmer = predict(glmer_output, newdata = sample_data, type="response")
  predictions_table <- data.frame(sample_data, glmer, gp)
  predictions_table$difference =  predictions_table$glmer - as.vector(predictions_table$gp)
  
  #function2
  #newdata2<-expand.grid(var1=unique(state_data$var1), var2=unique(state_data$var2),
  #      var3=unique(state_data$var3), var4=unique(state_data$var4))  
  #newdata2
  
  if (plots == 1){
    par(mfrow=c(2,2))
    plot(seq(from = 0, to = .5, by = .0125), seq(from = 0, to = .5, by = .0125), xlab = "GP", ylab = "Glmer", type = "n", 
         main = "Predictions compared by Ethnicity") #by ethnicity
    points(predictions_table$gp[predictions_table$eth == 1], predictions_table$glmer[predictions_table$eth == 1], col = "red", pch = 19)
    abline(lm(predictions_table$glmer[predictions_table$eth == 1] ~ predictions_table$gp[predictions_table$eth == 1]), col="red") # slope = 0.09
    points(predictions_table$gp[predictions_table$eth == 2], predictions_table$glmer[predictions_table$eth == 2], col = "yellow", pch = 19)
    abline(lm(predictions_table$glmer[predictions_table$eth == 2] ~ predictions_table$gp[predictions_table$eth == 2]), col="yellow") # slope = 0.70
    points(predictions_table$gp[predictions_table$eth == 3], predictions_table$glmer[predictions_table$eth == 3], col = "green", pch = 19)
    abline(lm(predictions_table$glmer[predictions_table$eth == 3] ~ predictions_table$gp[predictions_table$eth == 3]), col="green") # slope = 0.85
    points(predictions_table$gp[predictions_table$eth == 4], predictions_table$glmer[predictions_table$eth == 4], col = "black", pch = 19) 
    abline(lm(predictions_table$glmer[predictions_table$eth == 4] ~ predictions_table$gp[predictions_table$eth == 4]), col="black") # slope = -0.03
    
    fit<-lm(predictions_table$glmer ~ predictions_table$gp) #slope = 0.73
    abline(fit, col="blue")
    
    plot(seq(from = 0, to = .5, by = .0125), seq(from = 0, to = .5, by = .0125), type = "n", xlab= "GP", ylab = "Glmer", 
         main = "Predictions compared by Sex") #by sex
    points(predictions_table$gp[predictions_table$sex == 1], predictions_table$glmer[predictions_table$sex == 1], col = "blue", pch = 19)
    abline(lm(predictions_table$glmer[predictions_table$sex==1] ~ predictions_table$gp[predictions_table$sex==1]), col="blue") # slope = 0.55
    points(predictions_table$gp[predictions_table$sex == 2], predictions_table$glmer[predictions_table$sex == 2], col = "pink", pch = 19)
    abline(lm(predictions_table$glmer[predictions_table$sex==2] ~ predictions_table$gp[predictions_table$sex==2]), col="pink") # slope = 0.91
    
    fit<-lm(predictions_table$glmer ~ predictions_table$gp) #slope = 0.73
    abline(fit, col="black")
    
    plot(seq(from = 0, to = .5, by = .0125), seq(from = 0, to = .5, by = .0125), type = "n", xlab= "GP", ylab = "Glmer", 
         main = "Predictions compared by Education") #by sex
    points(predictions_table$gp[predictions_table$edu == 1], predictions_table$glmer[predictions_table$edu == 1], col = "purple", pch = 19)
    abline(lm(predictions_table$glmer[predictions_table$edu==1] ~ predictions_table$gp[predictions_table$edu==1]), col="purple") # slope = 0.47
    points(predictions_table$gp[predictions_table$edu == 2], predictions_table$glmer[predictions_table$edu == 2], col = "blue", pch = 19)
    abline(lm(predictions_table$glmer[predictions_table$edu==2] ~ predictions_table$gp[predictions_table$edu==2]), col="blue") # slope = 0.75
    points(predictions_table$gp[predictions_table$edu == 3], predictions_table$glmer[predictions_table$edu == 3], col = "green", pch = 19)
    abline(lm(predictions_table$glmer[predictions_table$edu==3] ~ predictions_table$gp[predictions_table$edu==3]), col="green") # slope = 1.07
    points(predictions_table$gp[predictions_table$edu == 4], predictions_table$glmer[predictions_table$edu == 4], col = "yellow", pch = 19)
    abline(lm(predictions_table$glmer[predictions_table$edu==4] ~ predictions_table$gp[predictions_table$edu==4]), col="yellow") # slope = 0.65
    points(predictions_table$gp[predictions_table$edu == 5], predictions_table$glmer[predictions_table$edu == 5], col = "red", pch = 19)
    abline(lm(predictions_table$glmer[predictions_table$edu==5] ~ predictions_table$gp[predictions_table$edu==5]), col="red") # slope = 1.53
    
    plot(predictions_table$glmer, predictions_table$difference, main = "Glmer versus difference in predictions", xlab ="Glmer", ylab ="Difference")
    
    fit<-lm(predictions_table$glmer ~ predictions_table$gp) #slope = 0.73
    abline(fit, col="black")
    print(fit$coefficients[[2]])
    return(predictions_table) 
  }
  else {
    return(predictions_table)
  }
}

sample_selector(state_numbers = c(1,2,3,5,6,7), sample_n = 500, plots = 1)
sample_selector(state_numbers = c(2,4,20), sample_n = 500, plots = 1) #this was beautiful
sample_selector(state_number = 20, sample_n = 2000, plots = 1) #Mass with plots, returns regression coefficient
sample_selector(state_number = 2, sample_n = 1700, plots = 1) #Arizona with plots
sample_selector(state_number = 4, sample_n = 1950, plots = 1) #california with plots

#fix margins in plots
#write a few paragraphs explaining
#color points in difference plots by race

