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

# Set working directory
setwd("/Users/kalendavison/Desktop/Applied Statistical Programming/GitHub/Gaussian")
setwd("/Users/isdav/Documents/GitHub/Gaussian")
setwd("/Users/noahbardash/Documents/GitHub/Gaussian")

# Master function
sample_selector = function(state_numbers, sample_n, plots){
  vote_data = read.delim("votingdata.dat") # Read in dataset from .dat file
  vote_data = na.exclude(vote_data) # Remove all entries with missing data
  
  vote_data <- vote_data[!(vote_data$stt==12),] # Removal of Hawaii entry from dataset
  vote_data$stt <- ifelse(vote_data$stt > 12, vote_data$stt - 1, vote_data$stt)
  vote_data$stt <- ifelse(vote_data$stt > 2, vote_data$stt - 1, vote_data$stt) # Recode stt value for states alphabetically after AK
  
  state_data = vote_data[vote_data$stt == state_numbers, c(1,2,3,6,7)]
  sample_data = state_data[sample(1:length(state_data$stt), sample_n),]
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
  
  jpeg("withAA")
  if (plots == 1){
    par(mfrow=c(2,2))
    plot(seq(from = 0, to = 1, by = .05), seq(from = 0, to = 1, by = .05), xlab = "GP", ylab = "Glmer", type = "n", 
         main = "Predictions compared by Ethnicity") #by ethnicity
    points(predictions_table$gp[predictions_table$eth == 1], predictions_table$glmer[predictions_table$eth == 1], col = "red", pch = 19)
    abline(lm(predictions_table$glmer[predictions_table$eth == 1] ~ predictions_table$gp[predictions_table$eth == 1]), col="red")
    points(predictions_table$gp[predictions_table$eth == 2], predictions_table$glmer[predictions_table$eth == 2], col = "yellow", pch = 19)
    abline(lm(predictions_table$glmer[predictions_table$eth == 2] ~ predictions_table$gp[predictions_table$eth == 2]), col="yellow")
    points(predictions_table$gp[predictions_table$eth == 3], predictions_table$glmer[predictions_table$eth == 3], col = "green", pch = 19)
    abline(lm(predictions_table$glmer[predictions_table$eth == 3] ~ predictions_table$gp[predictions_table$eth == 3]), col="green")
    points(predictions_table$gp[predictions_table$eth == 4], predictions_table$glmer[predictions_table$eth == 4], col = "black", pch = 19) 
    abline(lm(predictions_table$glmer[predictions_table$eth == 4] ~ predictions_table$gp[predictions_table$eth == 4]), col="black")
    legend("topleft", c("White","Black","Hispanic","Asian/Pacific Islander"), bty="y", pt.bg=c("red","yellow","green","black"),
           col=c("red","yellow","green","black"), pch = c(21,21), cex = 0.6)
    fit<-lm(predictions_table$glmer ~ predictions_table$gp)
    abline(fit, col="blue")
    
    plot(seq(from = 0, to = 1, by = .05), seq(from = 0, to = 1, by = .05), type = "n", xlab= "GP", ylab = "Glmer", 
         main = "Predictions compared by Sex") #by sex
    points(predictions_table$gp[predictions_table$sex == 1], predictions_table$glmer[predictions_table$sex == 1], col = "blue", pch = 19)
    abline(lm(predictions_table$glmer[predictions_table$sex==1] ~ predictions_table$gp[predictions_table$sex==1]), col="blue")
    points(predictions_table$gp[predictions_table$sex == 2], predictions_table$glmer[predictions_table$sex == 2], col = "pink", pch = 19)
    abline(lm(predictions_table$glmer[predictions_table$sex==2] ~ predictions_table$gp[predictions_table$sex==2]), col="pink")
    legend("topleft", c("Male","Female"), bty="y", pt.bg=c("blue","pink"),
          col=c("blue","pink"), pch = c(21,21), cex = 0.75)
 
    plot(seq(from = 0, to = 1, by = .05), seq(from = 0, to = 1, by = .05), type = "n", xlab= "GP", ylab = "Glmer", 
         main = "Predictions compared by Education") #by sex
    points(predictions_table$gp[predictions_table$edu == 1], predictions_table$glmer[predictions_table$edu == 1], col = "purple", pch = 19)
    abline(lm(predictions_table$glmer[predictions_table$edu==1] ~ predictions_table$gp[predictions_table$edu==1]), col="purple")
    points(predictions_table$gp[predictions_table$edu == 2], predictions_table$glmer[predictions_table$edu == 2], col = "blue", pch = 19)
    abline(lm(predictions_table$glmer[predictions_table$edu==2] ~ predictions_table$gp[predictions_table$edu==2]), col="blue")
    points(predictions_table$gp[predictions_table$edu == 3], predictions_table$glmer[predictions_table$edu == 3], col = "green", pch = 19)
    abline(lm(predictions_table$glmer[predictions_table$edu==3] ~ predictions_table$gp[predictions_table$edu==3]), col="green")
    points(predictions_table$gp[predictions_table$edu == 4], predictions_table$glmer[predictions_table$edu == 4], col = "yellow", pch = 19)
    abline(lm(predictions_table$glmer[predictions_table$edu==4] ~ predictions_table$gp[predictions_table$edu==4]), col="yellow")
    points(predictions_table$gp[predictions_table$edu == 5], predictions_table$glmer[predictions_table$edu == 5], col = "red", pch = 19)
    abline(lm(predictions_table$glmer[predictions_table$edu==5] ~ predictions_table$gp[predictions_table$edu==5]), col="red")
    legend("topleft", c("No HS","HS Graduate","Some College","College Graduate","Advanced Degree"), bty="y", pt.bg=c("purple","blue","green","yellow","red"),
           col=c("purple","blue","green","yellow","red"), pch = c(21,21), cex = 0.6)
    
    plot(seq(from = 0, to = 1, by = .05), seq(from = -.5, to = .5, by = .05), main = "Glmer vs. Difference in Predictions", xlab ="Glmer", ylab ="Difference", type = "n")
    points(predictions_table$glmer[predictions_table$eth == 2], predictions_table$difference[predictions_table$eth == 2], col = "yellow", pch = 16)
    points(predictions_table$glmer[predictions_table$eth != 2], predictions_table$difference[predictions_table$eth != 2], col = "black", pch = 16)
    abline(a=0, b=0, col = "black", lwd = 2)
    legend("topleft", c("African American","Not African American"), bty="y", pt.bg=c("yellow", "black"),
           col=c("yellow","black"), pch = c(21,21), cex = 0.5)
    
    fit<-lm(predictions_table$glmer ~ predictions_table$gp)
    dev.off()
    print(fit$coefficients[[2]])
    return(predictions_table) 
  }
  else {
    return(predictions_table)
  }
}

sample_selector(state_numbers = c(1,2,3,5,6,7), sample_n = 500, plots = 1)
sample_selector(state_numbers = c(2,4,20), sample_n = 500, plots = 1)




sample_selector_noAA = function(state_numbers, sample_n, plots){
  vote_data = read.delim("votingdata.dat") # Read in dataset from .dat file
  vote_data = na.exclude(vote_data) # Remove all entries with missing data
  
  vote_data <- vote_data[!(vote_data$stt==12),] # Removal of Hawaii entry from dataset
  vote_data$stt <- ifelse(vote_data$stt > 12, vote_data$stt - 1, vote_data$stt)
  vote_data$stt <- ifelse(vote_data$stt > 2, vote_data$stt - 1, vote_data$stt) # Recode stt value for states alphabetically after AK
  
  state_data = vote_data[vote_data$stt == state_numbers, c(1,2,3,6,7)]
  sample_data = state_data[sample(1:length(state_data$stt), sample_n),]
  sample_data = unique(sample_data[,c("stt", "eth", "sex", "edu")])
  sample_data = sample_data[sample_data$eth != 2,]
  
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
  
  jpeg("withoutAA")
  if (plots == 1){
    par(mfrow=c(2,2))
    plot(seq(from = 0, to = 1, by = .05), seq(from = 0, to = 1, by = .05), xlab = "GP", ylab = "Glmer", type = "n", 
         main = "Predictions compared by Ethnicity") #by ethnicity
    points(predictions_table$gp[predictions_table$eth == 1], predictions_table$glmer[predictions_table$eth == 1], col = "red", pch = 19)
    abline(lm(predictions_table$glmer[predictions_table$eth == 1] ~ predictions_table$gp[predictions_table$eth == 1]), col="red")
    points(predictions_table$gp[predictions_table$eth == 3], predictions_table$glmer[predictions_table$eth == 3], col = "green", pch = 19)
    abline(lm(predictions_table$glmer[predictions_table$eth == 3] ~ predictions_table$gp[predictions_table$eth == 3]), col="green")
    points(predictions_table$gp[predictions_table$eth == 4], predictions_table$glmer[predictions_table$eth == 4], col = "black", pch = 19) 
    abline(lm(predictions_table$glmer[predictions_table$eth == 4] ~ predictions_table$gp[predictions_table$eth == 4]), col="black")
    legend("topleft", c("White","Hispanic","Asian/Pacific Islander"), bty="y", pt.bg=c("red","yellow","green","black"),
           col=c("red","green","black"), pch = c(21,21), cex = 0.6)
    fit<-lm(predictions_table$glmer ~ predictions_table$gp)
    abline(fit, col="blue")
    
    plot(seq(from = 0, to = 1, by = .05), seq(from = 0, to = 1, by = .05), type = "n", xlab= "GP", ylab = "Glmer", 
         main = "Predictions compared by Sex") #by sex
    points(predictions_table$gp[predictions_table$sex == 1], predictions_table$glmer[predictions_table$sex == 1], col = "blue", pch = 19)
    abline(lm(predictions_table$glmer[predictions_table$sex==1] ~ predictions_table$gp[predictions_table$sex==1]), col="blue")
    points(predictions_table$gp[predictions_table$sex == 2], predictions_table$glmer[predictions_table$sex == 2], col = "pink", pch = 19)
    abline(lm(predictions_table$glmer[predictions_table$sex==2] ~ predictions_table$gp[predictions_table$sex==2]), col="pink")
    legend("topleft", c("Male","Female"), bty="y", pt.bg=c("blue","pink"),
           col=c("blue","pink"), pch = c(21,21), cex = 0.75)
    
    plot(seq(from = 0, to = 1, by = .05), seq(from = 0, to = 1, by = .05), type = "n", xlab= "GP", ylab = "Glmer", 
         main = "Predictions compared by Education") #by sex
    points(predictions_table$gp[predictions_table$edu == 1], predictions_table$glmer[predictions_table$edu == 1], col = "purple", pch = 19)
    abline(lm(predictions_table$glmer[predictions_table$edu==1] ~ predictions_table$gp[predictions_table$edu==1]), col="purple")
    points(predictions_table$gp[predictions_table$edu == 2], predictions_table$glmer[predictions_table$edu == 2], col = "blue", pch = 19)
    abline(lm(predictions_table$glmer[predictions_table$edu==2] ~ predictions_table$gp[predictions_table$edu==2]), col="blue")
    points(predictions_table$gp[predictions_table$edu == 3], predictions_table$glmer[predictions_table$edu == 3], col = "green", pch = 19)
    abline(lm(predictions_table$glmer[predictions_table$edu==3] ~ predictions_table$gp[predictions_table$edu==3]), col="green")
    points(predictions_table$gp[predictions_table$edu == 4], predictions_table$glmer[predictions_table$edu == 4], col = "yellow", pch = 19)
    abline(lm(predictions_table$glmer[predictions_table$edu==4] ~ predictions_table$gp[predictions_table$edu==4]), col="yellow")
    points(predictions_table$gp[predictions_table$edu == 5], predictions_table$glmer[predictions_table$edu == 5], col = "red", pch = 19)
    abline(lm(predictions_table$glmer[predictions_table$edu==5] ~ predictions_table$gp[predictions_table$edu==5]), col="red")
    legend("topleft", c("No HS","HS Graduate","Some College","College Graduate","Advanced Degree"), bty="y", pt.bg=c("purple","blue","green","yellow","red"),
           col=c("purple","blue","green","yellow","red"), pch = c(21,21), cex = 0.6)
    
    plot(seq(from = 0, to = 1, by = .05), seq(from = -.5, to = .5, by = .05), main = "Glmer vs. Difference in Predictions", xlab ="Glmer", ylab ="Difference", type = "n")
    points(predictions_table$glmer[predictions_table$eth == 2], predictions_table$difference[predictions_table$eth == 2], col = "yellow", pch = 16)
    points(predictions_table$glmer[predictions_table$eth != 2], predictions_table$difference[predictions_table$eth != 2], col = "black", pch = 16)
    abline(a=0, b=0, col = "black", lwd = 2)
    
    dev.off()
    print(fit$coefficients[[2]])
    return(predictions_table) 
  }
  else {
    return(predictions_table)
  }
}

sample_selector_noAA(state_numbers = c(1,2,3,5,6,7), sample_n = 500, plots = 1)
