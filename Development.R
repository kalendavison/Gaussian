# Clear global environment
rm(list = ls())

library(devtools)

# Installation of necessary packages
install_github('goldingn/gpe')
install.packages("arm")
install.packages('lme4')

# Loading of necessary packages
library(arm)
library(gpe)
library(lme4)
?lme4
?gp
?display

# Set working directory
setwd("/Users/kalendavison/Desktop/Applied Statistical Programming/GitHub/Gaussian")
setwd("/Users/isdav/Documents/GitHub/Gaussian")
setwd("/Users/noahbardash/Documents/GitHub/Gaussian")

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

# State data subsets and cast as dataframes
votedata23<-subset(vote_data, vote_data$state.f23==1) # Mississippi subset
vote.df23<-as.data.frame(votedata23)
votedata2<-subset(vote_data, vote_data$state.f2==1) # Arizona subset
vote.df2<-as.data.frame(votedata2)
votedata20<-subset(vote_data, vote_data$state.f20==1) # Massachusetts subset
vote.df20<-as.data.frame(votedata20)
vote.df<-as.data.frame(vote_data) # Full dataset cast as dataframe


### GP FUNCTION ###

# Run GP function for MS, AZ, MA, full dataset
output_miss<-gp(formula = rvote~rbf(c("sex", "edu", "eth")), data = vote.df23, family = binomial)
output_ariz<-gp(formula = rvote~rbf(c("sex", "edu", "eth")), data = vote.df2, family = binomial)
output_mass<-gp(formula = rvote~rbf(c("sex", "edu", "eth")), data = vote.df20, family = binomial)
output_mass2<-gp(formula = rvote~rbf(c("sex", "edu", "eth"), sigma = 1, l = c(1,1,2)), data = vote.df20, family = binomial)
rm(list=setdiff(ls(), "vote.df")) #put this in to potentially help with the other function; doesnt work 
output_all<-gp(formula = rvote~rbf(c("sex", "edu", "eth", "stt")), data = vote.df, family = binomial)

# Creation of a fake dataset for each unique demographic combination 
eth = c(rep(1,10), rep(2,10), rep(3,10), rep(4,10))
sex = c(rep((c(rep(1,5), rep(2,5))), 4))
edu = rep(1:5, 8)
fake.dataset = data.frame(eth, sex, edu) # Cast fake dataset as dataframe

# Run GP function for each of the demographic subgroups to obtain 40 predicted R-vote values
predictions_miss<-predict(output_miss, fake.dataset, type="response")
predictions_ariz<-predict(output_ariz, fake.dataset, type="response")
predictions_mass<-predict(output_mass, fake.dataset, type="response")

# Display R-vote predictions alongside demographic categories in a dataframe
demographic.prediction.MS = data.frame(predictions_miss, fake.dataset)
View(demographic.prediction.MS) 
demographic.prediction.AZ = data.frame(predictions_ariz, fake.dataset)
View(demographic.prediction.AZ) 
demographic.prediction.MA = data.frame(predictions_mass, fake.dataset)
View(demographic.prediction.MA)


### GLMER FUNCTION ###


# Make a fake dataset for each unique demographic combination using dummy variables
white <- c(rep(1,10), rep(0,30))
black <- c(rep(0,10), rep(1,10), rep(0,20))
hisp <- c(rep(0,20), rep(1,10), rep(0,10))
api <- c(rep(0,30), rep(1,10))
male <- rep((c(rep(1,5), rep(0,5))), 4)
female <- rep((c(rep(0,5), rep(1,5))), 4)
noHS <- rep(c(1,0,0,0,0),8)
HSgrad <- rep(c(0,1,0,0,0),8)
somecollege <- rep(c(0,0,1,0,0),8)
bachelors <- rep(c(0,0,0,1,0),8)
adv_degree <- rep(c(0,0,0,0,1),8)
fake.dataset.2 = data.frame(white, black, hisp, api, male, female, noHS, HSgrad, somecollege, bachelors, adv_degree)

#MASSACHUSETTS
vote.df20<-vote.df20[,c("rvote", "eth", "sex", "edu")]

var1 = vote.df20$eth
var1 = as.factor(var1)
var2 = vote.df20$sex
var2 = as.factor(var2)
var3 = vote.df20$edu
var3 = as.factor(var3)


check = glmer(formula = rvote ~ (1|var1) + (1|var2) + (1|var3), data = vote.df20, family = binomial) 
display(check) 

glmer_predictions = predict(check, newdata = vote.df20, type="response")
glmer_predictions = round(glmer_predictions, digits = 7) #round for aesthetics and clear comparison
glmer_predictions = as.data.frame(table(glmer_predictions)) #creates a frequency column to match with gp later
glmer_predictions = glmer_predictions[order(glmer_predictions$Freq),] #order data frame by frequency
glmer_predictions

#compare to
gptest = gp(formula = rvote~rbf(c("sex", "edu", "eth")), data = vote.df20, family = binomial)
gp_predictions<-predict(gptest, vote.df20, type="response") #run gp through whole data set to get frequencies
gp_predictions = round(gp_predictions, digits = 7) #round for clear comparison
gp_predictions = as.data.frame(table(gp_predictions)) #access frequency
gp_predictions = gp_predictions[order(gp_predictions$Freq),] #order by frequency to match with glmer
gp_predictions

ordered = demographic.prediction.MA[order(demographic.prediction.MA$predictions_mass),] #order gp by fake.dataset to later add to master comparison
comparison = data.frame(gp_predictions$gp_predictions, glmer_predictions$glmer_predictions, ordered) #to be cleaned to make sense
comparison = comparison[order(comparison$gp_predictions),] #order by gp_predictions to match methods
comparison$gp_predictions.gp_predictions = NULL #no longer necessary because added predictions_mass
comparison = comparison[order(comparison$glmer_predictions.glmer_predictions),] #reorder 
comparison$glmer = comparison$glmer_predictions.glmer_predictions #rename for sense
comparison$glmer_predictions.glmer_predictions = NULL #no longer needed (just renamed)
comparison$gp = comparison$predictions_mass #rename for sense
comparison$predictions_mass = NULL #no longer needed (just renamed)
comparison$glmer = as.vector(comparison$glmer) #change from a factor to numeric for plotting purposes
View(comparison) #compares glmer and gp methods. Shows demographic group associated with each prediction.

###plots to figure out where the problems arise comparing the two groups###
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

#GP seems to generally overestimate probabilities while Glmer underestimates



#function takes in as arguments state number and desired sample size.It returns a data set 
# of random observations of the specified state and size.

sample_selector = function(state_number, sample_n){ 
  vote_data = read.delim("votingdata.dat") # Read in dataset from .dat file
  vote_data = na.exclude(vote_data) # Remove all entries with missing data
  
  vote_data <- vote_data[!(vote_data$stt==12),] # Removal of Hawaii entry from dataset
  vote_data$stt <- ifelse(vote_data$stt > 2, vote_data$stt - 1, vote_data$stt) # Recode stt value for states alphabetically after AK
  vote_data$stt <- ifelse(vote_data$stt > 12, vote_data$stt - 1, vote_data$stt)
  
  group = vote_data[vote_data$stt == state_number, 1:9]
  sample_data = group[sample(1:length(group$stt), sample_n),]
  sample_data = sample_data[,c("rvote", "eth", "sex", "edu")]
  
  gp_output<-gp(formula = rvote~rbf(c("sex", "edu", "eth")), data = sample_data, family = binomial)
  gp_predictions<-predict(gp_output, sample_data, type="response") 
  ?rbf
  eth = c(rep(1,10), rep(2,10), rep(3,10), rep(4,10))
  sex = c(rep((c(rep(1,5), rep(2,5))), 4))
  edu = rep(1:5, 8)
  fake.dataset = data.frame(eth, sex, edu)
  gp_predict<-predict(gp_output, fake.dataset, type="response") 
  demographic.predictions = data.frame(gp_predict, fake.dataset)
  
  gp_predictions = as.data.frame(table(gp_predictions)) 
  gp_predictions = gp_predictions[order(gp_predictions$Freq),]
  
  glmer_output = glmer(formula = rvote ~ (1|(as.factor(sample_data$eth))) + (1|(as.factor(sample_data$sex))) + (1|(as.factor(sample_data$edu))), data = sample_data, family = binomial) 
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
  View(comparison)
}

sample_selector(state_number = 20, sample_n = 2000) #Massachusetts, take out 300 observations and make a new data frame
#it almost works. It's just something about the glmer output

