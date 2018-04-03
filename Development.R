rm(list = ls())

#setting up
library(devtools)
install_github('goldingn/gpe')
install.packages("arm")
library(arm)
?lmer
library(gpe)
install.packages('lme4')
library(lme4)
?gp

getwd()
setwd("/Users/kalendavison/Desktop/Applied Statistical Programming")
setwd("/Users/isdav/Documents/GitHub/Gaussian")
setwd("/Users/noahbardash/Documents/GitHub/Gaussian")
vote_data = read.delim("votingdata.dat") #read in dataset



### recoding dataset for analysis
vote_data = na.exclude(vote_data) #cleaning of all missing data
vote_data <- vote_data[!(vote_data$stt==12),] #removal of Hawaii
state.f<-factor(vote_data$stt)
dummies<-model.matrix(~state.f)
vote_data<-cbind(vote_data, dummies)
vote_data$white<-ifelse(vote_data$eth==1, c(1), c(0))
vote_data$black<-ifelse(vote_data$eth==2, c(1), c(0))
vote_data$hisp<-ifelse(vote_data$eth==3, c(1), c(0))
vote_data$api<-ifelse(vote_data$eth==4, c(1), c(0))

# dummy variables for sex
vote_data$male <- ifelse(vote_data$sex==1, c(1), c(0))
vote_data$female <- ifelse(vote_data$sex==2, c(1), c(0))

# order might be off, and these might not be proper categories
# dummy variables for education
vote_data$noHS <- ifelse(vote_data$edu==1, c(1), c(0))
vote_data$HSgrad <- ifelse(vote_data$edu==2, c(1), c(0))
vote_data$somecollege <- ifelse(vote_data$edu==3, c(1), c(0))
vote_data$bachelors <- ifelse(vote_data$edu==4, c(1), c(0))
vote_data$adv_degree <- ifelse(vote_data$edu==5, c(1), c(0))

vote_data$mar<-ifelse(vote_data$mar==1, c(1), c(0)) #recode married to 0 1
vote_data$kid<-ifelse(vote_data$kid==1, c(1), c(0)) #recode kid to 0 1 

# vote_data$WM<-ifelse(vote_data$white==1 & vote_data$man==1, c(1), c(0)) #White Male combined variable
# vote_data$WF<-ifelse(vote_data$white==1 & vote_data$man==0, c(1), c(0)) #White female combined variable 
# vote_data$NWM<-ifelse(vote_data$white==0 & vote_data$man==1, c(1), c(0)) #Nonwhite Male combined variable 
# vote_data$NWF<-ifelse(vote_data$white==0 & vote_data$man==0, c(1), c(0)) #Nonwhite female combined variable 

votedata25<-subset(vote_data, vote_data$state.f25==1) #using only 25th state for now - Mississippi
votedata3<-subset(vote_data, vote_data$state.f3==1) #using only 3rd state - Arizona
votedata22<-subset(vote_data, vote_data$state.f22==1) #Massachusetts


#using 
vote.df25<-as.data.frame(votedata25) #Mississippi
vote.df3<-as.data.frame(votedata3) #Arizona
vote.df22<-as.data.frame(votedata22) #Massachusetts
#vote.df25$sex = as.factor(vote.df25$sex)
#vote.df25$edu = as.factor(vote.df25$edu)
#vote.df25$eth = as.factor(vote.df25$eth) #changing them to factors DOES NOT WORK
output_miss<-gp(formula = rvote~rbf(c("sex", "edu", "eth")), data = vote.df25, family = binomial)
output_ariz<-gp(formula = rvote~rbf(c("sex", "edu", "eth")), data = vote.df3, family = binomial)
output_mass<-gp(formula = rvote~rbf(c("sex", "edu", "eth")), data = vote.df22, family = binomial)

#make a fake dataset for each unique demographic combination 
eth = c(rep(1,10), rep(2,10), rep(3,10), rep(4,10))
sex = c(rep((c(rep(1,5), rep(2,5))), 4))
edu = rep(1:5, 8)
fake.dataset = data.frame(eth, sex, edu)
predictions_miss<-predict(output_miss, fake.dataset, type="response")
predictions_ariz<-predict(output_ariz, fake.dataset, type="response")
predictions_mass<-predict(output_mass, fake.dataset, type="response")

demographic.prediction.MS = data.frame(predictions_miss, fake.dataset)
View(demographic.prediction.MS) 
demographic.prediction.AZ = data.frame(predictions_ariz, fake.dataset)
View(demographic.prediction.AZ) 
demographic.prediction.MA = data.frame(predictions_mass, fake.dataset)
View(demographic.prediction.MA)

plot(demographic.prediction.MS$predictions_miss) #MS voting patterns are highly related to ethnicity
plot(demographic.prediction.AZ$predictions_ariz)
plot(demographic.prediction.MA$predictions_mass)


#working with glmer function

#make a fake dataset for each unique demographic combination
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
View(fake.dataset.2)


### MISSISSIPPI
vote.df25<-vote.df25[,c("rvote", "eth", "sex", "edu")]

var1 = vote.df25$eth
var1 = as.factor(var1)
var2 = vote.df25$sex
var2 = as.factor(var2)
var3 = vote.df25$edu
var3 = as.factor(var3)


check = glmer(formula = rvote ~ (1|var1) + (1|var2) + (1|var3), data = vote.df25, family = binomial) 
display(check) 

dim(vote.df25)
glmer_predictions = predict(check, newdata = vote.df25, type="response")
glmer_predictions = round(glmer_predictions, digits = 10)
glmer_predictions = as.data.frame(table(glmer_predictions))
glmer_predictions = glmer_predictions[order(glmer_predictions$Freq),] #order data frame by frequency
glmer_predictions
#compare to
gptest = gp(formula = rvote~rbf(c("sex", "edu", "eth")), data = vote.df25, family = binomial)
gp_predictions<-predict(gptest, vote.df25, type="response")
gp_predictions = round(gp_predictions, digits = 10)
gp_predictions = as.data.frame(table(gp_predictions))
gp_predictions = gp_predictions[order(gp_predictions$Freq),]
gp_predictions

comparison = data.frame(glmer_predictions$glmer_predictions, gp_predictions$gp_predictions) #direct comparison between two methods. The predictions are sometimes close and sometimes not.
comparison = comparison[order(comparison$glmer_predictions.glmer_predictions),]
comparison
#37 observations instead of 40 because there are some missing demographic groups in the Mississippi data set




#MASSACHUSETTS
vote.df22<-vote.df22[,c("rvote", "eth", "sex", "edu")]

var1 = vote.df22$eth
var1 = as.factor(var1)
var2 = vote.df22$sex
var2 = as.factor(var2)
var3 = vote.df22$edu
var3 = as.factor(var3)


check = glmer(formula = rvote ~ (1|var1) + (1|var2) + (1|var3), data = vote.df22, family = binomial) 
display(check) 

dim(vote.df22)
glmer_predictions = predict(check, newdata = vote.df22, type="response")
glmer_predictions = round(glmer_predictions, digits = 10)
glmer_predictions = as.data.frame(table(glmer_predictions))
glmer_predictions = glmer_predictions[order(glmer_predictions$Freq),] #order data frame by frequency
glmer_predictions
#compare to
gptest = gp(formula = rvote~rbf(c("sex", "edu", "eth")), data = vote.df22, family = binomial)
gp_predictions<-predict(gptest, vote.df22, type="response")
gp_predictions = round(gp_predictions, digits = 10)
gp_predictions = as.data.frame(table(gp_predictions))
gp_predictions = gp_predictions[order(gp_predictions$Freq),]
gp_predictions

comparison = data.frame(glmer_predictions$glmer_predictions, gp_predictions$gp_predictions) #direct comparison between two methods. The predictions are sometimes close and sometimes not.
comparison = comparison[order(comparison$glmer_predictions.glmer_predictions),]
comparison
