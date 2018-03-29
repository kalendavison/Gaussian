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
state.f<-factor(vote_data$stt)
dummies<-model.matrix(~state.f)
vote_data<-cbind(vote_data, dummies)
vote_data$white<-ifelse(vote_data$eth==1, c(1), c(0))
vote_data$black<-ifelse(vote_data$eth==2, c(1), c(0))
vote_data$hisp<-ifelse(vote_data$eth==3, c(1), c(0))
vote_data$api<-ifelse(vote_data$eth==4, c(1), c(0))

vote_data$man<-ifelse(vote_data$sex==1, c(1), c(0)) #recode sex to 0 1 dummy instead of 1 2
vote_data$mar<-ifelse(vote_data$mar==1, c(1), c(0)) #recode married to 0 1
vote_data$kid<-ifelse(vote_data$kid==1, c(1), c(0)) #recode kid to 0 1 

vote_data$WM<-ifelse(vote_data$white==1 & vote_data$sex==1, c(1), c(0)) #White Male combined variable
vote_data$WF<-ifelse(vote_data$white==1 & vote_data$sex==0, c(1), c(0)) #White female combined variable 
vote_data$NWM<-ifelse(vote_data$white==0 & vote_data$sex==1, c(1), c(0)) #Nonwhite Male combined variable 
vote_data$NWF<-ifelse(vote_data$white==0 & vote_data$sex==0, c(1), c(0)) #Nonwhite female combined variable 

votedata25<-subset(vote_data, vote_data$state.f25==1) #using only 25th state for now

mean(votedata25$rvote[votedata25$eth == 1], na.rm = TRUE) #white mean republican vote proportion
mean(votedata25$rvote[votedata25$eth == 2], na.rm = TRUE) #black
mean(votedata25$rvote[votedata25$eth == 3], na.rm = TRUE) #asian/hispanic
mean(votedata25$rvote[votedata25$eth == 4], na.rm = TRUE) #asian/hispanic
mean(votedata25$rvote[votedata25$sex == 1], na.rm = TRUE) #male republican vote proportion
mean(votedata25$rvote[votedata25$sex == 2], na.rm = TRUE) #female

#basic multivariate regression analysis
#we need to make dummy variables for ethnicity to isolate its effect
output = lm(rvote ~ white, data = votedata25)
output
output = lm(rvote ~ white + sex, data = votedata25)
output #being white has a stronger affect on voting republican

#using gp function to do analysis
vote.df25<-as.data.frame(votedata25)
vote.df25.reduced<-vote.df25[,c("rvote", "white", "man", "mar", "kid")]
output<-gp(formula = rvote~rbf(c("white", "man")), data = vote.df25.reduced, family = binomial) ### compare output of this with lmer output. see pdf on doc for assistance
my.prediction<-predict(output, vote.df25, type="response")
table(my.prediction) #there are four possible probabilities of voting republican (associated with white male, nonwhite male, white female, nonwhite female)
plot(output$posterior$components$a, vote.df25.reduced$rvote)
plot(my.prediction, vote.df25$white)

<<<<<<< HEAD
output<-gp(formula = rvote~rbf(c("white", "man", "mar", "kid")), data = vote.df25.reduced, family = binomial)
my.prediction<-predict(output, vote.df25, type="response")
table(my.prediction) #there are 16 possibilities 
plot(output$posterior$components$a, vote.df25.reduced$rvote)


=======
?glmer
>>>>>>> b407ed7b049707836f6c1705124f22a1abac0167

#working with glmer function
var1 = as.factor(vote.df25.reduced$white)
var2 = as.factor(vote.df25.reduced$man)
var3 = as.factor(vote.df25.reduced$mar)
check = glmer(formula = rvote ~ (1|kid) + var1 + var2, data = vote.df25.reduced, family = binomial) #may need more variables (white man...nonwhite woman)
display(check) 
plot(check)
#the functionality of glmer seems to be working but not sure how to interpret, and inputs are prob formatted incorrectly
#the results suggest that being a woman makes you less likely to vote repub and that being white makes you more likely to vote repub
