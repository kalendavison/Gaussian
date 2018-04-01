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

vote_data$man<-ifelse(vote_data$sex==1, c(1), c(0)) #recode sex to 0 1 dummy instead of 1 2
vote_data$mar<-ifelse(vote_data$mar==1, c(1), c(0)) #recode married to 0 1
vote_data$kid<-ifelse(vote_data$kid==1, c(1), c(0)) #recode kid to 0 1 

# vote_data$WM<-ifelse(vote_data$white==1 & vote_data$man==1, c(1), c(0)) #White Male combined variable
# vote_data$WF<-ifelse(vote_data$white==1 & vote_data$man==0, c(1), c(0)) #White female combined variable 
# vote_data$NWM<-ifelse(vote_data$white==0 & vote_data$man==1, c(1), c(0)) #Nonwhite Male combined variable 
# vote_data$NWF<-ifelse(vote_data$white==0 & vote_data$man==0, c(1), c(0)) #Nonwhite female combined variable 

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

# make a pretend data set with one instance of all possible configurations of demographics white male, black male, 
# feed into predict function, append

output<-gp(formula = rvote~rbf(c("white", "man")), data = vote.df25.reduced, family = binomial) ### compare output of this with lmer output. see pdf on doc for assistance
my.prediction<-predict(output, vote.df25, type="response") # replace vote.df25 with fake data set made above
my.prediction <- unique(my.prediction)
table(my.prediction)
# put demographic category w/ prediction into a table
as.data.frame(table(my.prediction)) #there are four possible probabilities of voting republican (associated with white male, nonwhite male, white female, nonwhite female)
plot(output$posterior$components$a, vote.df25.reduced$rvote)

output<-gp(formula = rvote~rbf(c("man", "white", "mar", "kid")), data = vote.df25.reduced, family = binomial)
my.prediction<-predict(output, vote.df25, type="response")
as.data.frame(table(my.prediction)) #there are 16 possibilities 
plot(output$posterior$components$a, vote.df25.reduced$rvote)



#working with glmer function
vote.df25.reduced<-vote.df25[,c("rvote", "white", "black", "hisp", "api", "male", "female", "noHS", "HSgrad", "somecollege", "bachelors","adv_degree")]
var1 = vote.df25.reduced$white
var2 = vote.df25.reduced$hisp
var3 = vote.df25.reduced$black
var4 = vote.df25.reduced$api
var5 = vote.df25.reduced$male
var6 = vote.df25.reduced$female
var7 = vote.df25.reduced$noHS
var8 = vote.df25.reduced$HSgrad
var9 = vote.df25.reduced$somecollege
var10 = vote.df25.reduced$bachelors
var11 = vote.df25.reduced$adv_degree


check = glmer(formula = rvote ~ (1|var1) + (1|var2) + (1|var3) + (1|var4) + (1|var5)
              + (1|var7) + (1|var8) + (1|var9) + (1|var10) + (1|var11), data = vote.df25.reduced, family = binomial) 
display(check) 

#the functionality of glmer seems to be working but not sure how to interpret, and inputs are prob formatted incorrectly
#the results suggest that being a woman makes you less likely to vote repub and that being white makes you more likely to vote repub

#fit the models with all the covariates - want 2 to 3 states, show how predictions do or don't match up
#extend the code out to the full set
#race/ethnicity sex state education

# 1 add dummy variables for education
# 2 change output of gp function to just return unique predictor values for each demographic subgroup
# 3 do gp function for 2 - 3 states, see how predictors hold up in other states - check for similarity
# 4 glmer: make all of the indicators random effect variables, enter all the variables

# use same strategy with glmer as we do with gp. make a fake dataset for predictions