rm(list = ls())

library(devtools)
install_github('goldingn/gpe')
library(gpe)
install.packages('lme4')
?gp

getwd()
setwd("/Users/kalendavison/Desktop/Applied Statistical Programming")
setwd("/Users/noahbardash/Documents/GitHub/Gaussian")
vote_data = read.delim("votingdata.dat")
mean(vote_data$rvote, na.rm = TRUE)


vote_data = na.exclude(vote_data) #cleaning of all missing data
summary(vote_data$stt)

state.f<-factor(vote_data$stt)
dummies<-model.matrix(~state.f)

vote_data$white<-ifelse(vote_data$eth==1, c(1), c(0))
vote_data$black<-ifelse(vote_data$eth==2, c(1), c(0))
vote_data$hisp<-ifelse(vote_data$eth==3, c(1), c(0))
vote_data$api<-ifelse(vote_data$eth==4, c(1), c(0))

vote_data$male<-ifelse(vote_data$sex==1, c(1), c(0))
vote_data$female<-ifelse(vote_data$sex==2, c(1), c(0))

vote_data<-cbind(vote_data, dummies)

summary(vote_data$state.f25)

votedata25<-subset(vote_data, vote_data$state.f25==1)

mean(votedata25$rvote[votedata25$eth == 1], na.rm = TRUE) #white mean republican vote proportion
mean(votedata25$rvote[votedata25$eth == 2], na.rm = TRUE) #black
mean(votedata25$rvote[votedata25$eth == 3], na.rm = TRUE) #asian/hispanic
mean(votedata25$rvote[votedata25$eth == 4], na.rm = TRUE) #asian/hispanic
mean(votedata25$rvote[votedata25$sex == 1], na.rm = TRUE) #male republican vote proportion
mean(votedata25$rvote[votedata25$sex == 2], na.rm = TRUE) #female

output = lm(rvote ~ eth, data = votedata25) #we need to make dummy variables for ethnicity to isolate its effect
output = lm(rvote ~ white, data = votedata25)
output
output = lm(rvote ~ white + sex, data = votedata25)
output #being white has a stronger affect on voting republican

vote.df25<-as.data.frame(votedata25)
head(vote.df25)
View(vote.df25)
vote.df25.reduced<-vote.df25[,c("rvote", "white", "sex")]
head(vote.df25.reduced)
output<-gp(formula = rvote~rbf("white"), data = vote.df25.reduced, family = binomial)
plot(output$posterior$components$a, vote.df25.reduced$rvote)
?rbf

test <- expand.grid(vote.df25.reduced$white, vote.df25.reduced$male)
test

my.prediction<-predict(output, vote.df25, type="response")
plot(my.prediction, vote.df25$white)

