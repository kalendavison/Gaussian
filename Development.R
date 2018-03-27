library(devtools)
install_github('goldingn/gpe')
library(gpe)
?gp

getwd()
setwd("/Users/kalendavison/Desktop/Applied Statistical Programming")
vote_data = read.delim("votingdata.dat")
mean(vote_data$rvote, na.rm = TRUE)


vote_data = na.exclude(vote_data) #cleaning of all missing data
View(vote_data)
summary(vote_data$stt)

state.f<-factor(vote_data$stt)
dummies<-model.matrix(~state.f)

vote_data$white<-ifelse(vote_data$eth==1, c(1), c(0))
vote_data$black<-ifelse(vote_data$eth==2, c(1), c(0))
vote_data$hisp<-ifelse(vote_data$eth==3, c(1), c(0))
vote_data$api<-ifelse(vote_data$eth==4, c(1), c(0))

vote_data<-cbind(vote_data, dummies)

summary(vote_data$state.f5)

votedata5<-subset(vote_data, vote_data$state.f5==1)
View(votedata5)

mean(votedata5$rvote[votedata5$eth == 1], na.rm = TRUE) #white mean republican vote proportion
mean(votedata5$rvote[votedata5$eth == 2], na.rm = TRUE) #black
mean(votedata5$rvote[votedata5$eth == 3], na.rm = TRUE) #asian/hispanic
mean(votedata5$rvote[votedata5$eth == 4], na.rm = TRUE) #asian/hispanic

output = lm(rvote ~ eth, data = votedata5) #we need to make dummy variables for ethnicity to isolate its effect
output = lm(rvote ~ white, data = votedata5)
output
output = lm(rvote ~ white + sex, data = votedata5)
output #being white has a stronger affect on voting republican

vote.df5<-as.data.frame(votedata5)
vote.df5.reduced<-vote.df5[,c("rvote", "white")]

output<-gp(rvote~rbf("white") , data = vote.df5.reduced , family = binomial)

plot(output$posterior$components$a, vote.df5.reduced$rvote)


my.prediction<-predict(output, vote.df5, type="response")
plot(my.prediction, vote.df5$white)
vote.df5$rvote

as.data.frame(votedata5$rvote)

