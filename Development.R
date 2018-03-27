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

#vote_data$state.f1<-

#vote_data$state.f2

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

<<<<<<< HEAD
=======
?rbf

kernels = function(x, prime, sigma){ #kernels function to inputted in GP
  out = exp(-((abs(x-prime))^2)/(2*(sigma)^2))
  return(out)
}
<<<<<<< HEAD
kernels(1:10,rep(2,10),3)
=======
?rbf
>>>>>>> 6a60ad685dcb664e2bfcb6bd0b9bf6c5a06228f7


vote.df5<-as.data.frame(votedata5)
vote.df5.reduced<-vote.df[,c("rvote", "white")]
vote.df5.reduced<-na.exclude(vote.df.reduced)



output<-gp(rvote~rbf("white") , data = vote.df5.reduced , family = binomial)

plot(output$posterior$components$a, vote.df5.reduced$rvote)


my.prediction<-predict(output, vote.df5, type="response")
plot(my.prediction, vote.df5$white)
vote.df5$rvote

<<<<<<< HEAD
as.data.frame(votedata5$rvote)
=======
as.data.frame(votedata25$rvote)
>>>>>>> 6a0d0869b1da1def66c2e13733bd86109206aae5
>>>>>>> 6a60ad685dcb664e2bfcb6bd0b9bf6c5a06228f7
