library(devtools)
install_github('goldingn/gpe')
library(gpe)
?gp

getwd()
setwd("/Users/kalendavison/Desktop/Applied Statistical Programming")
vote_data = read.delim("votingdata.dat")
mean(vote_data$rvote, na.rm = TRUE)

output = lm(rvote ~ eth + stt, data = vote_data)
output 

#########

# Create an rbf kernel which acts on some variable named temperature
k1 <- rbf('temperature')
# look at the parameters
summary(k1)
# plot covariance
plot(k1)
# look at some GPs drawn from this kernel
demoKernel(k1)

# make a fake 'true' function
f <- function(x) 2 * sin(x)

# make a fake dataset
x <- sort(runif(100, -2, 2))
y <- rpois(100, exp(f(x)))
df <- data.frame(y, x)

# fit a Poisson GP model with an rbf kernel
m <- gp(y ~ rbf('x'), data = df, family = poisson)

# predict from it
pred_df <- data.frame(x = seq(min(df$x), max(df$x), len = 500))
lambda <- predict(m, pred_df, type = 'response')

# plot the predicted rate parameter, the true model and the data
plot(lambda ~ pred_df$x, type = 'l', lwd = 2, ylim = range(y))
lines(exp(f(pred_df$x)) ~ pred_df$x, lty = 2)
points(y ~ x, data = df)

# note you can get the posterior variance (prediction uncertainty) too,
# just set 'sd = TRUE' when predicting

gp(formula = )

#############

View(vote_data)
summary(vote_data$stt)

state.f<-factor(vote_data$stt)
dummies<-model.matrix(~state.f)
View(dummies)

#vote_data$state.f1<-

#vote_data$state.f2


vote_data<-cbind(vote_data, dummies)

summary(vote_data$state.f30)

votedata30<-subset(vote_data, vote_data$state.f30==1)
View(votedata30)

