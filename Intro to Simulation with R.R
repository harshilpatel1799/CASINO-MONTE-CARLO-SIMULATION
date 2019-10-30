##################################
# Intro to Simulation - STAT 3201#
##################################

############
#References#
##########

#get the working directory
getwd()

#set the working directory. you can change it if you like; the file, along w/ any output, will now save to this location
#setwd("G:/OSU/2018 AU/STAT 3201")

#Use Monte Carlo simulation to estimate the prob of winning bets in roulette and the expected value, variance of the bets

###########
#Example 1#
###########

#Bet on a single number

#start by setting the seed; this ensures you'll get the same random process every time you run the code
set.seed(35)

#set the number of iterations; it's a good idea when you're starting a simulation to choose a small number of iterations
#once you're sure the code is working correctly, you can increase the number of iterations to a more reasonable number
n.sim <- 60000

#initialize some storage vectors

#n.wins = the number of wins (this is used to count the number of wins)
n.wins <- 0

#p.hat = the estimated prob of winning (that is, p.hat = n.wins/n.sim)
p.hat <-0

#choose a number upon which to bet
n.bet <- 10

#since the possible numbers are 00, 0, 1, 2, ..., 36 and we're going to be sampling from the vector 1, 2, 3, ..., 36, 37, 38
#it makes sense to convert or assign 0 to either 37 and 00 to 38 (or vice versa)
#R does not distinguish between 0 and 00; hence if either are chosen they will be assigned to 37 and, by default, the other
#will be assigned to 38 (wlog)

#the n.bet==0 is equal to a 1 in the mult if n.bet is 0 and is equal to 0 o.w. 
#hence, the line below maps 0 to 37 and leaves all other numbers as entered
n.bet <- n.bet + 37*(n.bet==0)

#build the wheel
numbers <- c(0:37)

#build a storage vector for the winning numbers
win.numbers <- vector()

#loops in R look like: for (i in 1:n.sim){}
#all of the commands that are to be done each time should be contained w/in the {}
#R will do the loop for each integer, i, between 1 and n.sim, inclusive (in other words, i is the index here)

#set up the loop
for (i in 1:n.sim){
  #generate a winning number for spin i by randomly choosing a number from 1,2, ..., 38
  #i=3
  win.numbers[i] <- sample(numbers,1)
  
  #if you won on spin i add 1 to n.wins, if not add 0
  n.wins <- n.wins + 1*(n.bet==win.numbers[i])   # you can add logical operations within expressions.
  
  #compute and store est'd win prob, phat, at iteration i
  p.hat[i] <- n.wins/i
  
  #close the loop!
}

#the estimated prob of a win can be obtained by either extracting the last entry in p.hat
p.hat[n.sim]
#or by dividing the number of wins by the number of iterations
n.wins/n.sim

#in this case we know the true win prob is 1/38 => we can compute the error or absolute error of the est

true.win.prob <- 1/38

error <- p.hat[n.sim] - (true.win.prob)
error

abs.error <- abs(error)
abs.error

#when you're estimating a prob in lieu of computing it this is not possible

#make a line plot of the est'd win prob by iteration with the true win prob superimposed to show convergence of phat
par(ps=20)
plot(1:n.sim, p.hat, type='l',lwd=3, col='blue',xlab='Iteration', ylab='Est. Win Prob.',main='Monte Carlo Est. of Win Prob.')
abline(h=true.win.prob, col='red', lwd=2)


###########
#Example 2#
###########

#compute the est'd expected win/loss for a bet using the est'd win prob from above

#start by setting the seed; this ensures you'll get the same random process every time you run the code
set.seed(35)

#choose an amount to bet (to keep it realistic, choose a positive integer between $1 and $500)
bet <- 25

#set the number of iterations; it's a good idea when you're starting a simulation to choose a small number of iterations
#once you're sure the code is working correctly, you can increase the number of iterations to a more reasonable number
n.sim <- 60000

#initialize some storage vectors

#win = the amount won each spin
win <- 0

#win.total = running total of amount won
win.total <- 0

#avg.win = running avg of amount won
avg.win <- 0

#choose a number upon which to bet
n.bet <- 10

#since the possible numbers are 00, 0, 1, 2, ..., 36 and we're going to be sampling from the vector 1, 2, 3, ..., 36, 37, 38
#it makes sense to convert or assign 0 to either 37 and 00 to 38 (or vice versa)
#R does not distinguish between 0 and 00; hence if either are chosen they will be assigned to 37 and, by default, the other
#will be assigned to 38 (wlog)

#the n.bet==0 is equal to a 1 in the mult if n.bet is 0 and is equal to 0 o.w. 
#hence, the line below maps 0 to 37 and leaves all other numbers as entered
n.bet <- n.bet + 37*(n.bet==0)

#build the wheel
numbers <- c(0:37)

#build a storage vector for the winning numbers
win.numbers <- vector()

#loops in R look like: for (i in 1:n.sim){}
#all of the commands that are to be done each time should be contained w/in the {}
#R will do the loop for each integer, i, between 1 and n.sim, inclusive (in other words, i is the index here)

#set up the loop
for (i in 1:n.sim){
  #generate a winning number for spin i by randomly choosing a number from 1,2, ..., 38
  win.numbers[i] <- sample(numbers,1)
  
  #store win total add 35 times the bet amount to the win total; if not, subtract the bet amount
  win[i] <- 35*bet*(n.bet==win.numbers[i]) - bet*(n.bet!=win.numbers[i])
  
  #compute and store running win total
  win.total[i] <- sum(win)
  
  #compute and store running win avg
  avg.win[i] <- sum(win)/i
  
  #close the loop!
}

#the estimated expected value of a win can be obtained by either extracting the last entry in avg.win
avg.win[n.sim]

#or by dividing the last entry of the total amount won by the number of iterations
win.total[n.sim]/n.sim

#in this case we know the true expected value is 35*bet*(1/38) - bet*(37/38) => we can compute the error or absolute error of the est

true.exp.value <- 35*bet*(1/38) - bet*(37/38)

error.ev <- avg.win[n.sim] - (true.exp.value)
error.ev

abs.error.ev <- abs(error.ev)
abs.error.ev


#make a line plot of the est'd avg win by iteration with the true expected win superimposed to show convergence of est'd exp value
par(ps=20)
plot(1:n.sim, avg.win, type='l',lwd=3, col='blue',xlab='Iteration', ylab='Est. Exp. Value',main='Monte Carlo Est. of Expected Value')
abline(h=true.exp.value, col='red', lwd=2)

#make a line plot of the total amount won by iteration with the break even point superimposed 
par(ps=20)
plot(1:n.sim, win.total, type='l',lwd=3, col='blue',xlab='Iteration', ylab='Total Amount Won',main='Monte Carlo Est. of Amount Won')
abline(h=0, col='red', lwd=2)
max(win.total)# highest amount you have ever won

###########
#Example 3#
###########

#compute the est'd expected value 

est.ex.value <- 35*bet*p.hat[n.sim] - bet*(1-p.hat[n.sim])
est.ex.value

#or 
est.ex.value2 <- avg.win[n.sim]
est.ex.value2

#compute the est'd variance
est.var <- ((35*bet)^2)*p.hat[n.sim] + (bet^2)*(1-p.hat[n.sim])
est.var

#compute the est'd standard deviation
est.sd <- sqrt(est.var)
est.sd