##MONTE CARLO##

expert <- rnorm(100, 0, 0.2)
actual <- rnorm(100, 0, 0.2)

#Are the experts predictions better than random according to Norm with mean=0, sd=0.2?
#H0: random
#HA: better than random 

#Test stat: average absolute difference between expert and actual
obs.dif <- mean(abs(expert-actual))
obs.dif

#Run 10000 sims 
nsim <- 10000
sim_differences <- numeric(nsim)
random <- numeric(100)
alpha <- .05

#Try multiple testing! Should have alpha*m tests where we reject (.05*100=5)
ntests <- 100
pvals <- numeric(ntests)
num_reject <- 0

for(j in 1:ntests){
  #simulate under null (h0), norm mean=0, sd=0.2
  for(i in 1:nsim){
    random <- rnorm(100, 0, 0.2)
    sim_differences[i] <- mean(abs(random-actual))
  }
  
  #Reject if expert has lower difference from actual than random does
  pvals[j] <- (sum(sim_differences>=obs.dif)+1)/(nsim+1)
  
  if(pvals[j] <= alpha){num_reject = num_reject+1}
}

num_reject
