# ------- Group member names----------------------------------------------------
# Jiayi Tu(s2306508)
# Xiaoyu Zhu(s2296761)
# Jingyun Shan(s1943324)

# -------- Individual Contributions --------------------------------------------
# 
# 
# 
# 
# 
# 

# Rough proportions: 

# ------- Outline of project----------------------------------------------------
# This project estimates the probabilities of finding numbers for a single prisoner and 
# total 2n prisoners under three different strategies. And it summarizes a surprising result using
# two examples of probabilities. We create a function to return a 2n-vector of probabilities for 
# 1 to 2n loops of sequence of cards, and use it to estimate the probabilities of 100 prisoners. 
# Finally, we plot probability density function diagrams for success of 100 prisoners.


# ------- Creating function for individual probability of finding number---------------------

pone <- function(n = 100, k = 1, strategy, ngreps = 10000){
  strategy = ["own_number", "first_random", "all_random"]
  results = rep(0,ngreps)
  for(i in 1:ngreps){
    # A random permutation:
    boxes = sample(1:(2*n),2*n)
    # Track the prisoners path
    path = c(k)
    tries = 1
    if (strategy = strategy[1]){
      # Look first in the box that matches your own number
      inBox = boxes[k]
    }else if (strategy = strategy[2,3]){
      inBox = sample(boxes, 1)
    }
    while(tries < n){
      path = c(path, inBox)
      if(inBox == k){
        break;
      }else{
        if (strategy = strategy[1,2]){
          # Follow that number to the next box
          inBox = boxes[inBox]
        }else if (strategy = strategy[3]){
          # delete the first box from the box pool
          boxes.temp <- boxes.temp[boxes.temp!=inBox]	
          # choose randomly among any of the remaining boxes
          inBox = sample(boxes.temp,1)
        }
      }
      tries = tries+1 
    }
    return(tries/(2*n))
  }
}

# ------- Creating function for joint probability of finding number---------------------

pall <- function(n = 100, strategy, nreps){
  for(round in 1:nreps){
    game  <- sapply(1:(2*n), function(k) Pone(n,k,srategy,1)) 
    roundpardoned <- sum(game)
    if (roundpardoned==(2*n){
      allpardoned = allpardoned + 1 
    }
    results[i]=allpardoned
  }
  return(results)
}

# ------- Estimate probabilities for n=5 and n=50---------------------

for (i in 1:3){
  pone(5, k, strategy[i], 10000)
  pone(50, k, strategy[i], 10000)
  pall(5, strategy[i], 10000)
  pall(50, strategy[i], 10000)
}

# ------- Surprising findings of the probabilities---------------------

# Intuitively, the probability of each prisoner completing the challenge is 1/2
# and each prisoner's challenge is an independent event, so the probability of 
# 100 prisoners succeeding at the same time is (1/2) ^100. However, the 
# probability of success in strategy 1 and 2 is not (1/2)^100.


# ------- Creating function for probabilities of 1 to 2n loop---------------------

dloop <- function(n=100,nreps=10000){
  p = rep(0,nreps)
  for (i in 1:nreps){
    for (i in 1:(2*n)){
      p[i] = (factorial(100)/i)/factorial(100)
    }
    return(p)
  }
}

# -------Estimating 100-vector of probabilities ---------------------

results <- dloop(50,10000)

# -------Visualization of the probabilities ---------------------

# load the libraries
library(ggplot2)
library(gridExtra)

# set the variable values
n=50
nreps=10000

ggplot(data.frame(model=rep("pall",nreps),
                  values=results)) +
    aes(y=values) +
    geom_density(aes(fill=as.factor(model),colour=as.factor(model)),alpha=0.3) + 
    labs(title="Density plot of number of prisoners that succeeded retrieving their number",
         x="Loop Length",
         y="Probability") 
