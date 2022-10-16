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
  if (strategy == 1){
    results = rep(0,ngreps)
    for(i in 1:ngreps){
      # A random permutation:
      boxes = sample(1:(2*n),2*n)
      # Track the prisoners path
      path = c(k)
      tries = 1
      # Look first in the box that matches your own number
      inBox = boxes[k]
      while(tries < n){
        path = c(path, inBox)
        if(inBox == k){
          break;
        }else{
          # Follow that number to the next box
          inBox = boxes[inBox]
        }
        tries = tries+1 
      }
    }
  }else if(strategy == 2){
    results = rep(0,ngreps)
    for(i in 1:ngreps){
      # A random permutation:
      boxes = sample(1:(2*n),2*n)
      # Track the prisoners path
      path = c(k)
      tries = 1
      # Look first in the box that matches your own number
      inBox = sample(boxes, 1)
      while(tries < n){
        path = c(path, inBox)
        if(inBox == k){
          break;
        }else{
          # Follow that number to the next box
          inBox = boxes[inBox]
        }
        tries = tries+1 
      }
    }
  }else{
    for(i in 1:ngreps){
      # A random permutation:
      boxes = sample(1:(2*n),2*n)
      # create a copy of the boxes object to 
      #keep an index of the opened boxes for
      #each prisoner
      boxes.temp <- boxes
      #Track the prisoners path
      path = c(k)
      tries = 1
      # find a box randomly
      inBox = sample(boxes, 1)
      while(tries < n){
        path = c(path, inBox) 
        if(inBox == k){
          break; 	
        }else{
          # delete the first box from the box pool
          boxes.temp <- boxes.temp[boxes.temp!=inBox]	
          # choose randomly among any of the remaining boxes
          inBox = sample(boxes.temp,1)
        }
        tries = tries+1
      }
    }
    return(tries/2*n)
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
  pone(5,k,i,10000)
  pone(50,k,i,10000)
  pall(5,i,10000)
  pall(50,i,10000)
}

# ------- Surprising findings of the probabilities---------------------

# Intuitively, the probability of each prisoner completing the challenge is 1/2
# and each prisoner's challenge is an independent event, so the probability of 
# 100 prisoners succeeding at the same time is (1/2) ^100. However, the 
# probability of success in strategy 1 and 2 is not (1/2)^100.


# ------- Creating function for probabilities of 1 to 2n loop---------------------

dloop <- function(n,nreps){
  
}

# -------Estimating 100-vector of probabilities ---------------------

results <- dloop(100,10000)

# -------Visualization of the probabilities ---------------------

# load the libraries
library(ggplot2)
library(gridExtra)

# set the variable values
n=100
nreps=1000

p <- ggplot(data.frame(model=rep("model",nreps),
                        values=results)) +
  aes(x=values) +
  geom_histogram(stat="bin",binwidth=1,
                 fill="lightblue",colour="blue",alpha=0.3) + 
  labs(title="MODEL: Number of prisoners that succeeded the test",
       x="Success event",
       y="Frequency") 
