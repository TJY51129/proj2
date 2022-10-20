# ------- Group Member Names----------------------------------------------------
# Jiayi Tu(s2306508)
# Xiaoyu Zhu(s2296761)
# Jingyun Shan(s1943324)

# -------- Individual Contributions --------------------------------------------
##All steps were discussed among the group before writing the code. 
##Xiaoyu Zhu wrote the code for steps 1 and 2 and added comments. 
##Jingyun Shan wrote the code for steps 3 and 4 and added comments. 
##Jiayi Tu wrote the code for steps 5 and 6 and added comments. 
##At each step, two people other than the person responsible for 
##writing the code will test and check and suggest changes.


# ------- Outline of Project----------------------------------------------------
#Problem:
#This project analyzes the problem of 2n prisoners and 2n boxes by estimating 
#the probabilities of success under three strategies. In this problem, each box 
#was printed with the number 1 to 2n, and it contained a card with numbers from 
#1 to 2n. The 2n prisoners are required to find their numbers on cards within n 
#times. 

#Strategy:
#1st strategy: The prisoner firstly opens the box with their numbers, and if
#their number is not on the card in the box, then find the box painted with the 
#number on the card and repeat the process until they find the card with their 
#number. If they repeat less than n times and see it, all prisoners will be 
#freed.

#2nd strategy: The prisoner firstly opens a random box, and if their number is 
#not on the card in the box, then repeat the same process as 1st strategy.

#3rd strategy: The 2n prisoners open n random boxes individually, and if the n
#boxes contain their own number on the cards, then all prisoners will be 
#released.

#Solving process:
#This project estimates the probabilities of finding numbers for a single 
#prisoner and total 2n prisoners under three different strategies. And it 
#summarises a surprising result using two examples of probabilities (n=5 and 
#n=50). We create a function to return a 2n-vector of the probability of each 
#loop length from 1 to 2n occurring at least once and use it to estimate the 
#probabilities of 100 prisoners. Finally, we visualise the result by making 
#probability plots for the success of 100 prisoners.



##------------------------------------------------------------------------
## Define followBox function for strategy 1
## ------------------------------------------------------------------------
followBox <- function(n,k,boxes){
  #The first strategy: within a limited number of n attempts, 
  #the prisoner opens the box with their own number k, 
  #and opens a new box according to the number in the box. 
  #If the number contained in the opened box matches the prisoner's own, 
  #the prisoner is rejected. Release and output result 1, 
  #otherwise output result 0  
  pardoned <- 0
  for(i in 1:n) {
    if(i==1) {
      openbox <- k       ## start with box with k
    } else {
      openbox <- boxes[lastbox]
    }
    if(boxes[openbox]==k){
      pardoned  <- 1 ##be released and return 1 if open the box with his number
      break 
    } 
    lastbox <- openbox   #if the prisoner did not find the number K,
  }                      #loop with following the number in the opened box
  return(pardoned)
}

##------------------------------------------------------------------------
## Define randomBox function for strategy 2
## ------------------------------------------------------------------------
randomBox <- function(n,k,boxes) {
  #Change the function of strategy of 1
  #Prisoners choose the first box by random now.
  pardoned <- 0
  for(i in 1:n) {
    if(i==1) {
      openbox <- sample(2*n,1) ## start with box with random
    } else {
      openbox <- boxes[lastbox] 
    }
    if(boxes[openbox]==k){
      pardoned <- 1  ##be released and return 1 if open the box with his number
      break
    } 
    lastbox <- openbox       #if the prisoner did not find the number K,
  }                          #loop with following the number in the opened box
  return(pardoned)  
}

##------------------------------------------------------------------------
## Define Pone function to estimate the probability of a single prisoner 
## ------------------------------------------------------------------------

# Run some simulations to test the 3 different strategies
Pone <- function(n,k,strategy,nreps=10000) {
  pardoned <- 0
  
  if (strategy==1){   ##strategy 1
    for( r in 1:nreps){
      boxes <- sample(1:(2*n))  #Generate 2n boxes and use them every time
      #If the prisoner is released, adding a 1 to pardoned
      pardoned <- pardoned + followBox(n,k,boxes)   
    }                                              
  } 
  
  else if (strategy==2) { ##strategy 2
    for( r in 1:nreps){
      boxes <- sample(1:(2*n))        #Generate 2n boxes and use them every time
      #If the prisoner is released, adding a 1 to pardoned
      pardoned <- pardoned + randomBox(n,k,boxes)  
    }                                             
  } 
  
  else { ##strategy 3: opening n random boxes to find prisoner's number
    for( r in 1:nreps){
      #select n boxes from 2n random boxes to open
      openedboxes <- sample(1:(2*n),n)
      #If k is in the selected opened n boxes, adding a 1 to pardoned
      if(k %in% openedboxes) pardoned <- pardoned + 1  
    }                               
  }                                                
  return(pardoned/nreps)  #output the probability of individual success
}

##------------------------------------------------------------------------
## Define Pall function to estimate the probability of all prisoners 
## ------------------------------------------------------------------------

Pall <- function(n,strategy,nreps=10000){
  #The function used followBox and randomBox functions for nreps times to 
  #estimate the probabilities of releasing all 2n prisoners in three different 
  #strategies.
  result <- rep(0,nreps)
  
  if (strategy==1){            
    for(i in 1:nreps){
      boxes <- sample(1:(2*n))
      #return results 0 or 1 of single simulations by 
      #looping k in followBox function from 1 to 2n 
      game  <- sapply(1:(2*n), function(k) followBox(n,k,boxes)) 
      result[i] <- all(game==1)*1 
    }
  } 
  
  else if (strategy==2) {
    for(i in 1:nreps){
      boxes <- sample(1:(2*n))
      #return results 0 or 1 of single simulations by 
      #looping k in randomBox function from 1 to 2n 
      game  <- sapply(1:(2*n),function(k) randomBox(n,k,boxes)) 
      #In the ith simulation, if all 2n results are 1,
      #i.e all prisoners are released, return the ith result as 1
      result[i] <- all(game==1)*1
    }  
  } 
  
  else {#strategy 3
    for(i in 1:nreps){
      pardoned <- 0
      for (k in 1:(2*n)){
        #open n random boxes from 2n boxes
        openedboxes <- sample(1:(2*n),n)
        #if opened the box with number k, adding a 1 to pardoned
        if(k %in% openedboxes) pardoned <- pardoned + 1  ## free + 1
      }
      #In the ith simulation, if the sum of pardoned is 2n,
      #i.e all prisoners are released, return the ith result as 1
      if (pardoned==2*n)  result[i] = 1
    }
  }
  return(mean(result)) #output the probability of joint success
}


##-----------------------------------------------------------------------------
## Estimate probabilities for a single prisoner and all prisoners when n=5 and 
## n=50
## ----------------------------------------------------------------------------
k <- 1
nreps <- 10000
for (i in 1:3){
  result <- Pone(5,1,i,10000)
  cat("Pone n=5,k=1,strategy=",i,",nreps=10000 :",result,"\n") 
  
  result <-Pone(50,1,i,10000)
  cat("Pone n=50,k=1,strategy=",i,",nreps=10000 :",result,"\n")
  
  result <-Pall(5,i,10000)  ##be surprised,the probability is not (1/2)^(2*n) 
  cat("Pall n=5,strategy=",i,",nreps=10000 :",result,"\n")
  
  result <-Pall(50,i,10000)
  cat("Pall n=50,strategy=",i,",nreps=10000 :",result,"\n")
}

##------------------------------------------------------------------------
## Surprising findings of the probabilities
## ------------------------------------------------------------------------

# Intuitively, the probability of each prisoner completing the challenge is 1/2
# and each prisoner's challenge is an independent event, so the probability of 
# 100 prisoners succeeding at the same time is (1/2) ^100. However, the 
# probability of success in strategy 1 is much larger than (1/2)^100.


##------------------------------------------------------------------------
## Define dloop function to estimate probabilities of 1 to 2n loop
## ------------------------------------------------------------------------

dloop <- function(n,nreps){
  result <- rep(0,2*n)
  tmp <- rep(0,2*n)
  for (r in 1:nreps){
    boxes <- sample(2*n)
    
    for (i in 1:(2*n)){
      isLoop <-TRUE 
      openbox <- i
      lcount <-0
      
      while(isLoop){
        #for each 2n box, we loop by matching the number on the box lid with 
        #the number on the cards and get the length of sequence of cards
        
        if (boxes[openbox]==i) isLoop <- FALSE
        else openbox <- boxes[openbox]
        lcount <- lcount+1
      }
      tmp[i] <- lcount
    }
    
    maxLoop <- max(tmp) #find the maximum length of loop
    #in each simulation, assign the ith result as 1 if the ith length is 
    #the largest
    result[maxLoop] <- result[maxLoop]+1 
  }
  result <- result/sum(result)
  return(result) 
}

##------------------------------------------------------------------------
## Estimating 100-vector of probabilities
## ------------------------------------------------------------------------

n <- 50
nreps <-10000
probability <- dloop(n,nreps)

##------------------------------------------------------------------------
## Visualization of the probabilities
## ------------------------------------------------------------------------



#The red line separates the probability of success 
#from the probability of failure
plot(1:(2*n),probability,type="l",
     xlab="length of longest loop",main="Longest loop probability distribution")
abline(v=n, lty=2, col="red", lwd=2)   

#sum of the probabilities in red color is the probability of failure
#sum of the probabilities in green color is the probability of success
barplot(probability,names.arg = 1:(2*n),
        xlab="length of longest cycle",ylab="probability",
        main = "longest cycle probability ",
        col=c(rep("green",n),rep("red",n)))
