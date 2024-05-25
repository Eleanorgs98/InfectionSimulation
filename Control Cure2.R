#install.packages('Rtools')
#install.packages('abind')
#library(abind)

###################### functions ##############################
# function to check which directions are possible i.e. if on the boundary or not
direction_check <- function(x,y) {
  directions=c("North","South","East","West","Remain")
  if (y==a) {
    directions=directions[directions !="North"]
  }
  if (y==0) {
    directions=directions[directions !="South"]
  }
  if (x==a) {
    directions=directions[directions !="East"]
  }
  if (x==0) {
    directions=directions[directions !="West"]
  }
  return(directions)
}

# currently assuming all directions have equal probabilities
# function to move in correct direction one step
one_step <- function(x,y){
  directions <- direction_check(x,y)
  h <- directions[sample(1:length(directions),1)]
  if (h == "North") {y=y+1}
  else if (h == "South") {y=y-1}
  else if (h == "East") {x=x+1}
  else if (h == "West") {x=x-1}
  return(c(x,y))
}

add_locations <- function(x,y,j,count){
  locations_all[a+1-y,x+1] <<- locations_all[a+1-y,x+1]+1 # add location to all matrix
  if(animals[count+1,3,j] == 1){ 
    locations_infected[a+1-y,x+1,j] <<- locations_infected[a+1-y,x+1,j]+1 # add location to infected matrix if at the last step the animal was infected
  }
}

check_cure <- function(j,count){
  if (animals[count,3,j] == 1){
    r=runif(1,0,1)
    if (r <= P){
      animals[count+1,3,j] <<- 0
      }
  }
}

check_infected <- function(x,y,j,count){
  checkinf <- apply(locations_infected,c(1,2),sum)[a+1-y,x+1]
  if (checkinf != 0) {
    #print(c("check",j))
    animals[count+1,3,j] <<- 1
    #print(c("func",j,animals[count,3,j]))
  }
  }
###################### multiple experiments ################################
a=10
#N=500 # max number of steps
S=1000 # number of experiments
R=10 # number of uninfected animals (total number is R+1)
TT=100
#C= seq(from=1,to=10,by=1)
#C= seq(from=0.1,to=1.0,by=0.1)
#C= seq(from=0.001,to=0.01,by=0.001)
M=R+1
C=0.1
####### run ########
profit=vector()
for (c in C){  
  #P=(c-1)/(c+1)
  P=((2*exp(c))/(exp(c)+1))-1
  #P=((c-1)/(c+1))+1
  #P=c/(sqrt(1+c^2))
  times=vector()
  numberinfected=vector() # vector storing number of infected for each experiment

  
  for (i in 1:S){
    
    # create array - each page corresponds to one animal, each row is the step, first column is x coord, second column is y coord
    # third column is binary variable as to whether the animal is infected or not
    animals <- array(rep(0),c(1,3,R+1))
    animals[1,3,1] <- 1 # indicate the first infected animal is infected
    animals
    # create array - each page is a grid for one animal, containing all infected locations for each animal
    locations_infected <- array(rep(0),c(a+1,a+1,R+1)) 
    locations_infected[a+1,1,1] <- 1
    # matrix of all locations visited
    locations_all <- matrix(0,nrow=a+1,ncol=a+1) 
    locations_all[a+1,1] <- 1
    
    # vectors to store the locations at each time point of uninfected animal walks
    coords_x <- c(rep(0,R+1))
    coords_y <- c(rep(0,R+1))
    
    # set up R uninfected animals' initial locations and add to matrix tracking all locations
    for (j in 1:R){
      coords_x[j+1] <- sample(seq(0,a),1)
      coords_y[j+1] <- sample(seq(0,a),1)
      u <- coords_x[j+1]
      v <- coords_y[j+1]
      locations_all[a+1-v,u+1] <- locations_all[a+1-v,u+1]+1
      #print(locations_all)
    }
    animals[,1,] <- coords_x
    animals[,2,] <- coords_y
    
    count <- 1 # number of steps
    newinf <- c(0)
    
    repeat {
      newrow <- array(rep(0), c(1,3,R+1))
      animals <<- abind(animals, newrow, along=1)
      for (j in 1:M){
        if(animals[count,3,j] == 1){
          animals[count+1,3,j] <- 1
        }
       check_cure(j,count) # check which infected animals are cured
        x <- animals[count,1,j]
        y <- animals[count,2,j]
        s <- one_step(x,y)
        x <- s[1]
        y <- s[2]
        animals[count+1,1:2,j] <- c(x,y)
        check_infected(x,y,j,count) # check if now infected
        add_locations(x,y,j,count)
      }
      count <- count + 1 
      #print(animals[count,3,2])
      if (count >=TT) {break}
      if (sum(animals[,3,][count,]) == M){
        break
      }
    }
  countinfected <- sum(animals[,3,][count,]) # considers infected status at last time step for each animal
  numberinfected <- append(numberinfected,countinfected) # vector storing number of infected for each experiment
  times <- append(times,count) 
  assign(paste("numberinfected",c,sep=""),numberinfected)
  }
print(table(numberinfected))
profit <- append(profit,mean(M-numberinfected)-M*c)
print(profit)
}
profit2 <- profit

plot(C,profit)
par(mfrow=c(1,2))
C2 = seq(from=0.1,to=1.0,by=0.1)
C1 = seq(from=0.01,to=0.1,by=0.01)
plot(C1,profit1,pch=16)
plot(C2,profit2,pch=16)




