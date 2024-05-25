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

check_infected <- function(x,y,j,count){
  checkinf <- apply(locations_infected,c(1,2),sum)[a+1-y,x+1]
  if (checkinf != 0) {
    animals[count+1,3,j] <<- 1
  }
}
###################### multiple experiments ################################
par(mfrow=c(1,2))
a=20
N=1000 # max number of steps
S=1000 # number of experiments
R=20 # number of uninfected animals (total number is R+1)
M=R+1

####### run ########
times=vector()
for (i in 1:S){
  # create array - each page corresponds to one animal, each row is the step, first column is x coord, second column is y coord
  # third column is binary variable as to whether the animal is infected or not
  animals <- array(rep(0),c(1,3,R+1))
  animals[1,3,1] <- 1 # indicate the first infected animal is infected
  animals
  # create array - each page is grid for one animal, containing all infected locations for each animal
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
    coords_x[j+1] <- sample(c(0,seq(1:a)),1)
    coords_y[j+1] <- sample(c(0,seq(1:a)),1)
    u <- coords_x[j+1]
    v <- coords_y[j+1]
    locations_all[a+1-v,u+1] <- locations_all[a+1-v,u+1]+1
  }
  animals[,1,] <- coords_x
  animals[,2,] <- coords_y
  
  count <- 1 # number of steps
  
  newinf <- c(0)
  
  repeat {
    newrow <- newrow <- array(rep(0), c(1,3,R+1))
    animals <- abind(animals, newrow, along=1)
    for (j in 1:M){
      if(animals[count,3,j] == 1){
        animals[count+1,3,j] = 1
      }
      
      x <- animals[count,1,j]
      y <- animals[count,2,j]
      s <- one_step(x,y)
      x <- s[1]
      y <- s[2]
      
      animals[count+1,1:2,j] <- c(x,y)
      #print(animals[count+1,,j])
      check_infected(x,y,j,count) # check if now infected
      add_locations(x,y,j,count)
    }
    if (count >=N) {break} 
    if (sum(animals[,3,][count,]) == M){break}
    count <- count + 1 
  }

times=append(times,count)
}
assign(paste("times",a,R,sep=""),times)
