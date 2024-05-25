###################### set up ##############################
# function to check which directions are possible i.e. if on the boundary or not
direction_check = function(x,y) {
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
one_step = function(x,y){
  directions=direction_check(x,y)
  h=directions[sample(1:length(directions),1)]
  if (h == "North") {y=y+1}
  else if (h == "South") {y=y-1}
  else if (h == "East") {x=x+1}
  else if (h == "West") {x=x-1}
  return(c(x,y))
}


###################### multiple experiments ################################
a=10
#N=200 # max number of steps
S=1000 # number of experiments
R=10 # number of uninfected animals (total number is R+1)
TT=100
#C= seq(from=1,to=10,by=1)
C= seq(from=0.01,to=0.1,by=0.01)
#C= seq(from=0.001,to=0.01,by=0.001)
M=R+1
    
    
####### run ########
profit=vector()
for (c in C){  
    #P=(c-1)/(c+1)
    P=10*(((2*exp(c))/(exp(c)+1))-1)
    #P=((c-1)/(c+1))+1
    #P=c/(sqrt(1+c^2))
    times=vector()
    value=vector()
    for (i in 1:S){
      # set up infected animal walk
      x=0
      y=0
      xcoords=c(x)
      ycoords=c(y)
      locations_infected=array(rep(0),c(a+1,a+1,R+1)) # array of all infected animal locations, starts with only 
      # one infected animal
      locations_infected[a+1,1,1]=1
      locations_all=matrix(0,nrow=a+1,ncol=a+1) # matrix of all locations visited
      locations_all[a+1,1]=1
      
      # data frame to store the locations at each time point of uninfected animal walks
      
      coords_x=matrix(0,nrow=1,ncol=R)
      coords_y=matrix(0,nrow=1,ncol=R)
      
      # set up R uninfected animals' initial locations and add to matrix tracking all locations
      
      for (j in 1:R){
        coords_x[1,j]=sample(c(0,seq(1:a)),1)
        coords_y[1,j]=sample(c(0,seq(1:a)),1)
        u=coords_x[1,j]
        v=coords_y[1,j]
        locations_all[a+1-v,u+1]=locations_all[a+1-v,u+1]+1
      }
      
      coords_x
      coords_y
      locations_infected[,,1]
      locations_all
      
      
      # map n steps
      count=0
      
      newinf=c(0)
      timepointx=matrix(0,1,R)
      timepointy=matrix(0,1,R)
      
      
      repeat {
        # check which infected animals are cured
        cured <- c()
        for(l in newinf){
        r <- runif(1,0,1)
        if (r <= P){cured <- append(cured,l)} # create vector of infected animals that are cured
        newinf <- newinf[! newinf %in% cured] # remove the cured animals from the infected animals list
        }
        
        # random walk for the initially infected animal
        s <- one_step(x,y)
        x <- s[1]
        y <- s[2]
        xcoords <- append(xcoords,x)
        ycoords <- append(ycoords,y)
        walk_infected <- data.frame(xcoords,ycoords)
        walk_infected
        if (0 %in% newinf){
          locations_infected[a+1-y,x+1,1] <- locations_infected[a+1-y,x+1,1]+1 # only add infected location if the initially infected animal is still infected
        }
        locations_all[a+1-y,x+1] <- locations_all[a+1-y,x+1]+1
        
        # check if the new location of this animal is now infected but also 
        # prevent the cured initially infected animal from becoming reinfected straight away IF it doesn't move
        
        allinfected <- apply(locations_infected,c(1,2),sum) # all infected locations
        checkinf <- allinfected[a+1-y,x+1,1] # check if the new location of the uninfected animal is a current or previous location any infected animal
        if (checkinf != 0) {
          for(j in cured){
            if(xcoords[count+2] != xcoords[count+1]){
            if(ycoords[count+2] != ycoords[count+1]){
              newinf=append(newinf,0)
              newinf=unique(newinf) 
              }
            }
          }
        }
        
        # prevent the cured animals that don't move from becoming reinfected straight away
        nonmovers=c()
        cured=cured[! cured %in% 0]
        for(j in cured){
          if(coords_x[count+1,j]==coords_x[count,j]){
              if(coords_y[count+1,j]==coords_y[count,j]){
                nonmovers <- append(nonmovers,j) 
              }
          }
          newinf <- newinf[! newinf %in% nonmovers]
        }
        
        # random walk for the R initially uninfected animals
        for (j in 1:R){
          xu=coords_x[count+1,j]
          yu=coords_y[count+1,j]
          su=one_step(xu,yu)
          xu=su[1]
          yu=su[2]
          timepointx[j]=xu
          timepointy[j]=yu
          locations_all[a+1-yu,xu+1]=locations_all[a+1-yu,xu+1]+1 # add uninfected location to all matrix
          if (j %in% newinf){
            locations_infected[a+1-yu,xu+1,j+1]=locations_infected[a+1-yu,xu+1,j+1]+1 # add infected locations
          }
        }
        
        coords_x <- rbind(coords_x,timepointx) 
        coords_y <- rbind(coords_y,timepointy)
        
        count=count+1
        
        # determine if the uninfected animals are now infected
        
        for (j in 1:R){
          xu <- coords_x[count,j]
          yu <- coords_y[count,j]
          
          allinfected <- apply(locations_infected,c(1,2),sum) # all infected locations
          checkinf <- allinfected[a+1-yu,xu+1,1] # check if the new location of the uninfected animal is a current or previous location any infected animal
          if (checkinf != 0) {
            newinf=append(newinf,j)
            newinf=unique(newinf) # which initially uninfected animals are now infected
          }
        }
        
        if (count >=TT) {break}
        check=c(1:R) %in% newinf
        if (!(FALSE %in% check)) {break}
      }
      
      countinfected = length(newinf)
      last=newinf[R]
      locations_infected[a+1-yu,xu+1,last+1]=locations_infected[a+1-yu,xu+1,last+1]+1
      
      value=append(value,countinfected)
      
    }

profit=append(profit,mean(value)-M*c)
print(profit)
table(value)
}


profit9=profit
plot(C,profit)

    
######## creating plots #########
par(mfrow=c(2,2))
C0=seq(from=1,to=10,by=1)
C1=seq(from=0.01,to=0.1,by=0.01)
C2=seq(from=0.001,to=0.01,by=0.001)
C=append(C2,C1)
# C=append(C,C0)
profit=append(profit6,profit5)
# profit=append(profit,profit4)
plot(C,profit)

plot(C1,profit7)
plot(C1,profit8)


    