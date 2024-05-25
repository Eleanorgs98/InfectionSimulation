rm(list=ls()[! ls() %in% c("times55", "times510", "times515", "times520","times105","times1010","times1015", "times1020", "times155", "times1510","times1515","times1520","times205", "times2010", "times2015", "times2020")])
###################### functions ##############################
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
par(mfrow=c(1,2))
for(a in list(20)){
  for(R in list(20)){
    print(c(a,R))
    N=1000 # max number of steps
    S=1000 # number of experiments
    
    ####### run ########
    times=vector()
    for (i in 1:S){
      # set up infected animal walk
      x=0
      y=0
      xcoords=c(x)
      ycoords=c(y)
      locations_infected=array(rep(0),c(a+1,a+1,R+1)) # array of all infected animal locations, starts with only one infected animal
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
      
      newinf=c()
      timepointx=matrix(0,1,R)
      timepointy=matrix(0,1,R)
      
      repeat {
        # random walk for infected animal
        s=one_step(x,y)
        x=s[1]
        y=s[2]
        xcoords = append(xcoords,x)
        ycoords = append(ycoords,y)
        walk_infected=data.frame(xcoords,ycoords)
        walk_infected
        locations_infected[a+1-y,x+1,1]=locations_infected[a+1-y,x+1,1]+1
        locations_all[a+1-y,x+1]=locations_all[a+1-y,x+1]+1
        
        
        # random walk for R uninfected animals
        
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
       
        coords_x = rbind(coords_x,timepointx) 
        coords_y = rbind(coords_y,timepointy)
        
        count=count+1
      
        # determine if the uninfected animals are now infected
        
        for (j in 1:R){
          xu=coords_x[count,j]
          yu=coords_y[count,j]
          
          allinfected=apply(locations_infected,c(1,2),sum) # all infected locations
          checkinf=allinfected[a+1-yu,xu+1] # check if the new location of the uninfected animal is a current or previous location any infected animal
          if (checkinf != 0) {
            newinf=append(newinf,j)
            newinf=unique(newinf) # which initially uninfected animals are now infected
          }
        }
      
        if (count >=N) {break}
        check=c(1:R) %in% newinf
        if (!(FALSE %in% check)) {break}
      }
      times=append(times,count)
    }
    
    
    assign(paste("times",a,R,sep=""),times)
  }
}