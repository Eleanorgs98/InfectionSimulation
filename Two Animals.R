#rm(list=ls())
#rm(list=ls()[! ls() %in% c("timesA", "timesB", "timesC", "timesD","times22","times25")])

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
par(mfrow=c(1,2))
a=5 # grid size
n=0.2 # prob north
s=0.2 # prob south
e=0.2 # prob east
w=0.2 # prob west
r=0.2 # prob remain
N=1000 # max number of steps
S=1 # number of experiments

times=vector()
for (i in 1:S){
  # set up infected animal walk
  xcoords=c(0)
  ycoords=c(0)
  x=0
  y=0
  locations_infected=matrix(0,nrow=a+1,ncol=a+1)
  locations_infected[a+1,1]=1
  
  # set up uninfected animal walk
  xucoords=c(a)
  yucoords=c(a)
  xu=a
  yu=a
  locations_uninfected=matrix(0,nrow=a+1,ncol=a+1)
  locations_uninfected[1,a+1]=1
  
  # map n steps
  count=0
  
  repeat {
    s=one_step(x,y)
    x=s[1]
    y=s[2]
    xcoords = append(xcoords,x)
    ycoords = append(ycoords,y)
    walk_infected=data.frame(xcoords,ycoords)
    locations_infected[a+1-y,x+1]=locations_infected[a+1-y,x+1]+1
    
    su=one_step(xu,yu)
    xu=su[1]
    yu=su[2]
    xucoords = append(xucoords,xu)
    yucoords = append(yucoords,yu)
    walk_uninfected=data.frame(xucoords,yucoords)
    locations_uninfected[a+1-yu,xu+1]=locations_uninfected[a+1-yu,xu+1]+1
    
    count=count+1
    
    # check if the new location of the uninfected animal is a previous location of the infected animal
    checkinf=locations_infected[a+1-yu,xu+1]
    if(checkinf != 0 ){break}
    
    if (count >=N) {break}
  }
  times=append(times,count)
}

#times25=times

par(mfrow=c(1,1))
hist(times,breaks=5)
# plot one pair of walks
par(mar = c(5, 4, 4, 8))
plot(xcoords,ycoords,xlim = c(0,a),ylim=c(0,a),cex.axis=1.5,col="red",pch=16,cex=1.5,lwd=3,xlab="",ylab="")
for (i in 0:a) {
  abline(h=i,col="lightgray")
  abline(v=i,col="lightgray")
}
par(new=T)
plot(xcoords,ycoords,xlim = c(0,a),ylim=c(0,a),cex.axis=1.5,col="red",pch=16,cex=1.5,lwd=3,xlab="",ylab="")
par(new=T)
# visualise uninfected walk
plot(xucoords,yucoords,xlim = c(0,a),ylim=c(0,a),cex.axis=1.5,col="blue",cex=2,pch=4,lwd=3,xlab="",ylab="")
legend(x=5.25,y=4,xpd=TRUE,c("Infected","Uninfected"),lwd=4,horiz=FALSE, col=c("red","blue"))

#############################################################################
locations_infected
locations_uninfected
count
# walk_infected
# walk_uninfected

################### summary statistics
range(times)
mean(times)
var(times)
sd(times)
match(N,times) # finds any censored times

boxplot(times, ylab="Number of Steps")

plot(times, ylab="Number of Steps")

par(mfrow=c(1,1))

boxplot(timesA, timesB, timesC, timesD, ylab="Number of Steps", names=c("A","B","C","D"))

rm(list=ls()[! ls() %in% c("timesA", "timesB", "timesC", "timesD","times")])


