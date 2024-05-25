for (j in 1:B){ 
  if(animals[count,3,j] == 1){
    animals[count+1,3,j] = 3
  }
}
animals

B=R+1
for(j in 1:B){
  print(j)
}