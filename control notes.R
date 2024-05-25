# need to check at each step whether each infected animal is removed or cured
# the probability of this happening is P
# uniform distribution generate one number between 0 and 1, 
#     if less than or equal to P then event happens
#     if greater than P then the event does not happen
# 
# where in the code does this need to happen?
# check each infected animal before we map the random walk -> this could mean the initially infected animal is removed before it infects any other animals
# 
# removal
# remove the animal index from the initially uninfected animals list (1,2,...,R)
# need to keep previously infected locations
#
#
# cure
# change from infected to uninfected, keep in experiment, could result in reinfections
# need to ensure we remove its index from the newinf vector
# 

# does this apply to the initially infected animal?