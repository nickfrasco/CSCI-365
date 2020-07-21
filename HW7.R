#Nicolo Frasco
#nfrasco@iu.edu

#Question 1
#This problem is essentialy the opposite of forward selection (obviously). With only a few changes and editing the variable selection to make sure the previous one isn't used, it is clear to see how backward selection works and is unique in its ways. 

#loop through all, get rid of one every time
n = 100
d = 100
X = matrix(rnorm(n*d),nrow = n, ncol=d); #100x100
truea = c(1:5,rep(0,d-5))    	
y = X %*% truea + rnorm(n);				#1x100

used = rep(TRUE,d);				# initially all varaibles available for selection
var = rep(0,d);						# var[j] will be variable chosen in jth round
bestsse = rep(10000000,d);
										#Taken from example given in class, but modified
for (j in 1:(d-1))  {						# choose 1 variable 
    for (i in which(used == TRUE)) {		
       used[i] = FALSE;
       XX = X[,used];						# take the "used" columns = used variables
       a = solve(t(XX) %*% XX , t(XX) %*% y);
       yhat = XX %*% a
       error = y-yhat
       sse = sum(error*error)
       #print(sse)						used for testing
       if (sse < bestsse[j]) {			# if we find a better sse, take it
          bestsse[j] = sse;
          var[j] = i;
       }
       used[i] = TRUE;
   }
   used[var[j]] = FALSE;	 # opposite of forward selection because we don't use variable for 									next iteration
   print(paste("REMOVED #: ", var[j], " on iteration number: ", j, ", best sse = ", bestsse[j]))
}

plot(bestsse[1:(d-1)]) 					#d-1 because get rid of one and don't start on n/a
#This takes a long time so can always reduce d value (nnumber of iterations) to speed up



#Question 2 - 
#data = {(0,1), (1,1), (0,2), (1,2), (0,3), (1,3)}
#m1 = t(0,1)
#m2 = t(0,2)

#A.
#m1 = {(0,1), (1,1)}
#m1 mean = (.5,1)
#m2 = {(0,2), (1,2), (0,3), (1,3)}
#m2 mean = (.5, 2.5)

#B.
#after calculations, 
#we know have m1 as (.5,1) and m2 as (.5,2.5)
#UPDATED
#m1 = {(0,1), (1,1)}
#m1 mean = (.5, 1)
#m2 = {(0,2), (1,2), (0,3), (1,3)}
#m2 mean = (.5, 2.5)

#C.
#Result: (final clusters)
#cluster1 = {(0,1),(1,1)}
#cluster2 = {(0,2), (1,2), (0,3), (1,3)}

#D.
#Terminates when none of points change/swap when m changes



#Question 3
#look at picture attatched labeled "Q3"



#Question 4
#A.
#If we reference problem 3, we can see that we minimized the function H(m) by setting m = (1/n)*sum[(x_i)]. We are able to minimize the inside sum. Because of this, H(m) decreases as the function iterates through.

#B.  
#The algorithm must terminate because it's impossible (or nearly impossible) for a set number of #data points on a plot to rotate between different clusters. Alos because you're using the #derivative of the sum, the algorithm will always terminate. 



#Question 5
#Taken from 2:
#	m1 = t(0,1)
#	m2 = t(0,2)
#H(m1,m2) = 4
#Same as 2 but but this version is not globally optimal

