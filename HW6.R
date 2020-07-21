#Nicolo Frasco
#nfrasco@iu.edu

#Question 1
#A. 
data = read.csv("~/Desktop/Vocab.csv", sep = ",", stringsAsFactors = FALSE);
x = as.vector(data[,4]);
y = as.vector(data[,5]);
n = length(x);
x = cbind(x,rep(1,n))
#print(as.matrix(x));

#B.
xbar = sum(x)/n;					#from example
ybar = sum(y)/n;
xybar = sum(x*y)/n;
xsqbar = sum(x*x)/n;
b = (ybar*xsqbar-xbar*xybar)/(xsqbar-xbar*xbar)
a = (ybar - b)/xbar
cat("a = ", a, "b = ", b, "\n")

#C. 
cat("Yes, it does appear that people with more education tend to have larger vocabularies", "\n")
#D. 
cat("I believe staying in school one extra year would increase the chances of performing better on the test.", "\n")





#Question 2 - Used a little help from stack on this one
data = read.csv("~/Desktop/ais.csv", stringsAsFactors = FALSE, sep = ",")
#A.
x = as.matrix(data[,3:12])
y = as.vector(data[,2])
a = solve(t(x) %*% x, t(x) %*% y)
cat("a: ", a[1], "b: ", a[2], "\n")

#B.
y_hat = x %*% a
e = y - y_hat
sse = sum(e*e)
cat("sum of squared errors: ", sse, "\n")

#C.
for(i in 3:12) {
	r = 3:12
	x = as.matrix(data[,r[r!=i]]) 		#got from example <>
	y = as.vector(data[,2])						
	a = solve(t(x) %*% x, t(x) %*% y)
	error = y - (x %*% a)
	sums = sum(error*error)
	cat("sum of squared errors [",i,"]:",sums,"\n")
}





#Question 3
data(nottem) 
y = nottem
n = length(y)
x = 1:n;

#A. 
plot(x,y, type = "b")

#B.
X = cbind(x,rep(1,n));
A = solve(t(X) %*% X , t(X) %*% y) #alpha
a = A[1]
b = A[2]
cat("a = ", a, "b = ", b, "\n")
plot(x,y,type = "b")
abline(b,a)			# add the line that fits the data


#C.
X = cbind(rep(1,n),cos(2*pi*x/12),sin(2*pi*x/12))
new_A = solve(t(X) %*% X, t(X) %*% y)
y_hat = X %*% new_A
lines(y_hat, col="green")

#D.
X = cbind(rep(1,n),x,cos(2*pi*x/12),sin(2*pi*x/12))
a1 = solve(t(X) %*% X, t(X) %*% y)
y_hat = X %*% a1
lines(y_hat, col = "red")




#Question 4
data(AirPassengers)
#print(AirPassengers)
y = AirPassengers
n = length(y)
x = 1:n

#A.plot(x,y, type = "b")  				plotted later on
#B.
X = cbind(x,rep(1,n));
A = solve(t(X) %*% X , t(X) %*% y) #alpha
a = A[1]
b = A[2]
plot(log(x) - log(y),type = "b")		
#C.
par(new = TRUE)
plot(x,y, type = "b", axes = FALSE)
par(new = F)

  

  


