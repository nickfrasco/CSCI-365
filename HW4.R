#Nicolo Frasco
#nfrasco@iu.edu

#Question 1
#A. 3 dimension table (age,education,vote)
x = read.csv2("~/Desktop/CSCI-B 365/chilean_voting.csv", stringsAsFactors = FALSE, 
	sep = ",")
x[, 5] = floor(x[, 5]/10)
education = 6
gender = 4
region = 2
age = 5
vote = 9
print(dimnames(x))
print(x[2,])
print(table(x[,c(age, education, vote)]))

#B.
print(table(x[, c(age, education)]))

#D. I am relatively confident in this classification, because bayes classifier is relatively reliable



#Question 2
yes = (x[, vote] == "Y" & !(is.na(x[, vote])))
no = (x[, vote] == "N" & !(is.na(x[, vote])))

#A.
yes_prob = sum(yes)/length(yes)
no_prob = sum(no)/length(no)
#B. Decided to hard code since i couldn't get my loop to work
#GENDER
female_yes = sum("F" == x[,gender][which("Y"==x[,vote])])/length(x[,gender][which("Y" == x[,vote])])
female_no = sum("F" == x[,gender][which("N"==x[,vote])])/length(x[,gender][which("N" == x[,vote])])
male_yes = sum("M" == x[,gender][which("Y"==x[,vote])])/length(x[,gender][which("Y" == x[,vote])])
male_no = sum("M" == x[,gender][which("N"==x[,vote])])/length(x[,gender][which("N" == x[,vote])])
#REGION
N_yes = sum("N" == x[,region][which("Y"==x[,vote])])/length(x[,region][which("Y" == x[,vote])])
N_no = sum("N" == x[,region][which("N"==x[,vote])])/length(x[,region][which("N" == x[,vote])])
C_yes = sum("C" == x[,region][which("Y"==x[,vote])])/length(x[,region][which("Y" == x[,vote])])
C_no = sum("C" == x[,region][which("N"==x[,vote])])/length(x[,region][which("N" == x[,vote])])
SA_yes = sum("SA" == x[,region][which("Y"==x[,vote])])/length(x[,region][which("Y" == x[,vote])])
SA_no = sum("SA" == x[,region][which("N"==x[,vote])])/length(x[,region][which("N" == x[,vote])])
M_yes = sum("M" == x[,region][which("Y"==x[,vote])])/length(x[,region][which("Y" == x[,vote])])
M_no = sum("M" == x[,region][which("N"==x[,vote])])/length(x[,region][which("N" == x[,vote])])
S_yes = sum("S" == x[,region][which("Y"==x[,vote])])/length(x[,region][which("Y" == x[,vote])])
S_no = sum("S" == x[,region][which("N"==x[,vote])])/length(x[,region][which("N" == x[,vote])])
#EDUCATION
P_yes = sum("P" == x[,education][which("Y"==x[,vote])])/length(x[,education][which("Y" == x[,vote])])
P_no = sum("P" == x[,education][which("N"==x[,vote])])/length(x[,education][which("N" == x[,vote])])
ps_yes = sum("PS" == x[,education][which("Y"==x[,vote])])/length(x[,education][which("Y" == x[,vote])])
ps_no = sum("PS" == x[,education][which("N"==x[,vote])])/length(x[,education][which("N" == x[,vote])])
s_yes = sum("S" == x[,education][which("Y"==x[,vote])])/length(x[,education][which("Y" == x[,vote])])
s_no = sum("S" == x[,education][which("N"==x[,vote])])/length(x[,education][which("N" == x[,vote])])
#AGE
one_yes = sum("1" == x[,age][which("Y"==x[,vote])])/length(x[,age][which("Y" == x[,vote])])
one_no = sum("1" == x[,age][which("N"==x[,vote])])/length(x[,age][which("N" == x[,vote])])
two_yes = sum("2" == x[,age][which("Y"==x[,vote])])/length(x[,age][which("Y" == x[,vote])])
two_no = sum("2" == x[,age][which("N"==x[,vote])])/length(x[,age][which("N" == x[,vote])])
three_yes = sum("3" == x[,age][which("Y"==x[,vote])])/length(x[,age][which("Y" == x[,vote])])
three_no = sum("3" == x[,age][which("N"==x[,vote])])/length(x[,age][which("N" == x[,vote])])
four_yes = sum("4" == x[,age][which("Y"==x[,vote])])/length(x[,age][which("Y" == x[,vote])])
four_no = sum("4" == x[,age][which("N"==x[,vote])])/length(x[,age][which("N" == x[,vote])])
five_yes = sum("5" == x[,age][which("Y"==x[,vote])])/length(x[,age][which("Y" == x[,vote])])
five_no = sum("5" == x[,age][which("N"==x[,vote])])/length(x[,age][which("N" == x[,vote])])
six_yes = sum("6" == x[,age][which("Y"==x[,vote])])/length(x[,age][which("Y" == x[,vote])])
six_no = sum("6" == x[,age][which("N"==x[,vote])])/length(x[,age][which("N" == x[,vote])])
seven_yes = sum("7" == x[,age][which("Y"==x[,vote])])/length(x[,age][which("Y" == x[,vote])])
seven_no = sum("7" == x[,age][which("N"==x[,vote])])/length(x[,age][which("N" == x[,vote])])
#C.
# ^^ check above for certain classifications


#Question 3
#A.
n <- 1000
x <- (runif(n) < 0.3) * 1
y <- c(0.65, 0.6, 0.57, 0.62, 0.58, 0.64, 0.67, 0.58, 0.61, 
	0.6)
z <- c()
#B.
for (i in 1:n) {
	temp <- runif(10)
	temp <- temp < y
	if (x[i] > 0) 
		z <- rbind(z, temp * 1)
	else z <- rbind(z, abs(temp - 1))
}
#C.
#posterior probability
p <- 0.3
prob <- c()
for (i in 1:n) {
	p_1 <- prod(z[i, ] * y + abs(z[i, ] - 1) * (1 - y))
	p_1 <- p_1 * p
	p_2 <- prod(z[i, ] * (1 - y) + abs(z[i, ] - 1) * y)
	p_2 <- p_2 * (1 - p)
	prob <- c(prob, p_1/(p_1 + p_2))
}

#D.
est_x <- (prob > 0.5) * 0.5
#1 means trait present and 0 means trait not present.
print(paste("Number correct: ", sum(x == est_x)))
print(paste("Percent correct: ", sum(x == est_x)/n))
print(paste("Error Rate: ", 1 - sum(x == est_x)/n))





#Question 4
#A.
data(iris)
error <- c()
d = as.matrix(dist(iris[,1:4]))
n = nrow(iris)
cl = as.numeric(iris[,5])
classhat = rep(0,n)
for (i in 1:n) {
	classhat[i] = iris[,5][order(d[i,])
	[2:6]][sort(table(iris[,5][order(d[i,])[2:6]]), #Found part of this formula online, adapted it
	decreasing = TRUE)[1]]
}
print(classhat)

#B.
print(paste("Error: ", sum(cl != classhat)))

#C.
#Because it's using the 5 closests flowers as a reference for the KNN, i believe this is error rate is an accurate estimate of the generalization error rate. 





#Question 5
x = 10 + 10 * rnorm(100)
y = -10 + 3 * rnorm(100)

#A.
#Raw vectors
plot(x, y, main = "Raw Vectors")

#Standardized vecors
x_standardized = ((x - mean(x))/sd(x))
y_standardized = ((y - mean(y))/sd(y))
plot(x_standardized, y_standardized, main = "Standardized Vectors")

#B. One difference is that the range of numbers in the raw vectors plot is much larger than the range of numbers in the standardized plot. Another difference is that the center of the raw vecors plot seems to lie right around 10 (on the x axis) while the center for the standardized plot seems to be right at the 0 mark. There are also more overlying points on the standardized vectors plot than the raw vectors plot. One similarity seems to be the distribution; they are both distributed relatively equally. 