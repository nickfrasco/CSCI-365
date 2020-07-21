# Nicolo Frasco
# nfrasco@iu.edu

#Question 1
#A. Since P(T) = 0.30 but P(T|S) = .56 which doesn't equal P(S) = .48 and  P(S|T) = .90
#.    P and S are not independent
#B. P(T|S) = .56
#C. 
n = 10000 #number of times it'll test
count = 0

s_count = rep(FALSE,n)
t_count = rep(FALSE,n)
for (i in 1:n) {
	s_count[i] = (runif(1) < .48)
	if (s_count[i]) {
		t_count[i] = (runif(1) < .56)
		count = count + 1
	}
}

prob = count / n
upper = prob + 1.96*sqrt(((prob)*(1-prob))/n) #upper bound
lower = prob - 1.96*sqrt(((prob)*(1-prob))/n) #lower bound
print(prob)

#D. Yes it is consistent




#Question 2
#A. P(A|not favor) = (P(A)*(1-P(favor|A)))/ ((P(A)*(1-P(favor|A)) + P(B)*(1-favor|A)) + P(C)*(1-P(favor|C)))
#B. P(A) = .2
#C. P(B) = .3
#D. P(c) = .5
#E. 

n = 10000
total = 0
party_a = rep(FALSE,n)
for (i in 1:n) {
	party_a[1] = (runif(1) < (0.3*0.2))
	if (party_a[i]) {
		total - total - 1
	}
}
prob_2 = total / n
print(prob_2)
#F. No because the calculation used above was for someone who is in Party A. We cant be sure this person is in Party A




#Question 3
#A. Department Vs. Admit - dependent
data("UCBAdmissions"); 
ucb = UCBAdmissions;  
print(dimnames(ucb));
print(apply(ucb,c("Dept","Admit"),sum));
mosaicplot(apply(ucb,c("Dept","Admit"),sum));
 
#B. Department Vs. Gender - dependent
data("UCBAdmissions"); 
ucb = UCBAdmissions;  
print(apply(ucb,c("Dept","Gender"),sum));
mosaicplot(apply(ucb,c("Dept","Gender"),sum));
 
#C. Gender Vs. Admit (Of department F) - independent
data("UCBAdmissions"); 
ucb = UCBAdmissions;  
print(apply(ucb,c("Gender","Admit"),sum));
mosaicplot(apply(ucb,c("Dept","Admit"),sum));
 
#D. Rejected Students 
data("UCBAdmissions"); 
ucb = UCBAdmissions;  
print(apply(ucb,("Admit"),sum));




#Question 4 - used a bunch of code from example to help
#A. look below
data(iris)
n = nrow(iris);    				#code taken from class example
type = rep(0,n);
color = rep(0,n);

type[iris[,5] == "setosa"] = "s";      
type[iris[,5] == "versicolor"] = "c";
type[iris[,5] == "virginica"] = "v";

color[iris[,5] == "setosa"] = 1;
color[iris[,5] == "versicolor"] = 2;
color[iris[,5] == "virginica"] = 3;

pairs(iris[,1:4], pch=type,col=color,cex=2)
#B.  Petal length would be the best coice for constructing a classifier. This is because the petals' widths are the most distinct between the different flowers. 




#Question 5
#A.
in_box = c(1,2,3)
on_die = c(1,2,3,4,5,6)

v = sample(in_box,1,replace = TRUE, prob = c(1/2,1/4,1/4));
print(v)

if (v == 1) {
	roll_a = sample(on_die,3,replace=TRUE,prob=c((1/6),(1/6),(1/6),(1/6),(1/6),(1/6))); 
	print("x1,x2,x3: ", roll_a) }
	else if (v == 2) {
	roll_b = sample(on_die,3,replace=TRUE,prob=c((2/9),(1/9),(2/9),(1/9),(2/9),(1/9))); 
	print("x1,x2,x3: ", roll_b) }
	else if (v == 3) {
	rollcb = sample(on_die,3,replace=TRUE,prob=c((1/9),(1/9),(1/9),(2/9),(2/9),(2/9))); 
	print("x1,x2,x3: ", roll_c) }


#B. 
a = ((1/6)*(1/6)*(1/6)*(1/2))/(((1/6)*(1/6)*(1/6)*(1/2)) + ((1/9)*(1/9)*(2/9)*(1/4)) + ((1/9)*(2/9)*(2/9)*(1/4)))
b = ((1/9)*(1/9)*(2/9)*(1/4))/(((1/6)*(1/6)*(1/6)*(1/2)) + ((1/9)*(1/9)*(2/9)*(1/4))+ ((1/9)*(2/9)*(2/9)*(1/4)))
c = ((1/9)*(2/9)*(2/9)*(1/4))/(((1/6)*(1/6)*(1/6)*(1/2)) + ((1/9)*(1/9)*(2/9)*(1/4)) + ((1/9)*(2/9)*(2/9)*(1/4)))

print("P(A|2,4,5) = " , a)
print("P(B|2,4,5) = " , b)
print("P(C|2,4,5) = " , c)

#C. Bayes classifier says classify the dice having maximum  probability - as A
# P(C=A|X=x1,x2,x3) = .529
# P(C=B|X=x1,x2,x3) = .157 
# P(C=C|X=x1,x2,x3) = .314

