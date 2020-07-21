#Nicolo Frasco
#nfrasco@iu.edu

#Question 1
#A. Can't be found, there are not enough kale-eating Massachusetts adults who were surveyed.
#B. 0.075 --> rounds to .08
#C. Diabetic non-kale eaters: 0.11 +/- 0.002, Diabetic kale eaters: 0.08 +/- 0.005
#D. Yes, because the highest probability of a kale eater actually having diabetes is 0.085, and the lowest probability of a non-kale eater actually having diabetes is 0.108.
#E. No, you would need to account for more factors such as family istory or their entire diet. Some people are also predisposed to having/developing diabetes. 
#F. A possible theory could be that they exercise more and eat less processed foods compared to tge non-kale eaters.



#Question 2
#A. I believe there isn't enough information to back this claim. It's very possible that people who end up with less rates of diabetes than those who eat kale end up that way out of coincidence. Just not enough background/evidence
#B. Saying Health-conscious is very vague and subjective, as someone who eats poorly and then has a kale salad may call themselves "health-conscious." Not enough evidence.
#C. When you use runif(), the number is random. This means people with diabetes may end up in the kale-eating group and vice-versa. Not enough evidence.



#Question 3
x = runif(1000) # values for X
y = runif(1000) # values for Y
event_a = x < 1 - y # Set up event A
event_b = x < y # Set up event B
U = rep(0, 1000) 

for(i in 1:1000){
  if(event_a[i] && event_b[i]){
    U[i] = 1;
  } else if (event_a[i] && !event_b[i]){
    U[i] = 2;
  } else if (!event_a[i] && event_b[i]){
    U[i] = 3;
  } else if (!event_a[i] && !event_b[i]){
    U[i] = 4;
  }
}

plot(x, y, pch=U)

probA = sum(event_a) / 1000 #probability of A
probB = sum(event_b) / 1000 #probability of B

upperA = probA + 1.96*sqrt(probA*(1-probA)/N) #upper confidence interval - a
lowerA = probA - 1.96*sqrt(probA*(1-probA)/N) #lower confidence interval - a

pAandB = (probA * pB) / pB #joint probability - a and b

uppercon = probAandB + 1.96*sqrt(probAandB*(1-probAandB)/N) #upper confidence interval - a (given B)
lowercon = probAandB - 1.96*sqrt(probAandB*(1-probAandB)/N) #lower confidence interval - a (given B)

#Yes they are consistent, and because the points on the graph don't overlap with one anotherwhich makes them independent.



#Question 4
N = 1000000
x = runif(N)
a = x < .5
b = ((2*x) %% 1) < .5 # input function from HW
prob_A = sum(a) / N #probability of A
prob_B = sum(b) / N #probability of B

prob_AandB = prob_A * prob_B #joint-probability for ease of calculation

upper_A = prob_A + 1.96*sqrt(prob_A*(1-prob_A)/N) #upper confidence interval - a
lower_A = prob_A - 1.96*sqrt(prob_A*(1-prob_A)/N) #lower confidence interval - a

upper_B = prob_B + 1.96*sqrt(prob_B*(1-prob_B)/N) #upper confidence interval - B
lower_B = prob_B - 1.96*sqrt(prob_B*(1-prob_B)/N) #lower confidence interval - B

upper_AandB = prob_AandB - 1.96*sqrt(prob_AandB*(1-prob_AandB)/N) #upper confidence interval - a&b
lower_AandB = prob_AandB - 1.96*sqrt(prob_AandB*(1-prob_AandB)/N) #lower confidence interval - a&b

print(prob_AandB / prob_B)
print(prob_A)
#Yes A and B are independent events

  
#Question 5
#A. It's possible that a citizen from Bloomington could like the mayor, in which case they'd reelect him, but don't like the police chief, and in which case they'd be unlikely to reelect the police chief, and vice versa. Because of this, should be modelled as independent.
#B. The 2 people may have have different political views, so independent. 
#C. The possibility of either heads or tails are both 50% regardless. Independent.
#D. There's no way to say if the person has seen the movies. Because of this, independent. 
  


