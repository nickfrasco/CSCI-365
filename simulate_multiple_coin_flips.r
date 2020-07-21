# Suppose X is # of H's in n coin flips.
# We know P(X = x) = choose(n,x)/2^n
# but what if we did't know this?
# Estimate P(X = x) by simulating the experiment



n = 100            # number of times we repeat experiment
x = rep(0,n)	   # will hold results of experiment here
f = 10             # number flips
outcome = 5	   # intereted in P(X = outcome)
truep = choose(f,outcome)/2^f
cat("True prob = ",truep, "\n");

for (i in 1:n) {
  x[i] = sum(runif(f)<.5);  # number heads out of n flips
}

cat("Est. prob = ",  sum(x==outcome)/n, "\n");
  