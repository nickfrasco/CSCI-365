# data on Chilean election for Pinochet in 1988.


x = read.csv2("~/Desktop/CSCI-B 365/chilean_voting.csv",stringsAsFactors=FALSE, sep=",")

# first "clean" data to retain only yes voters and no voters
vote = 9;        # the vote (Y,N,A,U) is 9th column
yes = (x[,vote] == "Y" & !(is.na(x[,vote])))   # boolean vector giving "Yes" voters who are not missing
no = (x[,vote] == "N" & !(is.na(x[,vote])))
x = x[no | yes,]


# some other variables
education = 6;   # education is the 6th column
gender = 4;
region = 2;
age = 5;

# quantize the age variable to multiples decades 1 is 10-19, 2 is 20-29, etc.
x[,age] = floor(x[,age]/10);  # floor rounds down
stop;





#t = table(x[,c(gender,vote)])

#t = table(x[,c(region,vote)])
#t = table(x[,c(education,vote)])
#t = table(x[,c(education,gender,vote)])
t = table(x[,c(education,gender,region,vote)])
print(t)
mosaicplot(t)
#cond_prob = prop.table(t,1);




