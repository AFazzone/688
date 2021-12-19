# Stat Methods Code

# Probabilities in R 
rm(list=ls()); cat("\014") # clear all
# Normal Distribution ====
x <- seq(from=-5, to=5, by=0.1); y <- dnorm(x); plot(x,y, main = "Normal Distribution", ylab = "Probability Distribution Plot") # Probability Distribution Plot
x <- seq(from=-5, to=5, by=0.1); y <- pnorm(x); plot(x,y, main = "Normal Distribution", ylab = "Cumulative Probability Plot") # Cumulative Probability Plot
Prob <- 0; x <- pnorm(Prob);  Prob==qnorm(x) # Find Probability from Cumulative Distribution 
x <- rnorm(1000); hist(x) # Generated random numbers according to the Normal Distribution

# Binomial Distribution ====
Ntrials <- 100 # number of trials  
Prob <- 0.7 # probability of success for a single trial. 
x <- seq(from=0, to=Ntrials, by=1); y <- dbinom(x, size=Ntrials, prob=Prob); plot(x,y, main = "Binomial Distribution", ylab = "Probability Distribution Plot") # Probability Distribution Plot
mtext(" 70% probability of success for a single trial.", side=1, line=2)
x <- seq(from=0, to=Ntrials, by=1); y <- pbinom(x, size=Ntrials, prob=Prob); plot(x,y, main = "Binomial Distribution", ylab = "Cumulative Probability Plot") # Cumulative Probability Plot
mtext(" 70% probability of success for a single trial.", side=1, line=2)
Prob1 <- 0.7; x <- pbinom(Prob1, size=Ntrials, prob=Prob1);  Prob1==qbinom(x, size=Ntrials, prob=Prob) # Find Probability from Cumulative Distribution 
x <- rbinom(1000, size=Ntrials, prob=Prob); hist(x) # Generated random numbers according to the Binomial Distribution
mtext(" 70% probability of success for a single trial.", side=1, line=2)

# Chi-Squared Distribution ====
x <- seq(from=-5, to=5, by=0.1); y <- dchisq(x,df=10); plot(x,y) # Probability Distribution Plot
x <- seq(from=-5, to=5, by=0.1); y <- pchisq(x,df=10); plot(x,y) # Cumulative Probability Plot

# T Distribution ====
x <- seq(from=-5, to=5, by=0.1); y <- dt(x,df=10); plot(x,y) # Probability Distribution Plot
x <- seq(from=-5, to=5, by=0.1); y <- pt(x,df=10); plot(x,y) # Cumulative Probability Plot
Prob <- 0; x <- pt(Prob,df=10);  Prob==qt(x,df=10) # Find Probability from Cumulative Distribution 
x <- rt(1000,df=10); hist(x) # Generated random numbers according to the T Distribution

# Example: Newsfeed & Binomial Distribution====
# Every story within a Newsfeed has a 4% chance of being an ad. 
# What is the chance a user will be shown only a single ad in 100 stories? 
Ntrials <- 100 # number of trials (stories)
Prob <- 4/100 # probability of success for a single trial 
Nevent <- 1 # shown only a single ad 
# Assume the probability distribution of ads being shown is binomial with Prob=1/4 out of 100 trials. max is for 4 events
yy <- dbinom(Nevent, size=Ntrials, prob=Prob) # 7% chance a user will be shown only a single ad in 100 stories
x <- seq(from=0, to=Ntrials, by=1); y <- dbinom(x, size=Ntrials, prob=Prob);
plot(x,y, main = "Binomial Distribution", ylab = "Probability Distribution Plot") # Probability Distribution Plot
mtext("7% probability of success for a single trial.", side=1, line=2)

# 1) Hypothesis testing ====
# Hypothesis testing Examples
# t-Test - Determine whether the means of two groups are equal to each other.====
# The null hypothesis is that the two means of these 2 random normal distributions are equal.
x = rnorm(1e3); y = rnorm(1e3)
t.test(x,y)

# Chi-squared Test of Independence - commonly used for testing relationships between categorical variables. ====
# The null hypothesis of the Chi-Square test is that NO relationship exists on the categorical variables
p_test <- as.table(c(21,10)); P <- c(2/3,1/3); chisq.test(p_test,p=P)$p.value # p.value=0.8989403 > 0.05 => Keep Null Hyp (p_test=P)
p_test <- as.table(c(41,10)); P <- c(2/3,1/3); chisq.test(p_test,p=P)$p.value # p.value=0.03758888 < 0.05 => Reject Null Hyp (p_test<>P)


# Example: Biased Coin: ====
# A Coin is sold for cheaters which is weighted unevenly so that the probability of a head is 0.54 versus the probability 
# of a tail which  is 0.46. 
# Create a code that can prove/disprove that a coin is biased.

# Answer: 
# Create a vector of biased probabilities and their names.
Pnames <- c("Tail","Head")
P <- c(0.46, 0.54) # Given Biased Probabilities

# Use R’s function “sample()” to generate the 10,000 coin tosses. 
# Using sampling with replacement and specify the vector of 
# biased probabilities “P” as arguments of the function “sample()”.

Num.Samples <- 1e4
throws <- sample(1:2, Num.Samples, replace=TRUE, prob=P )
TT <- table(throws)
# throws
# tails   heads 
# 4643    5357 

TTP <- table(throws)/Num.Samples # Coin Toss Probabilities
# throws
# tails   heads 
# 0.4643  0.5357 

# Tests the goodness of the sample fit to the given probabilities using chi-squared test.
ChiSq <- chisq.test(TT, p = P)
# Chi-squared test for given probabilities
ChiSq
# data:  TT
# X-squared = 0.74436, df = 1, p-value = 0.3883

# The P value is 0.38 (not bellow 0.05) - so the fit to the given probabilities is good.
P.Value <- round(ChiSq$p.value,2)

# Histogram of the distribution
hist(throws, probability=TRUE, col=gray(.9), breaks=c(0:6), 
     main=paste0("Probability of Biased Coin Tossed ",Num.Samples," Times", 
                 "\n Goodness of the fit to the given probabilities (p value) is ",P.Value),
     xlab = "Coin Toss Outcome", xaxt="n" ) 
axis(1, at=1:2, labels=c("Tail","Head"))
points(P, col = "red", pch=16)
legend("topright", paste0(Pnames,"=",P), 
       pch = 16, col = "red", title = "Given Probabilities", inset = .02)








