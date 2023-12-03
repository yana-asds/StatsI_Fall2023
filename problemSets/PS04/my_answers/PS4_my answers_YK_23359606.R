#1. Question 1: Economics
#In this question, use the prestige dataset in the car library.

install.packages(car)
library(car)
data(Prestige)
help(Prestige)
View(Prestige) 

summary(Prestige) 
# (a)Creating a new variable professional by recoding the variable type so that professionals are coded as 1, 
# and blue and white collar workers are coded as 0. 
# Converting categorical variable into factor
Prestige$professional <- ifelse(Prestige$type == "prof", 1, 0)
head(Prestige)

# (b) Running a linear model with prestige as an outcome and income, professional, 
# and the interaction of the two as predictors.
model <- lm(prestige ~ income + professional + income:professional, data = Prestige)
summary(model)

# (c) Writing the prediction equation based on the result.
coefficients_q1 <- model$coefficients
print(coefficients_q1)

#Printing the prediction equation
cat("Prediction Equation:\nprestige =", coefficients_q1[1], "+", coefficients_q1[2], "* income +", 
    coefficients_q1[3], "* professionals", "+", "(", coefficients_q1[4],")", "* income:professionals\n")


# (d) Interpret the coefficient for income.


# (e) Interpret the coefficient for professional.


# (f) What is the effect of a $1,000 increase in income on prestige score f
# or professional occupations? 
# In other words, we are interested in the marginal effect of income 
# when the variable professional takes the value of 1. 
# Calculate the change in yˆ associated with a $1,000 increase in income 
# based on your answer for (c).
income <- 0
professional <- 1
y_hat <- 21.14226 + 0.003170909 * income + 37.78128 * professional - 
  0.002325709  * income * professional
print(y_hat)
income <- 1000
y_hat_new_income <- 21.14226 + 0.003170909 * income + 37.78128 * professional -
  0.002325709  * income * professional
print(y_hat_new_income)
marginal_effect <- y_hat_new_income - y_hat
print(marginal_effect)


# g) What is the effect of changing one’s occupations from non-professional 
# to professional when her income is $6,000? 
# We are interested in the marginal effect of professional jobs 
# when the variable income takes the value of 6,000. 
# Calculate the change in yˆ based on your answer for (c).
income <- 6000
professional <- 0
y_hat_non_prof <- 21.14226 + 0.003170909 * income + 37.78128 * professional - 
 0.002325709 * income*professional
print(y_hat_non_prof)
professional <- 1
y_hat_prof <- 21.14226 + 0.003170909 * income + 37.78128 * professional - 
  0.002325709 * income*professional
print(y_hat_prof)
marginal_effect <- y_hat_prof - y_hat_non_prof
print(marginal_effect)


# 2.Question 2: Political Science
# (a) Use the results from a linear regression to determine whether having these yard signs 
# in a precinct affects vote share (e.g., conduct a hypothesis test with α = .05).
#Hypothesis: 
# H0: Having these yard signs in a precinct don't affect vote share (beta1 = 0)
# Ha: Having these yard signs in a precinct affect vote share (beta1 /= 0)
# 1. Calculating test statistic 
beta1 <- 0.042
se_beta1 <- 0.016
t1 <- (beta1 - 0)/(se_beta1)
print(t1)

# 2.Calculating degrees of freedom
N <- 131
k <- 3
df = N-k
print(df)

# 3. Calculating P-value
p_value <- 2* pt(t1, df, lower.tail = FALSE)
print(p_value)

# (b) Use the results to determine whether being next to precincts with 
# these yard signs affects vote share (e.g., conduct a hypothesis test with α = .05).
#Hypothesis: 
# H0: Being next to precincts with these yard signs doesn't affect vote share (beta2 = 0)
# Ha: Being next to precincts with these yard signs affects vote share (beta2 /= 0)
# 1. Calculating test-statistic
beta2 <- 0.042
se_beta2 <- 0.013
t2 <- (beta2 - 0)/se_beta2
print(t2)

# 2.Calculating degrees of freedom
N <- 131
k <- 3
df = N-k
print(df)

# 3. Calculating P-value
p_value <- 2*pt(t2, df, lower.tail = FALSE)
print(p_value)


# (c) Interpret the coefficient for the constant term substantively.
# In regression analysis, the constant term is an y-intercept that represents 
# the expected value of the dependent variable when all independent variables are 
# equal to zero. Thus, constant = 0.302 represents the estimated the proportion 
# of the vote that went to McAuliff’s opponent Ken Cuccinelli in the absence of
# assigned and adjacent to lawn signs.


# (d) Evaluate the model fit for this regression. 
# What does this tell us about the importance of yard signs
# versus other factors that are not modeled?
# R-squared is small.