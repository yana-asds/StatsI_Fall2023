#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2023/main/datasets/incumbents_subset.csv")
View(inc.sub)
head(inc.sub)


# Question 1. 
# 1.1. Running a regression where the outcome variable is voteshare and the explanatory variable is difflog
model_q1<- lm(voteshare ~ difflog, data = inc.sub)
model_summary_q1 <- summary(model_q1)
print(model_summary_q1)

# 1.2. Making a scatterplot of the two variables 
pdf("scatterplot_q1.pdf") 
plot(inc.sub$difflog,
     inc.sub$voteshare,
     xlab="Difference in campaing spending between incumbent and challenger (difflog)",
     ylab="Vote share of the presidential candidate of the incumbent's party (voteshare)",
     main="The Relationship between \nDifference in campaing spending between incumbent and challenger \nand Vote share of the presidential candidate of the incumbent's party")

# Adding the regression line
abline(model_q1, col = "red")
dev.off()

# 1.3. Saving the residuals of the model in a separate object
residuals_q1 <- residuals(model_q1)
head(residuals_q1)

# 1.4. Printing coefficients for the prediction equation
coefficient_q1 <- model_summary_q1$coefficients
print(coefficient_q1)
# Printing the prediction equation
cat("Prediction Equation:\n voteshare =", coefficient_q1[1], "+", coefficient_q1[2], "* difflog\n")


# Question 2.
# 2.1.Running a regression where the outcome variable is presvote and the explanatory variable is difflog.
model_q2<- lm(presvote ~ difflog, data = inc.sub)
model_summary_q2 <- summary(model_q2)
print(model_summary_q2)

# 2.2. Making a scatterplot of the two variables and adding the regression line
pdf("scatterplot_q2.pdf") 
plot(inc.sub$difflog,
     inc.sub$presvote,
     xlab="Difference in campaing spending between incumbent and challenger (difflog)",
     ylab="Incumbent's electoral success (presvote)",
     main="The Relationship between \nDifference in campaing spending between incumbent and challenger \nand Incumbent's electoral success")

# Adding the regression line
abline(model_q2, col = "red")
dev.off()

# 2.3. Saving the residuals of the model in a separate object
residuals_q2 <- residuals(model_q2)
head(residuals_q2)

# 2.4. Printing coefficients for the prediction equation
coefficient_q2 <- model_summary_q2$coefficients
print(coefficient_q2)
# Printing the prediction equation
cat("Prediction Equation:\n presvote =", coefficient_q2[1], "+", coefficient_q2[2], "* difflog\n")


# Question 3.
# 3.1. Running a regression where the outcome variable is voteshare and the explanatory variable is presvote.
model_q3<- lm(voteshare ~ presvote, data = inc.sub)
model_summary_q3 <- summary(model_q3)
print(model_summary_q3)

# 3.2. Making a scatterplot of the two variables and adding the regression line
pdf("scatterplot_q3.pdf") 
plot(inc.sub$presvote,
     inc.sub$voteshare,
     xlab="Incumbent's electoral success (presvote)",
     ylab="Vote share of the presidential candidate of the incumbent's party (voteshare)",
     main="The Relationship between \nIncumbent's electoral success and \nVote share of the presidential candidate of the incumbent's party")

# Adding the regression line
abline(model_q3, col = "red")
dev.off()

# 3.3. Printing coefficients for the prediction equation
coefficient_q3 <- model_summary_q3$coefficients
print(coefficient_q3)
# Printing the prediction equation
cat("Prediction Equation:\n voteshare =", coefficient_q3[1], "+", coefficient_q3[2], "* presvote\n")


# Question 4.
# 4.1. Running a regression where the outcome variable is the residuals from Question 1 and 
# the explanatory variable is the residuals from Question 2.
residual_model<- lm(residuals_q1 ~ residuals_q2)
residual_summary_q4 <- summary(residual_model)
print(residual_summary_q4)

# 4.2. Making a scatterplot of the two residuals and adding the regression line.
pdf("scatterplot_q4.pdf") 
plot(residuals_q2,
     residuals_q1,
     xlab="The residuals from Question 2",
     ylab="The residuals from Question 1",
     main="The Relationship between the residuals from Question 2 \nand the residuals from Question 1")

# Adding the regression line
abline(residual_model, col = "red")
dev.off() 

# 4.3 Printing coefficients for the prediction equation
coefficients_res <- residual_model$coefficient
print(coefficients_res)

cat("Prediction Equation:\n residuals_question1 =", coefficients_res[1], "+", coefficients_res[2], "* residuals_question2\n")


# Question 5.
# 5.1. Running a regression where the outcome variable is the incumbent’s voteshare and 
# the explanatory variables are difflog and presvote.
model_q5 <- lm(voteshare ~ difflog + presvote, data = inc.sub)
model_summary_q4 <- summary(model_q5)
print(model_summary_q4)

# 5.2. Printing coefficients for the prediction equation
coefficients_q5 <- model_q5$coefficients
print(coefficients_q5)

cat("Prediction Equation:\n voteshare =", coefficients_q5[1], "+", coefficients_q5[2], "* difflog +", coefficients_q5[3], "* presvote\n")


# 5.3. What is it in this output that is identical to the output in Question 4? 
# Why do you think this is the case?
# Checking the correlation coefficient between presvote and residuals from Question 2
cor(inc.sub$presvote, residuals_q2)

