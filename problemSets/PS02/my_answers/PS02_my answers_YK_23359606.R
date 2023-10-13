#library(readr)
#library(tidyverse)
#library(ggplot2)

# 1. Calculate the χ2 test statistic
# Creating matrix
regressMat <- matrix(c(14, 6, 7, 7, 7, 1), nrow = 2, byrow = T)

# Calculating f expected values "by hand" 
#The first row of the table
fe11 <- 27*21/42
fe12 <- 27*13/42
fe13 <- 27*8/42

#The second row of the table
fe21 <- 15*21/42
fe22 <- 15*13/42
fe23 <- 15*8/42

# Calculate the chi-squared test statistic
chi_squared <- (regressMat[1,1] - fe11)^2/fe11 + (regressMat[1,2] - fe12)^2/fe12 +
  (regressMat[1,3] - fe13)^2/fe13 + (regressMat[2,1] - fe21)^2/fe21 +
  (regressMat[2,2] - fe22)^2/fe22 + (regressMat[2,3] - fe23)^2/fe23
chi_squared


#2.Calculate the p-value 
p_value <- pchisq(chi_squared, df = (nrow(regressMat) - 1)* (ncol(regressMat) - 1), lower.tail = FALSE)
p_value


#3.Calculating the standardized residuals for each cell 
# and putting them in the table
result <- chisq.test(regressMat)
stand_residuals <- result$stdres
table <- as.table(stand_residuals)
colnames(table) <- c("Not Stopped", "Bribe requested", "Stopped/given warning")
rownames(table) <- c("Upper Class", "Lower class")
print(table)


#2.(a) State a null and alternative (two-tailed) hypothesis.
# Viewing csv file with data and assigning data to vector df 
df <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")
View(df)

# Perform the bivariate regression
model <- lm(water ~ reserved, data = df)

# Summarize the regression results
summary(model)