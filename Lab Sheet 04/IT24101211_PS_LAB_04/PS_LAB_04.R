setwd("C:\\Users\\IT24101211\\Desktop\\IT24101211_PS_LAB_04")
# 1.
branch_data <- read.csv("Exercise.txt", header = TRUE)

# 2.
str(branch_data)  
summary(branch_data)

# 3. 
boxplot(branch_data$Sales_X1, main = "Boxplot of Sales", ylab = "Sales")

# 4. 
fivenum(branch_data$Advertising_X2)  
IQR_value <- IQR(branch_data$Advertising_X2)
IQR_value

# 5. 
find_outliers <- function(x) {
  Q1 <- quantile(x, 0.25)  
  Q3 <- quantile(x, 0.75)  
  IQR_value <- IQR(x)      
  lower_bound <- Q1 - 1.5 * IQR_value  
  upper_bound <- Q3 + 1.5 * IQR_value  

  outliers <- x[x < lower_bound | x > upper_bound]
  return(outliers)
}

outliers_years <- find_outliers(branch_data$Years_X3)
outliers_years

