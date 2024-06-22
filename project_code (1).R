#0data reading
load("C:/Users/Reema/Downloads/lead.RData")
my_data<- lead
#1summarizing data
summary(my_data)
catdata <-str(my_data)
table(lead$Sex)
table(lead$Lead_type)
table(lead$Area)

#frequency table

frequency_table <- table(my_data$existing)
frequency_table <- table(catdata)
#deleting NA values 
new_data <- na.omit(my_data)
# Calculating correlation coefficients
correlation_1 <- cor(new_data$MAXFWT, new_data$Ld72)
correlation_2 <- cor(new_data$MAXFWT, new_data$Ld73)
print(paste("Correlation between MAXFWT and Ld72:", correlation_1))
print(paste("Correlation between MAXFWT and Ld73:", correlation_2))

# Generate a bar chart for the gender variable
library(ggplot2)
ggplot(new_data, aes(x=Sex)) + geom_bar() + labs(title="Gender Distribution")

# Generate a bar chart with mean MAXFWT in males and females
ggplot(new_data, aes(x=Sex, y=MAXFWT, fill=Sex)) + 
  stat_summary(fun=mean, geom="bar") + 
  labs(title="Mean MAXFWT by Gender", y="Mean MAXFWT")

# Histogram of 'Age' and 'MAXFWT'
ggplot(new_data, aes(x=Age, fill=..count..)) +
  geom_histogram(bins=30) +
  labs(title="Histogram of Age") +
  scale_fill_gradient(low="blue", high="red")

ggplot(new_data, aes(x=MAXFWT, fill=..count..)) +
  geom_histogram(bins=30) +
  labs(title="Histogram of MAXFWT") +
  scale_fill_gradient(low="skyblue", high="lightpink")

# Scatterplot of Ld72 vs MAXFWT with regression lines for each gender
ggplot(new_data, aes(x=Ld72, y=MAXFWT, color=Sex)) + 
  geom_point() + 
  geom_smooth(method="lm", se=FALSE) + 
  labs(title="Scatterplot of Ld72 vs MAXFWT with Regression Lines")

# Boxplots for 'Age', 'Ld72', and 'Ld73'
ggplot(new_data, aes(x=factor(1), y=Age)) +
  geom_boxplot(color = "green") +
  labs(title="Boxplot of Age")
ggplot(new_data, aes(x=factor(Ld72), y=MAXFWT)) + geom_boxplot(color="blue") + labs(title="Boxplot per Ld72")
ggplot(new_data, aes(x=factor(Ld73), y=MAXFWT)) + geom_boxplot(color="orange") + labs(title="Boxplot per Ld73")


# 3-Outliers Detection

# Boxplot for MAXFWT
boxplot(new_data$MAXFWT, main = "Boxplot for MAXFWT", ylab = "MAXFWT", col="yellow")

# Boxplot for Ld72
boxplot(new_data$Ld72, main = "Boxplot for Ld72", ylab = "Lead Concentration in 1972" ,col="darkred")

# Boxplot for Ld73
boxplot(new_data$Ld73, main = "Boxplot for Ld73", ylab = "Lead Concentration in 1973" , col="lightgreen")

# Boxplot for Age
boxplot(new_data$Age, main = "Boxplot for Age", ylab = "Age", col="lightblue")

# Function to identify outliers
identify_outliers <- function(variable) {
  Q1 <- quantile(variable, 0.25, na.rm = TRUE)
  Q3 <- quantile(variable, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  return(cbind(lower = lower_bound, upper = upper_bound, outliers = variable[variable < lower_bound | variable > upper_bound]))
}


# Identify outliers in MAXFWT, Ld72, Ld73, and Age
outliers_MAXFWT <- identify_outliers(new_data$MAXFWT)
outliers_Ld72 <- identify_outliers(new_data$Ld72)
outliers_Ld73 <- identify_outliers(new_data$Ld73)
outliers_Age <- identify_outliers(new_data$Age)


# Output the outliers
print("Outliers in MAXFWT:")
print(outliers_MAXFWT)

print("Outliers in Ld72:")
print(outliers_Ld72)

print("Outliers in Ld73:")
print(outliers_Ld73)

print("Outliers in Age:")
print(outliers_Age)




###4
#Testing for normality
# Method 1 using Histogram
hist(lead$Age, col='violet')

hist(lead$MAXFWT, col='steelblue')

lead$Ld72 <- as.numeric(lead$Ld72)
hist(lead$Ld72, col='skyblue') 

lead$Ld73 <- as.numeric(lead$Ld73)
hist(lead$Ld73, col='orange')

hist(lead$Iqf, col='purple') 

hist(lead$Totyrs, col='lightgreen') 

# Method 2 using QQ-plot
qqnorm(lead$Age)
qqline(lead$Age)
shapiro.test(lead$Age)

qqnorm(lead$MAXFWT)
qqline(lead$MAXFWT)
shapiro.test(lead$MAXFWT)

qqnorm(lead$Ld72)
qqline(lead$Ld72)
shapiro.test(lead$Ld72)

qqnorm(lead$Ld73)
qqline(lead$Ld73)
shapiro.test(lead$Ld73)

qqnorm(lead$Iqf)
qqline(lead$Iqf)
shapiro.test(lead$Iqf)

qqnorm(lead$Totyrs)
qqline(lead$Totyrs)
shapiro.test(lead$Totyrs)

#For Age, MAXFWT, Ld72, Ld73, and Totyrs, the p-values are all very small (less than 0.05), 
#indicating strong evidence against the null hypothesis of normality. 
#Therefore, you would reject the null hypothesis and conclude that these variables are not normally distributed.

#For Iqf, the p-value is 0.07334, which is greater than 0.05. 
#so fail to reject the null hypothesis, suggesting that there is not enough evidence

#Testing for homoscedasticity 
#Method 1 using Boxlot


# Boxplot for Age
boxplot(Age ~ Lead_type, data = lead, main = "Box Plot of Age by Lead Type",
        xlab = "Lead Type", ylab = "Age", col = c("orange", "lightgreen"))
# Boxplot for MAXFWT
boxplot(MAXFWT ~ Lead_type, data = lead, main = "Box Plot of MAXFWT by Lead Type",
        xlab = "Lead Type", ylab = "MAXFWT", col = c("pink", "lightgreen"))

# Boxplot for Ld72
boxplot(Ld72 ~ Lead_type, data = lead, main = "Box Plot of Ld72 by Lead Type",
        xlab = "Lead Type", ylab = "Ld72", col = c("violet", "lightgreen"))

# Boxplot for Ld73
boxplot(Ld73 ~ Lead_type, data = lead, main = "Box Plot of Ld73 by Lead Type",
        xlab = "Lead Type", ylab = "Ld73", col = c("lightblue", "lightgreen"))
# Boxplot for Iqf
boxplot(Iqf ~ Lead_type, data = lead, main = "Box Plot of Iqf by Lead Type",
        xlab = "Lead Type", ylab = "Iqf", col = c("yellow", "lightgreen"))

# Boxplot for Totyrs
boxplot(Totyrs ~ Lead_type, data = lead, main = "Box Plot of Totyrs by Lead Type",
        xlab = "Lead Type", ylab = "Totyrs", col = c("skyblue", "lightgreen"))


#Mehod 2 using levene test

installed.packages("carData")

library(car)

lead$Lead_type <- factor(lead$Lead_type)

leveneTest(MAXFWT ~ Lead_type, data = lead)

leveneTest(Ld72 ~ Lead_type, data = lead)

leveneTest(Ld73~ Lead_type, data = lead)

leveneTest(Iqf ~ Lead_type, data = lead)

leveneTest(Totyrs ~ Lead_type, data = lead)


#For MAXFWT, Ld73, and Totyrs, the p-values are all relatively large (greater than 0.05), 
#indicating that there is no significant difference in variances across groups defined by Lead_type.

#For Ld72, the p-value is less than 0.05, 
#suggesting that there is evidence of a significant difference in variances across groups.

#For Iqf, the p-value is close to 0.1,
#It suggests a slight indication of difference in variances across groups,
#but it does not reach conventional levels of statistical significance.

###5
# Statistical Inference

# Calculate confidence intervals for the means of MAXFWT per each Sex

#confidence_intervals_90
tapply(lead$MAXFWT, lead$Sex, function(x) t.test(x, conf.level = 0.90)$conf.int)

#confidence_intervals_95
tapply(lead$MAXFWT, lead$Sex, function(x) t.test(x, conf.level = 0.95)$conf.int)

#confidence_intervals_99
tapply(lead$MAXFWT, lead$Sex, function(x) t.test(x, conf.level = 0.99)$conf.int)

#As the confidence level increases from 90% to 99%, the width of the confidence intervals increases. 
#This is expected because higher confidence levels require wider intervals to capture the true population parameter with higher certainty.


# 6 Hypothesis tests

t.test(lead$MAXFWT ~ lead$Sex, var.equal = TRUE)  # Assuming normality and homoscedasticity
#The p-value for the t-test is 0.2364, which is greater than the significance level which is usually 0.05,Therefore, we fail to reject the null hypothesis
#The mean MAXFWT for males group 1 is 50.68,females group 2 is 54.12, but this difference is not statistically significant.
#The 95% confidence interval for the difference in means ranges from -9.181 to 2.299 so confidence interval also includes zero,further supporting the conclusion that the difference is not statistically significant

wilcox_test <- wilcox.test(
  subset(lead$MAXFWT, lead$Ld72 <= 40),
  subset(lead$MAXFWT, lead$Ld72 > 40),
  alternative = "less"
)
print(wilcox_test)
#we don`t have enough evidence to reject NULL`
#in support to alternative

lead$Sex <- factor(lead$Sex, levels = c(1, 2), labels = c("male", "female"))

#lead$Sex <- ifelse(lead$Sex == 1, "male", "female")
#lead$sex[lead$Sex == 1] <- "male"
#lead$sex[lead$Sex == 2] <- "female"
#View(lead)

lead$Lead_type <- factor(lead$Lead_type, levels = c(1, 2), labels = c("leadtype1", "leadtype2"))
#lead$Lead_type <- ifelse(lead$Lead_type == 1, "leadtype1", "leadtype2")
#lead$Lead_type[lead$Lead_type == 1] <- "leadtype1"
#lead$Lead_type[lead$Lead_type == 2] <- "leadtype2"

View(lead)


lead$Group <- paste(lead$Sex, lead$Lead_type, sep = "_")

unique(lead$Group)
View(lead)

model <- lm(MAXFWT ~ Group, data = lead)
anova(model)
#p value is 0.0038 which is less than 0.05,therefore,there are significant differences in MAXFWT among the interaction groups

TukeyHSD(aov(model))
#male_leadtype2-female_leadtype1   -14.2051282 -24.905088 -3.505169 0.0044057 there is a statistically significant difference between these two groups. 
#male_leadtype2-male_leadtype1     -13.8087318 -24.027962 -3.589502 0.0036208 there is a statistically significant difference between these two groups 


# 7 Linear Regression Model

# Plotting the data before modeling  
plot(new_data$Ld73, new_data$MAXFWT, pch=16, cex=2)

# Fitting linear model with Lead73
model_Lead73 <- lm(MAXFWT ~ Ld73, data = new_data)
abline(model_Lead73, lwd=5, col="red")

# Summary of the model to get coefficients
summary(model_Lead73)

# Calculating 95% confidence interval for the Ld73 coefficient
confint(model_Lead73, "Ld73", level = 0.95)

# Predict MAXFWT for Ld73 = 100
predict(model_Lead73, newdata = data.frame(Ld73 = 100))