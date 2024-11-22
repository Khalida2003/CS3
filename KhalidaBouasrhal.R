install.packages("lme4")
install.packages("Matrix")
library(Matrix)
library(lme4)
data("sleepstudy")
?sleepstudy
install.packages("broom")
install.packages("dplyr")
install.packages("ggplot2")

install.packages("tidyverse")

library(broom)
library(dplyr)
library(ggplot2)
library(tidyverse)
#############DATA EXPLORATION#########
head(sleepstudy)
?sleepstudy


# Remove Day 0 and Day 1 observations
data <- sleepstudy %>%
  filter(Days > 1)

# Verify the change
head(data)

# Histogram of Reaction times
ggplot(data, aes(x = Reaction)) +
  geom_histogram(binwidth = 10, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Reaction Times", x = "Reaction Time (ms)", y = "Count") +
  theme_minimal()

# Boxplot of Reaction Times across Days
ggplot(data, aes(x = factor(Days), y = Reaction)) +
  geom_boxplot(fill = "cyan", alpha = 0.6) +
  labs(title = "Reaction Times Across Days", x = "Days of Sleep Deprivation", y = "Reaction Time (ms)") +
  theme_minimal()

# Line plot to show trends of average reaction times over days
data_summary <- data %>%
  group_by(Days) %>%
  summarise(Average_Reaction = mean(Reaction))

ggplot(data_summary, aes(x = Days, y = Average_Reaction)) +
  geom_line(color = "darkred") +
  geom_point(color = "blue") +
  labs(title = "Average Reaction Time Over Days", x = "Days", y = "Average Reaction Time (ms)") +
  theme_minimal()
##############DESCRIPTIVE STATISTICS##################

# Summary statistics for the key variables
summary_statistics <- data %>%
  summarise(
    Mean_Reaction = mean(Reaction),
    SD_Reaction = sd(Reaction),
    Min_Reaction = min(Reaction),
    Max_Reaction = max(Reaction)
  )

# Print the summary statistics
print(summary_statistics)

# Summary statistics grouped by Days
grouped_summary <- data %>%
  group_by(Days) %>%
  summarise(
    Mean_Reaction = mean(Reaction),
    SD_Reaction = sd(Reaction),
    Min_Reaction = min(Reaction),
    Max_Reaction = max(Reaction)
  )

# Print grouped summary
print(grouped_summary)

#################################################

# Histogram of Reaction times
ggplot(data, aes(x = Reaction)) +
  geom_histogram(binwidth = 10, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Reaction Times", x = "Reaction Time (ms)", y = "Count") +
  theme_minimal()


# Boxplot of Reaction Times across Days
ggplot(data, aes(x = factor(Days), y = Reaction)) +
  geom_boxplot(fill = "cyan", alpha = 0.6) +
  labs(title = "Reaction Times Across Days", x = "Days of Sleep Deprivation", y = "Reaction Time (ms)") +
  theme_minimal()





##############MODEL####################


library(lme4)

# Linear Mixed Effects Model: Reaction time ~ Days + (1|Subject)
model <- lmer(Reaction ~ Days + (1 | Subject), data = data)
# Summary of the model
summary(model)


# Visualizing model predictions
data$Predicted_Reaction <- predict(model)

ggplot(data, aes(x = Days, y = Reaction, color = factor(Subject))) +
  geom_point(alpha = 0.5) +
  geom_line(aes(y = Predicted_Reaction), linetype = "dashed") +
  labs(title = "Model Predictions for Reaction Times Over Days",
       x = "Days", y = "Reaction Time (ms)") +
  theme_minimal()



###The plot shows the predicted reaction times over days for different subjects. 
###The dashed lines represent the model's predictions for each subject,
###capturing individual trends in reaction time as sleep deprivation progresses. 
###Reaction times generally increase over days, suggesting that sleep deprivation negatively 
###impacts reaction performance. The variability in slopes across subjects indicates 
###that individuals respond differently to sleep deprivation,
###with some showing a steeper increase in reaction times than others.

#####################RESIDUALS##########################3

# Extract residuals
residuals <- resid(model)

# Extract fitted values
fitted_values <- fitted(model)

# Add residuals and fitted values to the data
data$residuals <- residuals
data$fitted <- fitted_values

# 1. Residuals vs. Fitted Values
ggplot(data, aes(x = fitted, y = residuals)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs. Fitted Values",
       x = "Fitted Values",
       y = "Residuals") +
  theme_minimal()



# 2. QQ Plot to check normality of residuals
ggplot(data, aes(sample = residuals)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "QQ Plot of Residuals") +
  theme_minimal()

# 3. Histogram of Residuals
ggplot(data, aes(x = residuals)) +
  geom_histogram(binwidth = 10, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Residuals", x = "Residuals", y = "Frequency") +
  theme_minimal()



#####the resoduals are noramly distrubuted and they have a conctante variance.
