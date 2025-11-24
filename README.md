# HEART-DISEASE-ANALYSIS-BY-ASH-DANIELS
# Install necessary packages before beginning data analysis
install.packages(c("tidyverse",  # Data manipulation and visualization
                   "caret",      # Machine learning functions
                   "pROC"))      # ROC curve analysis

# Load packages
library(tidyverse)
library(caret)
library(pROC)


# Load the Heart Dataset in R
heart_data <- read.csv("heart.csv")


# Print some few rows of Heart Dataset and Check structure of the Data
head(heart_data)
str(heart_data)


# Select variables for further analysis
heart_data1 <- heart_data %>% 
  select(target, age, sex, cp, chol)

# Display the first rows to confirm
head(heart_data1)


# 1. Check for missing values
sum(is.na(heart_data1))            # Total number of missing values
colSums(is.na(heart_data1))        # Missing values per column

# 3. Convert categorical variables into factors
heart_data1$cp <- as.factor(heart_data1$cp)
heart_data1$sex <- as.factor(heart_data1$sex)

# 4. Convert target variable into factor
heart_data1$target <- as.factor(heart_data1$target)

# 5. Check structure of cleaned dataset
str(heart_data1)


# 6. Number of rows and columns in cleaned dataset
dim(heart_data1)

# 1. Summary of all variables
summary(heart_data1)



# Make plots bigger in R 

options(repr.plot.width = 10, repr.plot.height = 7)

# 2. Boxplot of cholesterol (chol) across levels of target
ggplot(heart_data1, aes(x = target, y = chol, fill = target)) +
  geom_boxplot() +
  stat_summary(fun = median, geom = "text", aes(label = round(..y..,1)),
               position = position_nudge(x = 0), vjust = -0.5, color = "black") +
  labs(title = "Cholesterol by Heart Disease Status",
       x = "Heart Disease (0 = No, 1 = Yes)",
       y = "Cholesterol (mg/dl)") +
  scale_fill_manual(values = c("pink", "salmon")) +
  theme_minimal()

# 3. Bar plot of sex vs target
ggplot(heart_data1, aes(x = sex, fill = target)) +
  geom_bar(position = "dodge") +
  geom_text(stat = "count", aes(label = after_stat(count)), 
            position = position_dodge(width = 0.9), vjust = -0.25) +
  labs(title = "Heart Disease by Sex",
       x = "Sex (0 = Female, 1 = Male)",
       y = "Count") +
  scale_fill_manual(values = c("lightgreen", "lightcoral")) +
  theme_minimal()

# 4. Bar plot of chest pain type (cp) vs target
ggplot(heart_data1, aes(x = cp, fill = target)) +
  geom_bar(position = "dodge") +
  geom_text(stat = "count", aes(label = after_stat(count)),
            position = position_dodge(width = 0.9), vjust = -0.25) +
  labs(title = "Heart Disease by Chest Pain Type",
       x = "Chest Pain Type (0,1,2,3)",
       y = "Count") +
  scale_fill_manual(values = c("skyblue", "tomato")) +
  theme_minimal()


# Fit logistic regression model
logit_model <- glm(target ~ age + sex + cp + chol,
                   data = heart_data1,
                   family = binomial)

# View model summary
summary(logit_model)

# Convert coefficients to odds ratios
odds_ratios <- exp(coef(logit_model))

# Display the odds ratios
odds_ratios


# 1. Fit the reduced model without cholesterol
reduced_model <- glm(target ~ age + sex + cp, data = heart_data1, family = binomial)

summary(reduced_model)

# 2. Perform likelihood ratio test to compare full vs reduced models
anova_result <- anova(reduced_model, logit_model, test = "Chisq")

anova_result


# 1. Predict probabilities of heart disease
predicted_prob <- predict(logit_model, type = "response")

# 2. Convert probabilities to binary predictions using threshold = 0.5
predicted_class <- ifelse(predicted_prob >= 0.5, 1, 0)

# Convert both to factors for confusion matrix
predicted_class <- factor(predicted_class, levels = c(0, 1))
actual_class <- factor(heart_data1$target, levels = c(0, 1))

# 3. Create confusion matrix
confusion_matrix <- table(Predicted = predicted_class, Actual = actual_class)

confusion_matrix


# Generate ROC curve
roc_curve <- roc(heart_data1$target, predicted_prob)

# Plot ROC curve
plot(roc_curve, col = "blue", lwd = 2, main = "ROC Curve for Logistic Regression Model")



# Calculate AUC
auc_value <- auc(roc_curve)
auc_value
