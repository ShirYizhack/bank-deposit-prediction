######### Bank Deposit Prediction

bankdata <- read.csv("data/Bank_data.csv")
View(bankdata)

###################################### Checking the data structure
str(bankdata)
summary(bankdata)

###################################### Checking for NA values
colSums(is.na(bankdata))
sum(!complete.cases(bankdata))

###################################### Converting Categorical Variables to Factor
bankdata$job <- as.factor(bankdata$job)
bankdata$marital <- as.factor(bankdata$marital)
bankdata$education <- as.factor(bankdata$education)
bankdata$default <- as.factor(bankdata$default)
bankdata$housing <- as.factor(bankdata$housing)
bankdata$loan <- as.factor(bankdata$loan)
bankdata$contact <- as.factor(bankdata$contact)
bankdata$month <- as.factor(bankdata$month)
bankdata$poutcome <- as.factor(bankdata$poutcome)
bankdata$deposit <- as.factor(bankdata$deposit)

####### Check it worked
str(bankdata)

###################################### Creating new variables
#Age Group
bankdata$age_group <- cut(
  bankdata$age,
  breaks = c(17, 30, 45, 60, 100),
  labels = c("young", "adult", "middle age", "senior")
)
bankdata$age_group <- as.factor(bankdata$age_group)

#Balance Positive
bankdata$balance_positive <- ifelse(bankdata$balance > 0, "positive", "negative")
bankdata$balance_positive <- as.factor(bankdata$balance_positive)

#Was Contacted Before
bankdata$was_contacted_before <- ifelse(bankdata$pdays == -1, "no", "yes")
bankdata$was_contacted_before <- as.factor(bankdata$was_contacted_before)

#Num Contacts Total
bankdata$num_contacts_total <- bankdata$campaign + bankdata$previous
View(bankdata)

#Duration Group
bankdata$duration_group <- cut(
  bankdata$duration,
  breaks = c(0, 100, 500, 2000, Inf),
  labels = c("very_short", "short", "medium", "long")
)
bankdata$duration_group <- as.factor(bankdata$duration_group)

###################################### Checking the validity of the new variables
############## 1) Validity test
#Checking for NA
colSums(is.na(bankdata))

#Checking the distribution
table(bankdata$age_group)
table(bankdata$balance_positive)
table(bankdata$was_contacted_before)
table(bankdata$num_contacts_total > 0)  
table(bankdata$duration_group)

#Summary check for variables
summary(bankdata[, c("age_group",
                     "balance_positive",
                     "was_contacted_before",
                     "num_contacts_total",
                     "duration_group")])

############## 2) Basic logic test
#Age
range(bankdata$age)

#Num of contacts
summary(bankdata$num_contacts_total)

#Number of days passed
summary(bankdata$pdays)

#####################################################################################################
########################################## Exploratory Data Analysis (EDA)
#################### Distribution of the target variable

table(bankdata$deposit)
prop.table(table(bankdata$deposit))

library(ggplot2)
library(dplyr)

deposit_counts <- bankdata %>%
  count(deposit) %>%
  mutate(
    percent = n / sum(n) * 100,
    label = paste0(round(percent, 1), "%")
  )

ggplot(deposit_counts, aes(x = "", y = n, fill = deposit)) +
  geom_col(color = "black", width = 1) +
  coord_polar(theta = "y") +
  
  # אחוזים על הגרף
  geom_text(
    aes(label = label),
    position = position_stack(vjust = 0.5),
    size = 5,
    fontface = "bold"
  ) +
  
  scale_fill_manual(values = c(
    "no" = "#FFB3BA",
    "yes" = "#B5EAD7"
  )) +
  
  labs(
    title = "Distribution of Deposit Outcome",
    fill = "Deposit Decision"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.title = element_text(face = "bold")
  )

#################### Visual analysis of quantitative variables

## Age distribution analysis
ggplot(bankdata, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "#D6E0FF", color = "black") +
  geom_density(color = "darkgreen", linewidth = 0.4) +
  labs(title = "Distribution of Customer Age",
       x = "Age",
       y = "Frequency") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"))

#Checking the age range
summary(bankdata$age)
range(bankdata$age)

## Account balance distribution analysis
ggplot(bankdata, aes(x = balance)) +
  geom_histogram(binwidth = 200, 
                 fill = "#FFDAC1"   
                 ) +
  geom_density(color = "darkred", linewidth = 0.4) +
  labs(title = "Distribution of Account Balance",
       x = "Account Balance",
       y = "Frequency") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

summary(bankdata$balance)
sum(bankdata$balance < 0)

## Analysis of the number of customer referrals
ggplot(bankdata, aes(x = campaign)) +
  geom_histogram(binwidth = 1,
                 fill = "#CDE7FF",  
                 color = "black") +
  labs(title = "Distribution of Campaign Contacts",
       x = "Number of Contacts During Campaign",
       y = "Frequency") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

summary(bankdata$campaign)

## Analysis of the number of times they attempted to contact the customer in previous campaigns
ggplot(bankdata, aes(x = deposit, y = previous, fill = deposit)) +
  geom_boxplot(outlier.shape = NA, color = "black") +
  geom_jitter(width = 0.1, alpha = 0.3, color = "grey30") +
  scale_fill_manual(values = c("no" = "#C7CEEA", "yes" = "#B5EAD7")) +
  labs(title = "Previous Contacts by Deposit Decision",
       x = "Deposit Decision",
       y = "Previous Contacts") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

#################### Visual analysis of categorical variables

## Distribution of Job Types
ggplot(bankdata, aes(x = job, y = age, fill = deposit)) +
  geom_boxplot(outlier.alpha = 0.3, color = "black") +
  scale_fill_manual(values = c("no" = "#C7CEEA", "yes" = "#B5EAD7")) +
  labs(title = "Age Distribution by Job and Deposit Decision",
       x = "Job Category",
       y = "Age") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(face = "bold")
  )

## Distribution of Education Levels
ggplot(bankdata, aes(x = education, fill = education)) +
  geom_bar(color = "black") +
  scale_fill_brewer(palette = "Pastel2") +
  labs(title = "Distribution of Education Levels",
       x = "Education Level",
       y = "Count") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

## Deposit Rate by Marital Status
ggplot(bankdata, aes(x = marital, fill = deposit)) +
  geom_bar(position = "fill", color = "black") +
  scale_fill_manual(values = c("no" = "#C7CEEA", "yes" = "#B5EAD7")) +
  labs(title = "Deposit Rate by Marital Status",
       x = "Marital Status",
       y = "Proportion") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

## Deposit rate by Housing
ggplot(bankdata, aes(x = housing, fill = deposit)) +
  geom_bar(position = "fill", color = "black") +
  scale_fill_manual(values = c("no" = "#F2B5D4", "yes" = "#78A1BB")) +
  labs(title = "Deposit Rate by Housing Loan Status",
       x = "Housing Loan",
       y = "Proportion") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

## Deposit rate by Loan
ggplot(bankdata, aes(x = loan, fill = deposit)) +
  geom_bar(position = "fill", color = "black") +
  scale_fill_manual(values = c("no" = "#00A6A6", "yes" = "#F7EFA2")) +
  labs(title = "Deposit Rate by Personal Loan Status",
       x = "Personal Loan",
       y = "Proportion") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

## Faceted Heatmap -> Housing × Loan by Deposit Outcome
hl_data <- bankdata %>%
  count(housing, loan, deposit)

ggplot(hl_data, aes(x = housing, y = loan, fill = n)) +
  geom_tile(color = "white", linewidth = 1) +
  scale_fill_gradient(low = "#D6F5FF", high = "#0077B6") +
  facet_wrap(~ deposit) +
  labs(title = "Housing × Loan by Deposit Outcome",
       x = "Housing Loan",
       y = "Personal Loan",
       fill = "Count") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

## Deposit Rate by Contact Method
ggplot(bankdata, aes(x = contact, fill = deposit)) +
  geom_bar(position = "fill", color = "black") +
  scale_fill_manual(values = c("no" = "#A3C4F3", "yes" = "#B5EAD7")) +
  labs(title = "Deposit Rate by Contact Method",
       x = "Contact Method",
       y = "Proportion") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    legend.position = "none"
  )

## Deposit Outcome by Previous Campaign Result
pout_summary <- bankdata %>%
  group_by(poutcome, deposit) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n))

ggplot(pout_summary, aes(x = prop, y = poutcome, color = deposit)) +
  geom_point(size = 4) +
  geom_line(aes(group = poutcome), linewidth = 1) +
  scale_color_manual(values = c("no" = "#FF9AA2", "yes" = "#77DD77")) +
  labs(title = "Deposit Outcome by Previous Campaign Result",
       x = "Proportion",
       y = "Previous Outcome") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    legend.position = "none"
  )


#################### Analysis of the relationship between variables

## Deposit Conversion Rate by Previous Campaign Outcome
#Frequency band + conversion rate calculation
poutcome_table <- bankdata %>%
  group_by(
    "Previous Campaign Outcome" = poutcome,
    "Deposit Decision" = deposit
  ) %>%
  summarise(
    "Number of Clients" = n(),
    .groups = "drop"
  ) %>%
  group_by(`Previous Campaign Outcome`) %>%
  mutate(
    "Proportion Within Group" = `Number of Clients` / sum(`Number of Clients`)
  )

poutcome_table
View(poutcome_table)

ggplot(
  poutcome_table %>% filter(`Deposit Decision` == "yes"),
  aes(
    x = `Proportion Within Group`,
    y = `Previous Campaign Outcome`
  )
) +
  geom_point(color = "#7FBF7B", size = 3) +   # ירוק פסטלי
  labs(
    title = "Deposit Conversion Rate by Previous Campaign Outcome",
    x = "Proportion of Clients Who Opened a Deposit",
    y = "Previous Campaign Outcome"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      face = "bold",
      hjust = 0.5,
      size = 14
    ),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

## Age Distribution by Age Group and Deposit Decision
ggplot(
  bankdata,
  aes(
    x = age_group,
    y = age,
    fill = deposit
  )
) +
  geom_boxplot(alpha = 0.7, outlier.size = 0.8) +
  labs(
    title = "Age Distribution by Age Group and Deposit Decision",
    x = "Age Group",
    y = "Age"
  ) +
  scale_fill_manual(
    values = c("yes" = "#A1D99B", "no" = "#FC9272")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    legend.title = element_text(face = "bold")
  )


## Deposit Conversion Rate by Account Balance Status
balance_table <- bankdata %>%
  group_by(
    "Positive Account Balance" = balance_positive,
    "Deposit Decision" = deposit
  ) %>%
  summarise(
    "Number of Clients" = n(),
    .groups = "drop"
  ) %>%
  group_by(`Positive Account Balance`) %>%
  mutate(
    "Proportion Within Group" = `Number of Clients` / sum(`Number of Clients`)
  )

balance_table
View(balance_table)

ggplot(
  balance_table %>% filter(`Deposit Decision` == "yes"),
  aes(
    x = `Positive Account Balance`,
    y = `Proportion Within Group`,
    group = 1
  )
) +
  geom_line(color = "#BC80BD", linewidth = 1) +
  geom_point(color = "#BC80BD", size = 3) +
  labs(
    title = "Deposit Conversion Rate by Account Balance Status",
    x = "Positive Account Balance",
    y = "Proportion of Clients Who Opened a Deposit"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

## Deposit Decision by Previous Contact Status

install.packages("ggbeeswarm")
library(ggbeeswarm)

ggplot(
  bankdata,
  aes(
    x = was_contacted_before,
    y = deposit,
    color = was_contacted_before
  )
) +
  geom_quasirandom(
    alpha = 0.4,
    size = 1.2,
    width = 0.25
  ) +
  scale_color_manual(
    values = c("#8DA0CB", "#B3B3B3")
  ) +
  labs(
    title = "Deposit Decision by Previous Contact Status",
    x = "Previously Contacted",
    y = "Deposit Decision"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    legend.position = "none"
  )

#####################################################################################################
########################################## Logistic Regression + Random Forest Models

#################### Defining the learning problem and preparing the data for models
str(bankdata$deposit)
bankdata$deposit <- factor(bankdata$deposit, levels = c("no", "yes"))
model_data <- bankdata %>%
  select(
    deposit,
    age_group,
    balance_positive,
    was_contacted_before,
    poutcome,
    num_contacts_total
  )

#splitting the data
library(caTools)

set.seed(123)

split <- sample.split(model_data$deposit, SplitRatio = 0.7)

train_data <- subset(model_data, split == TRUE)
View(train_data)
test_data  <- subset(model_data, split == FALSE)
View(test_data)

#################### Logistic Regression
## Training the model

log_model <- glm(
  deposit ~ age_group + balance_positive + was_contacted_before +
    poutcome + num_contacts_total,
  data = train_data,
  family = binomial
)

summary(log_model)

## Model performance evaluation

#Calculate probabilities on the test set
log_pred_prob <- predict(
  log_model,
  newdata = test_data,
  type = "response"
)

#Converting probabilities to binary prediction (Yes / No)
log_pred_class <- ifelse(log_pred_prob > 0.5, "yes", "no")

log_pred_class <- factor(
  log_pred_class,
  levels = c("no", "yes")
)

#Confusion Matrix
conf_matrix <- table(
  Predicted = log_pred_class,
  Actual = test_data$deposit
)

conf_matrix

#Accuracy calculation
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
accuracy

## Presenting model findings using ROC Curve

install.packages("pROC")
library(pROC)

roc_log <- roc(
  response = test_data$deposit,
  predictor = log_pred_prob,
  levels = c("no", "yes")
)

auc_log <- auc(roc_log)
auc_log

plot(
  roc_log,
  col = "#4F81BD",
  lwd = 2,
  main = "ROC Curve – Logistic Regression Model"
)
abline(a = 0, b = 1, lty = 2, col = "gray")


#################### Random Forest
## Training the model

install.packages("randomForest")
library(randomForest)

set.seed(123)

rf_model <- randomForest(
  deposit ~ age_group + balance_positive + was_contacted_before +
    poutcome + num_contacts_total,
  data = train_data,
  ntree = 300,
  importance = TRUE
)

rf_model


## Model performance evaluation

#Calculate probabilities on the test set
rf_pred_class <- predict(
  rf_model,
  newdata = test_data,
  type = "class"
)

#Confusion Matrix
rf_conf_matrix <- table(
  Predicted = rf_pred_class,
  Actual = test_data$deposit
)

rf_conf_matrix

#Accuracy calculation
rf_accuracy <- sum(diag(rf_conf_matrix)) / sum(rf_conf_matrix)
rf_accuracy

## Presenting model findings using ROC Curve
rf_pred_prob <- predict(
  rf_model,
  newdata = test_data,
  type = "prob"
)[, "yes"]

library(pROC)

rf_roc <- roc(
  response = test_data$deposit,
  predictor = rf_pred_prob,
  levels = c("no", "yes")
)

rf_auc <- auc(rf_roc)
rf_auc

plot(
  rf_roc,
  col = "#2E8B57",
  lwd = 2,
  main = "ROC Curve – Random Forest Model"
)
abline(a = 0, b = 1, lty = 2, col = "gray")

