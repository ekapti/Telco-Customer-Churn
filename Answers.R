library(MASS) 
library(tidyverse) 
library(readxl) 
library(e1071)
library(dplyr)
library(tidyr)

# Adjust the path as needed

# Telco Customer Churn
telco <- read.csv('data2.csv')
# Recode "Churn" variable for classification
telco <- telco %>% 
  mutate(Churn = recode_factor(Churn, "No" = "No", "Yes" = "Yes"))

# Function for analyzing confusion matrices
confusion_matrix_results <- function(table_matrix, positive_value) {
  pos_row = which(rownames(table_matrix)  == positive_value)
  pos_col = which(colnames(table_matrix)  == positive_value)
  
  TP <- table_matrix[pos_row, pos_col] 
  FN <- table_matrix[pos_row, -pos_col] 
  FP <- table_matrix[-pos_row, pos_col] 
  TN <- table_matrix[-pos_row, -pos_col]
  
  results <-
    data.frame(correct = c(TN + TP, round((TN + TP)/sum(table_matrix),3)), misclassified = c(FN + FP, round((FN + FP)/sum(table_matrix),3)), true_pos = c(TP, round(TP/sum(table_matrix[pos_row,]),3)), false_neg = c(FN, round(FN/sum(table_matrix[pos_row,]),3)), true_neg = c(TN, round(TN/sum(table_matrix[-pos_row,]),3)), false_pos = c(FP, round(FP/sum(table_matrix[-pos_row,]),3)))
  
  rownames(results) <-  c("Observations", "Rate")
  
  return(results)
}  

p1 <- telco %>%
  group_by(PaymentMethod) %>%
  summarise(avg_TenureMonths = mean(TenureMonths),
            avg_monthly_charges = mean(MonthlyCharges),
            customers = n(),
            churn_yes_rate = length(which(Churn=='Yes'))/customers)

p2 <- telco %>% 
  group_by(PaymentMethod, Contract) %>%
  summarise(customers = n(),
            churn_yes_rate = length(which(Churn=='Yes'))/customers) %>%
  select(-customers) %>%
  spread(key = Contract, value = churn_yes_rate)

# Question 2 part a
ggplot(data = telco, aes(x = Contract, y = TenureMonths)) +
  geom_boxplot(aes(color = Churn)) +
  facet_grid(PaymentMethod ~ InternetService)

# Question 2 part b

ggplot(data = telco, aes(x = MonthlyCharges, y = TenureMonths)) +
  geom_point(aes(color = Churn)) +
  facet_wrap(~PaymentMethod )

# Question 3 part a

model <- glm(Churn ~ Contract + PaymentMethod + MonthlyCharges + TenureMonths, 
             family = binomial(link = 'logit'), data = telco )

customer_one <- with(telco, data.frame(PaymentMethod = 'Electronic check',
                                       Contract = 'Month-to-month',
                                       MonthlyCharges = 80, 
                                       TenureMonths = 30))
customer_one_probability <- predict(model, newdata = customer_one, type = 'response')  

customer_two <- with(telco, data.frame(PaymentMethod = 'Mailed check',
                                       Contract = 'Month-to-month',
                                       MonthlyCharges = 80, 
                                       TenureMonths = 30))

customer_two_probability <-  predict(model, newdata = customer_two, type = 'response')  

customer_one_probability/customer_two_probability
# %45 more likely to leave.

# Question 3 part b

logistic_subset <- telco %>%
  select(Contract, PaymentMethod, MonthlyCharges, TenureMonths)

churn_logistic <- predict(model, newdata = logistic_subset, type = 'response') %>%
  cut(c(0,0.4,1), labels = c('No', 'Yes')) 

logistic_subset <- cbind(logistic_subset, churn_logistic)

# Question 4
model_b <- naiveBayes(Churn ~ Contract + PaymentMethod + MonthlyCharges + TenureMonths, 
                      data = telco )
naive_subset <- telco %>%
  select(Contract, PaymentMethod, MonthlyCharges, TenureMonths)

churn_bayes <- predict(model_b, newdata = naive_subset, type = 'class') 
churn_bayes <- cut(churn_bayes, c(0, 0.4, 1), labels = c('No', 'Yes')) 

naive_subset <- cbind(naive_subset, churn_bayes)

# Question 5

confusion_matrix_results(table(telco$Churn, churn_bayes), 'Yes')
confusion_matrix_results(table(telco$Churn, churn_logistic), 'Yes')

# According to these results, i would choose according to the lowest
# Type 1 error since Type 1 error is more costly than Type 2 error. 
# it does not cost a lot when you predict that customer would 
# leave but he/she does not in fact (Type 2) compared to predicting
#that customer would stay but in fact he/she leaves. I would choose
#the naive bayes model 
# since Type 1 error probability is lower thant logit model (0.264 vs 
# 0.373). 
