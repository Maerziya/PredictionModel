##################################################
##################################################
##########results from mixed model################
##################################################
##################################################

#read csv
pred_mm = read.csv("pred_mm2.csv")

#merge
testing_data_mm <- merge(testing_data,pred_mm,by="Number")

#get ae_mm
testing_data_mm$ae_mm <- abs(testing_data_mm$pred_mm - testing_data_mm$real)

#boxplot
boxplot(testing_data_mm$ae_mm, ylim = c(0, 60))

testing_data_1 <- testing_data_mm[testing_data_mm$ARAT < 22, ]
boxplot(testing_data_1$ae_mm, 
        xlab = "ARAT<22", ylab="Absolute error",
        ylim = c(0, 15))

testing_data_2 <- testing_data_mm[testing_data_mm$ARAT >= 22 & testing_data_mm$ARAT < 47, ]
boxplot(testing_data_2$ae_mm, 
        xlab = "ARAT [22,47)", ylab="Absolute error",
        ylim = c(0, 15))

testing_data_3 <- testing_data_mm[testing_data_mm$ARAT >= 47 & testing_data_mm$ARAT < 58, ]
boxplot(testing_data_3$ae_mm, 
        xlab = "ARAT [47,58)", ylab="Absolute error",
        ylim = c(0, 15))

#
test_predicted_mm <- testing_data_mm$pred_mm
test_predicted_rf <- test_predicted


attach(testing_data_mm)
par(mfrow=c(2,1))

plot(test_actual, test_predicted_rf)
abline(a = 0,                # Add straight line
       b = 1,
       col = "red",
       lwd = 2)


plot(test_actual, test_predicted_mm)
abline(a = 0,                # Add straight line
       b = 1,
       col = "red",
       lwd = 2)




attach(testing_data_mm)
par(mfrow=c(3,2))
boxplot(testing_data1$ae, 
        xlab = "ARAT<22", ylab="Absolute error",
        ylim = c(0, 30))

boxplot(testing_data_1$ae_mm, 
        xlab = "ARAT<22", ylab="Absolute error",
        ylim = c(0, 30))

boxplot(testing_data2$ae, 
        xlab = "ARAT [22,47)", ylab="Absolute error",
        ylim = c(0, 30))

boxplot(testing_data_2$ae_mm, 
        xlab = "ARAT [22,47)", ylab="Absolute error",
        ylim = c(0, 30))

boxplot(testing_data3$ae, 
        xlab = "ARAT [47,58)", ylab="Absolute error",
        ylim = c(0, 30))

boxplot(testing_data_3$ae_mm, 
        xlab = "ARAT [47,58)", ylab="Absolute error",
        ylim = c(0, 30))




##################################################
##################################################
##########results from RF2########################
##################################################
##################################################

# Select data from first measurement.
data_first_measurement <- data3_Days %>%
  filter(measurement==1) %>%
  select(Number, ARAT, FMARM, Days, GENDER, NIHSS, SA, FE, AGE, BAMFORD, RTPA, MIARM, MILEG, Sens, Neglect, AFFECTED_BODYSIDE, CIRSTOT, PREFERRED_HAND, Index1)

# Select relevant data from last measurement.
data_last_measurement <- data3_Days %>%
  filter(measurement==2) %>%
  select(Number, ARAT)%>%
  rename(last_Number = Number,last_ARAT = ARAT)

# Add data from last measurement.
data_final <- cbind(data_first_measurement, data_last_measurement)
data_final <- as_tibble(data_final)

data_final$last_Number <- NULL

# Split the data into training and testing sets
set.seed(30)
gap_split <- initial_split(data_final, prop=0.75)
training_data <- training(gap_split)
testing_data <- testing(gap_split)

#Calculate the dimensions of both training_data and testing_data
dim(training_data)
dim(testing_data)
# Build a random forest model
rf_model <- randomForest(last_ARAT ~ ., data = training_data, ntree = 500, importance = TRUE)

# Select the most important features

importance <- importance(rf_model)
print(importance)
varImpPlot(rf_model)


########
# Split the CV dataset in five groups
cv_split <- vfold_cv(training_data, v=5) 
dim(cv_split)

# Create a train and validate dataset.
cv_data <- cv_split %>%
  mutate(train = map(splits, ~training(.x)),
         validate = map(splits, ~testing(.x)))

#cv_data
# build a random forest model for each fold 
cv_models_rf <- cv_data %>%
  mutate(model = map(train, ~randomForest(formula= last_ARAT ~ AGE + ARAT + SA + FE + FMARM + MIARM + NIHSS+ MILEG + Sens + CIRSTOT + Days + GENDER + BAMFORD + RTPA + AFFECTED_BODYSIDE + PREFERRED_HAND + Neglect ,
                                          data = .x, num.tree=100, seed=42)))
#cv_models_rf
# Generate predictions for each model.
cv_pred_rf <- cv_models_rf %>%
  mutate(validate_predicted = map2(.x= model, .y=validate, ~predict(.x,.y)))  

# Evaluate a random forest model
cv_pred_rf=cv_pred_rf %>% 
  mutate(validate_actual=map(validate,~.x$last_ARAT))

# Calculate validate MAE for each fold
cv_eval_rf <- cv_pred_rf %>% 
  mutate(validate_mae = map2_dbl(validate, validate_predicted, ~mae(actual = .x$last_ARAT, predicted = .y)))

# Print the validate_mae column
cv_eval_rf$validate_mae

# Calculate the mean of validate_mae column
mean(cv_eval_rf$validate_mae)

# Fine tune the model
# Prepare for tuning cross validation folds by varying mtry
cv_tune <- cv_data %>% 
  crossing(mtry = 2:8) 

# Build a model for each fold & mtry combination
cv_model_tunerf <- cv_tune %>% 
  mutate(model = map2(.x = train, .y = mtry, ~ranger(formula = last_ARAT~ AGE + ARAT + SA + FE + FMARM + MIARM + NIHSS+ MILEG + Sens + CIRSTOT + Days + GENDER + BAMFORD + RTPA + AFFECTED_BODYSIDE + PREFERRED_HAND + Neglect, 
                                                     data = .x, mtry = .y, 
                                                     num.trees = 100, seed = 42)))

# Generate validate predictions for each model
cv_pred_tunerf <- cv_model_tunerf %>% 
  mutate(validate_predicted = map2(.x = model, .y = validate, ~predict(.x, .y)$predictions))

cv_pred_tunerf=cv_pred_tunerf %>% 
  mutate(validate = map(validate,~.x$last_ARAT))


# Calculate validate MAE for each fold and mtry combination
cv_eval_tunerf <- cv_pred_tunerf %>% 
  mutate(validate_mae = map2_dbl(.x = validate, .y = validate_predicted, ~mae(actual = .x, predicted = .y)))

# Calculate the mean validate_mae for each mtry used  
cv_eval_tunerf %>% 
  group_by(mtry) %>% 
  summarise(mean_mae = mean(validate_mae))

# Build the model using all training data and the best performing parameter
best_model <- ranger(formula = last_ARAT~ AGE + ARAT + SA + FE + FMARM + MIARM + NIHSS+ MILEG + Sens + CIRSTOT + Days + GENDER + BAMFORD + RTPA + AFFECTED_BODYSIDE + PREFERRED_HAND + Neglect, data = training_data,
                     mtry = 7, num.trees = 100, seed = 42)

# Predict life_expectancy for the testing_data
test_predicted_prf <- predict(best_model, testing_data)$predictions

testing_data$pred_prf <- test_predicted_prf
testing_data$real <- test_actual

testing_data$ae_prf <- abs(testing_data$pred_prf - testing_data$real)

# Calculate the test MAE
mae(test_actual, test_predicted_prf)




###comparison plots######

#ggplot for figure5
library(ggplot2)
library("ggpubr")
theme_set(
  theme_bw() +
    theme(legend.position = "top")
)

# Load data and convert dose to a factor variable
library("writexl")


#read csv
plot_3methods = read.csv("3methods_2.csv")


# Box plot
y <- ggplot(plot_3methods, aes(x = actual_ARAT_score, y = absolute_error)) + 
  geom_boxplot(aes(fill = method), position = position_dodge(0.9)) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#56B4E9")) +
  labs(x="Actual ARAT Score", y="absolute error") +
  theme(axis.text.x = element_text(size = 9), axis.text.y = element_text(size = 8))
y
