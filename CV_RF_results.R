#load package
library(ranger)
library(randomForest)
library(rsample)
library(dplyr)
library(tidyr)
library(Metrics)

#load data
load("finaldata.RData")

# Split data into 75% training and 25% testing
gap_split <- initial_split(data_final, prop=0.75)
training_data <- training(gap_split)
testing_data <- testing(gap_split)

#Calculate the dimensions of both training_data and testing_data
dim(training_data)
dim(testing_data)

# Split the CV dataset in five groups
cv_split <- vfold_cv(training_data, v=5) 
dim(cv_split)

# Create a train and validate dataset.
cv_data <- cv_split %>%
  mutate(train = map(splits, ~training(.x)),
         validate = map(splits, ~testing(.x)))

head(cv_data)

# build a random forest model for each fold 
cv_models_rf <- cv_data %>%
  mutate(model = map(train, ~randomForest(formula= last_ARAT ~ FMARM + NIHSS + ARAT + GENDER + SA + FE
                                          , data=training_data, num.tree=100, seed=42)))

# Make predictions for each model.
cv_pred_rf <- cv_models_rf %>%
  mutate(validate_predicted = map2(model, validate, ~predict(.x,.y)))

# Calculate validate MAE for each fold
cv_eval_rf <- cv_pred_rf %>% 
  mutate(validate_mae = map2_dbl(validate, validate_predicted, ~mae(actual = .x$last_ARAT, predicted = .y)))

# Print the validate_mae column
cv_eval_rf$validate_mae

# Calculate the mean of validate_mae column
mean(cv_eval_rf$validate_mae)

# Prepare for tuning cross validation folds by varying mtry
cv_tune <- cv_data %>% 
  crossing(mtry = 2:10) 

cv_tune
# Build a model for each fold & mtry combination
cv_model_tunerf <- cv_tune %>% 
  mutate(model = map2(.x = train, .y = mtry, ~ranger(formula = last_ARAT~., 
                                                     data = .x, mtry = .y, 
                                                     num.trees = 100, seed = 42)))
cv_model_tunerf

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
best_model <- ranger(formula = last_ARAT~., data = training_data,
                     mtry = 6, num.trees = 100, seed = 42)

# Prepare the test_actual vector
test_actual <- testing_data$last_ARAT

# Predict life_expectancy for the testing_data
test_predicted <- predict(best_model, testing_data)$predictions

# Calculate the test MAE
mae(test_actual, test_predicted)

#plot
difference <- as_tibble(abs(test_actual-test_predicted))

ggplot(data = difference, aes(x="", y=value)) + geom_boxplot() + xlab('test data') + ylab('absolute_error') + ggtitle('Boxplot of absolute errors (n=64)') + theme(aspect.ratio=1)
plot(test_actual, test_predicted)