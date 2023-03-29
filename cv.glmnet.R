# Load the required libraries
library(dplyr)
library(tidyr)
library(caret)
library(glmnet)
library(readr)

# Load the data
data_ori <- read_rds("datos_prep/datos_procesados.rds")
colnames(data_ori)
vars <- c(
  "city","gender",
  "movac_obesity","movac_cvrisk","movac_pollut","movac_traffic","movac_academic",
  "parent_studies","family_vehicle","household_income",
  "ida_time_g1",
          "dens_cat",
          "emaps_cat",
          "active_mode")
  
# Preprocess the data
data <- data_ori %>% 
  select(vars)%>% 
  # Convert categorical columns to factor
  mutate(across(everything(), as.factor)) %>%  
  drop_na()  # Drop rows with missing values

# Split the data into training and testing sets
set.seed(1977)
train_index <- createDataPartition(data$active_mode, p = 0.7, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Create the model matrix and outcome vector
x <- model.matrix(active_mode ~ ., data = train_data) %>% as.matrix()
y <- train_data$active_mode %>% as.numeric()

# Perform cross-validated elastic net regression to select predictors
cv_model <- cv.glmnet(x, y, alpha = 0.5, family = "binomial", type.measure = "class")
cv_model$lambda.1se

summary(cv_model)
plot(cv_model)

# Extract the selected predictors
coefs <- coef.glmnet(cv_model, s = "lambda.1se")|>
  as.matrix()|>
  as.data.frame()%>% 
  rename(coef=lambda.1se)%>%
  mutate(var=rownames(.))
 
coefs

coefs_sel <- coefs|>
  filter(coef != 0,
         var!="X.Intercept."
         )

coefs_sel

  # Train a multinomial logistic regression model with the selected predictors
  # 
# Create the model matrix and outcome vector
data.f <- train_data|>
  select(city,
         gender,
         parent_sudies,
         ida_time_g1,
         dens_cat,
         emaps_cat)

x1 <- model.matrix(active_mode ~ ., data = train_data) %>% as.matrix()
y1 <- train_data$active_mode %>% as.numeric()

full_model <- glmnet(active_mode ~ 
                                
                        data = train_data)

base_model <- nnet::multinom(active_mode ~ 
                               city +
                               gender +
                               parent_studies +
                               movac_cvrisk+
                               ida_time_g1,
                             # dens_cat+
                             # emaps_cat,
                             data = train_data)

dens_model <- nnet::multinom(active_mode ~ 
                               city +
                               gender +
                               parent_studies +
                               movac_cvrisk+
                               ida_time_g1+
                             dens_cat,
                             # emaps_cat,
                             data = train_data)

emaps_model <- nnet::multinom(active_mode ~ 
                               city +
                               gender +
                               parent_studies +
                               movac_cvrisk+
                               ida_time_g1+
                               # dens_cat,
                             emaps_cat,
                             data = train_data)
summary(full_model)
summary(base_model)
summary(dens_model)
summary(emaps_model)

anova(full_model,dens_model)
# Evaluate the model on the testing set
# 
predicted_full <- predict(full_model, newdata = test_data)
confusionMatrix(predicted_full, test_data$active_mode)


# Train a multinomial logistic regression model with the selected predictors
base_model <- nnet::multinom(active_mode ~ 
                               city +
                               gender +
                               parent_studies +
                               movac_cvrisk+
                               ida_time_g1,
                               # dens_cat+
                               # emaps_cat,
                             data = train_data)

summary(base_model)
# Evaluate the model on the testing set
# 
predicted_base <- predict(base_model, newdata = test_data)
confusionMatrix(predicted_base, test_data$active_mode)



anova(full_model,base_model)

