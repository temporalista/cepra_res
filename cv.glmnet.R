# Load the required libraries
library(dplyr)
library(tidyr)
library(caret)
library(glmnet)
library(readr)

# Load the data
data_ori <- read_rds("datos_prep/datos_procesados.rds")
colnames(data_ori)
vars <- c("city","sex","movac_obesity","movac_cvrisk","movac_pollut","movac_traffic","movac_academic","parent_studies",    "family_vehicle","household_income","active_mode" )
  
# Preprocess the data
data <- data_ori %>% 
  select(vars)%>% 
  # Convert location columns to character
  mutate(across(everything(), as.factor)) %>%  
  # Convert categorical columns to factor
  drop_na()  # Drop rows with missing values

# Split the data into training and testing sets
set.seed(123)
train_index <- createDataPartition(data$active_mode, p = 0.7, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Create the model matrix and outcome vector
x <- model.matrix(active_mode ~ ., data = train_data) %>% as.matrix()
y <- train_data$active_mode %>% as.numeric()

# Perform cross-validated elastic net regression to select predictors
cv_model <- cv.glmnet(x, y, alpha = 0.5, family = "multinomial", type.measure = "class")
cv_model$lambda.1se

summary(cv_model)
plot(cv_model)

# Extract the selected predictors
coefs <- coef.glmnet(cv_model, s = "lambda.1se")[["1"]]|>
    as.matrix()%>% 
  as.data.frame()

coefs

# Train a multinomial logistic regression model with the selected predictors
final_model <- multinom(active_mode ~ age + education + has_transit_pass + transit_cost + driving_license + employment_status, data = train_data)

# Evaluate the model on the testing set
predicted <- predict(final_model
                     