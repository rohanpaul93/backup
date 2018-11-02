# Clear environment
rm(list = ls())



# Load essential libraries
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(caret)
library(gridExtra)
library(corrplot)
library(h2o)
library(caTools)
library(xgboost)

#install.packages('ggthemes')
# Load train and test files
list.files()
train_data <- read.csv("train_LZdllcl.csv", stringsAsFactors = F)
test_data <- read.csv("test_2umaH9m.csv", stringsAsFactors = F)

# List general attributes of the data
dim(train_data) # 54808 rows with 14 cols
dim(test_data) # 23490 rows with 13 cols

str(train_data)
# Convert employee id and is_promoted to factor type in both train and test
train_data$employee_id <- as.factor(train_data$employee_id)
test_data$employee_id <- as.factor(test_data$employee_id)

train_data$is_promoted <- as.factor(ifelse(train_data$is_promoted == 0, "No", "Yes"))

# Check missing values column wise in train and test
sapply(train_data, function(x) sum(is.na(x))) # 4124 missing values for previous rating
sapply(test_data, function(x) sum(is.na(x))) # 1812 missing values for previous rating

# Character variables may instead have Blanks. Check if any character variables have blanks
sapply(train_data, function(x) sum(x == "")) # 2409 Blank values in education
sapply(test_data, function(x) sum(x == "")) # 1034 Blank values in education
# Looking at the data it is not possible to make any reasonable assumption for the missing 
# education values. Hence we will treate the Blanks as a separate category

# Convert character type vars to factors
character_variables <- as.integer(which(sapply(train_data, function(x) is.character(x))))
train_data_factor <- as.data.frame(sapply(train_data[,character_variables], function(x) as.factor(x)))
train_data[,character_variables] <-  train_data_factor

character_variables_test <- as.integer(which(sapply(test_data, function(x) is.character(x))))
test_data_factor <- as.data.frame(sapply(test_data[,character_variables], function(x) as.factor(x)))
test_data[,character_variables_test] <-  test_data_factor

rm(train_data_factor, test_data_factor)

# Check a summary of data
summary(train_data) # At a glance there aren't any unusual values
summary(test_data)

# Check distribution of target
prop.table(table(train_data$is_promoted))
# 91% people were not promoted compared to only 9% promoted. So dataset is skewed.

# Missing values imputation
# 1) Impute missing values in previous_year_rating. Missing values in previous_year_rating may 
# occur if the employee hasn't had a rating yet. Let us check if this is true for all cases
unique(train_data$length_of_service[is.na(train_data$previous_year_rating)]) # 1.
# Indeed our suspicion is correct. So in this case we can't just impute a random rating. 
# Instead let's convert previous_year_rating to a categorical variable and convert the NA's to a 
# new category

train_data$previous_year_rating <- paste("Rating", train_data$previous_year_rating, sep = "_")
train_data$previous_year_rating <- as.factor(train_data$previous_year_rating)

test_data$previous_year_rating <- paste("Rating", test_data$previous_year_rating, sep = "_")
test_data$previous_year_rating <- as.factor(test_data$previous_year_rating)

# 2) Similarly create new category for Blanks in education
train_data$education <- as.character(train_data$education)
test_data$education <- as.character(test_data$education)
train_data$education[train_data$education == "" | train_data$education == " "] = "Unknown"
test_data$education[test_data$education == "" | test_data$education == " "] = "Unknown"
train_data$education <- as.factor(train_data$education)
test_data$education <- as.factor(test_data$education)

# EDA - Data Visualization #

# 1) For categorical variables
# Create a function which outputs two plots. Count of the target variable categories and 
# percentage of the target variable in each category
Plotter_Categorical <- function(data, source_var, target_var){
  p1 <- ggplot(data, aes(x = data[,c(source_var)], fill = data[,c(target_var)])) + geom_bar() +
    scale_fill_tableau() + theme_solarized() + theme(axis.text.x = element_text(angle = 90)) +
    geom_text(stat = "count", aes(label = ..count..), vjust = -0.1, position = "nudge") +
    labs(x = source_var, y = target_var) + theme(legend.title = element_blank())
  
  p2 <- ggplot(data, aes(x = data[,c(source_var)], fill = data[,c(target_var)])) + geom_bar(position = "fill") +
    scale_fill_tableau() + theme_solarized() + theme(axis.text.x = element_text(angle = 90)) +
    labs(x = source_var, y = target_var) + theme(legend.title = element_blank())
  x11() 
  grid.arrange(p1, p2)
  
}

# a) For department
Plotter_Categorical(train_data, "department", "is_promoted")
# Sales and Marketing is the most common department. There does not seem to be an appreciable
# differnce between the classes as far as the response is concerned however. Unlikely to be an 
# important factor.

# b) For region
Plotter_Categorical(train_data, "region", "is_promoted")
# Region has too many unique categories. Let's see the percentage distribution
# of promotions in each
round(prop.table(table(train_data$region, train_data$is_promoted),1),2)
# Variation between 3 to 4%. Not worth keeping this variable for analysis. So we
# will drop region
train_data <- train_data %>% dplyr::select(-region)
test_data <- test_data %>% dplyr::select(-region)

# c) For education
Plotter_Categorical(train_data, "education", "is_promoted")
# The unknown category seems to have lowest percentage of promotions.

# d) For gender
Plotter_Categorical(train_data, "gender", "is_promoted")
# Almost equal percentage of males and females get promoted (indicating no Gender bias)

# e) For recruitment_channel
Plotter_Categorical(train_data, "recruitment_channel", "is_promoted")
# Referred people are fewest. However they seem to get promoted more as compared to others.

# f) For previous_year_rating
Plotter_Categorical(train_data, "previous_year_rating", "is_promoted")
# As expected there is a steady and observable increase in no. of promotions with rise in Ratings

# g) KPIs _met_80- Let's convert this to a factor variable and change labels to "Yes/No"
train_data$KPIs_met..80. <- ifelse(train_data$KPIs_met..80. == 0, "No", "Yes")
test_data$KPIs_met..80. <- ifelse(test_data$KPIs_met..80. == 0, "No", "Yes")
Plotter_Categorical(train_data, "KPIs_met..80.", "is_promoted")
# Very Important variable. The %age of people getting promotions increases by almost 5 times if they meet
# the KPI > 80 criteria

# 2) For numeric variables
Plotter_Numeric <- function(data, source_var, target_var){
  
  p1 <- ggplot(data, aes(x = data[,c(source_var)], fill = data[,c(target_var)])) +
    geom_histogram(aes(y = ..density..),position = "dodge", col = "black", bins = 30) +
    theme_gdocs() + scale_fill_tableau(name = target_var) + geom_density(alpha = 0.3) +
    labs(x = source_var, y = "density") 
  
  p2 <- ggplot(train_data, aes(x = data[,c(target_var)], y = data[,c(source_var)], fill = data[,c(target_var)])) +
    geom_boxplot() + theme_gdocs() + scale_fill_tableau(name = target_var) + 
    labs(x = target_var, y = source_var)

  x11() 
  grid.arrange(p1, p2)
  
}

# a) For no_of_trainings
Plotter_Numeric(train_data, "no_of_trainings", "is_promoted")
# Clearly indicates that an overwhelming majority of employees havve
# only undergone 1 or 2 trainings and it doesnt seem to have much bearing on promotions

# b) For age
Plotter_Numeric(train_data, "age", "is_promoted")
# Histogram and density plots indicate Age is normally distributed and does
# not seem to have much influence on being promoted or not


# c) For length_of_service
Plotter_Numeric(train_data, "length_of_service", "is_promoted")
# length_of_service falls of sharply below 10 years but is not an influential variable

# d) For awards_won
Plotter_Numeric(train_data, "awards_won.", "is_promoted")
# There are only 2 values of awrds_won, 0 and 1. Clearly people winning awards
# are much more likely to be promoted. So convert awards_won to a categorical var.
train_data$awards_won. = ifelse(train_data$awards_won. == 1, "Awards_won", "No_awards")
test_data$awards_won. = ifelse(test_data$awards_won. == 1, "Awards_won", "No_awards")
Plotter_Categorical(train_data, "awards_won.", "is_promoted")

# e) For avg_training_score
Plotter_Numeric(train_data, "avg_training_score", "is_promoted")
# Two observations can be made-
# 1) 25th percentile score of people getting promoted is at least 60
# 2) Training scores above 80 are much more likely to see people promoted.
# It would be worthwhile to perform a bivariate analysis of avg_training_score
# against other variables

# avg_training_score vs is_promoted vs KPI's met
ggplot(train_data, aes(x = avg_training_score, fill = is_promoted)) +
  geom_histogram(aes(y = ..density..),position = "dodge", col = "black", bins = 30) +
  theme_economist() + scale_fill_tableau() + geom_density(alpha = 0.3) +
  facet_wrap(~KPIs_met..80.)

# avg_training_score vs is_promoted vs awards_won
ggplot(train_data, aes(x = avg_training_score, fill = is_promoted)) +
  geom_histogram(aes(y = ..density..),position = "dodge", col = "black", bins = 30) +
  theme_economist() + scale_fill_tableau() + geom_density(alpha = 0.3) +
  facet_wrap(~awards_won.)

# avg_training_score vs is_promoted vs previous_year_rating
ggplot(train_data, aes(x = avg_training_score, fill = is_promoted)) +
  geom_histogram(aes(y = ..density..),position = "dodge", col = "black", bins = 30) +
  theme_economist() + scale_fill_tableau() + geom_density(alpha = 0.3) +
  facet_wrap(~previous_year_rating)

# Check if any of the numeric variables have very high correlation
cor_matrix <- cor(train_data[,c(6,7,9,12)])
str(train_data)
corrplot(cor_matrix, method = "number", type = "upper", bg = "lightgreen")
# Age and length_of_service have quite high correlation as expected. Also as
# we had seen earlier both variables have seemingly little influence on promotions
# So we will elect to drop length_of_service
train_data <- train_data %>% select(-length_of_service)
test_data <- test_data %>% select(-length_of_service)

# EDA completed #

# Check structure again
str(train_data)
str(test_data)
# Convert KPI's and awards won to factor vars
train_data$KPIs_met..80. = as.factor(train_data$KPIs_met..80.)
train_data$awards_won. = as.factor(train_data$awards_won.)

test_data$KPIs_met..80. = as.factor(test_data$KPIs_met..80.)
test_data$awards_won. = as.factor(test_data$awards_won.)

# Dummy variable creation- For algorithms needing it such as logistic regression
is_promoted <- train_data$is_promoted
combined_data <- rbind(train_data[,-ncol(train_data)], test_data)
combined_data_with_dummies <- combined_data

# Dummy for department
dummy <- as.data.frame(model.matrix(~department, data = combined_data_with_dummies))
combined_data_with_dummies <- cbind(combined_data_with_dummies[,-2], dummy[,-1])

# For education
dummy <- as.data.frame(model.matrix(~education, data = combined_data_with_dummies))
combined_data_with_dummies <- cbind(combined_data_with_dummies[,-2], dummy[,-1])

# For gender
combined_data_with_dummies$gender <- ifelse(combined_data_with_dummies$gender == "m",
                                            1, 0)
combined_data_with_dummies$gender <- as.factor(combined_data_with_dummies$gender)

# For recruitment_channel
dummy <- as.data.frame(model.matrix(~recruitment_channel, data = combined_data_with_dummies))
combined_data_with_dummies <- cbind(combined_data_with_dummies[,-3], dummy[,-1])

# For previous_year_rating
dummy <- as.data.frame(model.matrix(~previous_year_rating, data = combined_data_with_dummies))
combined_data_with_dummies <- cbind(combined_data_with_dummies[,-5], dummy[,-1])

# Awards won and KPI met
combined_data_with_dummies$KPIs_met..80. <- ifelse(combined_data_with_dummies$KPIs_met..80. == "Yes",1,0)
combined_data_with_dummies$awards_won. <- ifelse(combined_data_with_dummies$awards_won. == "Awards_won",1,0)

combined_data_with_dummies$KPIs_met..80. <- as.factor(combined_data_with_dummies$KPIs_met..80.)
combined_data_with_dummies$awards_won. <- as.factor(combined_data_with_dummies$awards_won.)

str(combined_data_with_dummies)

sapply(combined_data_with_dummies, function(x) sum(is.na(x))) # No missing values

# Again separate into train and test
train_data_with_dummies <- cbind(combined_data_with_dummies[1:nrow(train_data),], is_promoted)
test_data_with_dummies <- combined_data_with_dummies[(nrow(train_data) + 1):nrow(combined_data_with_dummies),]       

train_data_with_dummies$is_promoted <- ifelse(train_data_with_dummies$is_promoted == "Yes",1,0)

# Separate into train and validation
set.seed(123)

indices = sample.split(train_data_with_dummies$is_promoted, SplitRatio = 0.75)

train_data_with_dummies_2 = train_data_with_dummies[indices,]

validation_data_with_dummies = train_data_with_dummies[!(indices),]


#### Model Building ####
# 1) Try Logistic regression
h2o.init(nthreads = -1)

# Transfer data to cluster
train_data_with_dummies.h2o <- as.h2o(train_data_with_dummies_2)
validation_data_with_dummies.h2o <- as.h2o(validation_data_with_dummies)
test_data_with_dummies.h2o <- as.h2o(test_data_with_dummies)

#check column index number
colnames(train_data_with_dummies.h2o)

# Set dependent and independent vars
y.dep <- 26
x.indep <- 2:25

#### LR in H2O ####
lr.model <- h2o.glm(y = y.dep, x = x.indep, training_frame = train_data_with_dummies.h2o,
                    validation_frame = validation_data_with_dummies.h2o,
                    nfolds = 3, family = "binomial", seed = 123)
summary(lr.model)
h2o.varimp(lr.model)

# Predict on test data
validation_predictions <- as.data.frame(h2o.predict(lr.model, validation_data_with_dummies.h2o))

# Find optimal probability cutoff
validation_data_with_dummies$probability <- validation_predictions$p1
summary(validation_data_with_dummies$probability)

# Selecting cutoff values
cutoff_data <- data.frame(cutoff = 0, TP = 0, TN = 0, FP = 0,FN = 0)
cutoffs <- seq(0.0002834,0.9999993,length=200)

for(cutoff in cutoffs){
  predicted <- as.numeric(validation_data_with_dummies$probability > cutoff)
  TP = sum(predicted==1 & validation_data_with_dummies$is_promoted==1)
  TN = sum(predicted==0 & validation_data_with_dummies$is_promoted==0)
  FP = sum(predicted==1 & validation_data_with_dummies$is_promoted==0)
  FN = sum(predicted==0 & validation_data_with_dummies$is_promoted==1)
  cutoff_data <- rbind(cutoff_data, c(cutoff, TP, TN, FP, FN))
}
cutoff_data <- cutoff_data[-1,]

# calculate metrics
cutoff_data <- cutoff_data %>% mutate(P = TP+FN, N = TN+FP)
cutoff_data <- cutoff_data %>% mutate(Accuracy = (TP+TN)/(P+N),
                                      Precision = TP/(TP+FP),
                                      Recall = TP/(TP+FN))
cutoff_data <- cutoff_data %>% mutate(F1_score = 2*(Precision*Recall)/(Precision+Recall))

cutoff_max_F1 <- cutoff_data$cutoff[which.max(cutoff_data$F1_score)]

# Now predict on test data with entire train set
train_data_with_dummies_full.h2o <- as.h2o(train_data_with_dummies)
lr.model <- h2o.glm(y = y.dep, x = x.indep, training_frame = train_data_with_dummies_full.h2o,
                    nfolds = 3, family = "binomial", seed = 123)
predictions_glm <- as.data.frame(h2o.predict(lr.model, test_data_with_dummies.h2o))

# Submission 1- with the tested cutoff
submission_1 <- as.data.frame(cbind(as.integer(as.character(test_data$employee_id)), predictions_glm$p1))
colnames(submission_1) = c("employee_id","is_promoted")
submission_1$is_promoted <- ifelse(submission_1$is_promoted >= cutoff_max_F1,1,0)
write.csv(submission_1, "submission_1_Logistic_Regression_optimum_cutoff.csv", row.names = F)

# Submission 2- H2O predictions direct
submission_2 <- as.data.frame(cbind(as.integer(as.character(test_data$employee_id)), predictions_glm$predict))
colnames(submission_2) = c("employee_id","is_promoted")
write.csv(submission_2, "submission_2_Logistic_Regression_h2O_direct_predictions.csv", row.names = F)

# 2) Try Random Forest
train.h2o <- as.h2o(train_data)
test.h2o <- as.h2o(test_data)

colnames(train.h2o)
y.dep = 12
x.indep = 2:11

rf.model <- h2o.randomForest(y = y.dep, x = x.indep, training_frame = train.h2o,
                             nfolds = 3, ntrees = 500, seed = 123)
summary(rf.model)
h2o.varimp(rf.model)

# Predict on test data
predictions_rf <- as.data.frame(h2o.predict(rf.model, test.h2o))

# Submission 3- RF default
submission_3 <- as.data.frame(cbind(as.integer(as.character(test_data$employee_id)), as.character(predictions_rf$predict)))
colnames(submission_3) = c("employee_id","is_promoted")
submission_3$is_promoted <- ifelse(submission_3$is_promoted == "Yes",1,0)
write.csv(submission_3, "H:/Career Development/Analytics Vidhya/WNS Analytics/Submissions/submission_3_Random_Forest_default.csv", row.names = F)

# F1 score 0.49

# 3) Try Naive Bayes
nb.model <- h2o.naiveBayes(y = y.dep, x = x.indep, training_frame = train.h2o,
                             nfolds = 3, seed = 123)
summary(nb.model)

# Predict on test data
predictions_nb <- as.data.frame(h2o.predict(nb.model, test.h2o))

# Submission 4- NB default
submission_4 <- as.data.frame(cbind(as.integer(as.character(test_data$employee_id)), as.character(predictions_nb$predict)))
colnames(submission_4) = c("employee_id","is_promoted")
submission_4$is_promoted <- ifelse(submission_4$is_promoted == "Yes",1,0)
write.csv(submission_4, "H:/Career Development/Analytics Vidhya/WNS Analytics/Submissions/submission_4_Naive_Bayes.csv", row.names = F)

# 4) Try GBM with alpha = 0.1
gbm.model <- h2o.gbm(y = y.dep, x = x.indep, training_frame = train.h2o,
                    ntrees = 1000, learn_rate = 0.1, seed = 123)
summary(gbm.model)

# Predict on test data
predictions_gbm <- as.data.frame(h2o.predict(gbm.model, test.h2o))

# Submission 4- GBM with learn rate 0.1
submission_5 <- as.data.frame(cbind(as.integer(as.character(test_data$employee_id)), as.character(predictions_gbm$predict)))
colnames(submission_5) = c("employee_id","is_promoted")
submission_5$is_promoted <- ifelse(submission_5$is_promoted == "Yes",1,0)
write.csv(submission_5, "H:/Career Development/Analytics Vidhya/WNS Analytics/Submissions/submission_5_GBM_learn_rate_0.1.csv", row.names = F)

# Leaderbaord F1 score and CV score 0.51

#### Final Model Tuning ####
# Since the GBM model is giving best results we will try to tune it to further 
# improve leaderbord rank
# Grid search H2O
# Split the data for tuning
splits <- h2o.splitFrame(
  data = train.h2o, 
  ratios = c(0.6,0.2),   ## only need to specify 2 fractions, the 3rd is implied
  destination_frames = c("train.hex", "valid.hex", "test.hex"), seed = 1234
)
train <- splits[[1]]
valid <- splits[[2]]
test  <- splits[[3]]

## Try different depths
hyper_params = list( max_depth = seq(1,29,2) )
#hyper_params = list( max_depth = c(4,6,8,12,16,20) ) ##faster for larger datasets

grid <- h2o.grid(
  ## hyper parameters
  hyper_params = hyper_params,
  
  ## full Cartesian hyper-parameter search
  search_criteria = list(strategy = "Cartesian"),
  
  ## which algorithm to run
  algorithm="gbm",
  
  ## identifier for the grid, to later retrieve it
  grid_id="depth_grid",
  
  ## standard model parameters
  x = x.indep, 
  y = y.dep, 
  training_frame = train, 
  validation_frame = valid,
  
  ## more trees is better if the learning rate is small enough 
  ## here, use "more than enough" trees - we have early stopping
  ntrees = 10000,                                                            
  
  ## smaller learning rate is better
  ## since we have learning_rate_annealing, we can afford to start with a bigger learning rate
  learn_rate = 0.05,                                                         
  
  ## learning rate annealing: learning_rate shrinks by 1% after every tree 
  ## (use 1.00 to disable, but then lower the learning_rate)
  learn_rate_annealing = 0.99,                                               
  
  ## sample 80% of rows per tree
  sample_rate = 0.8,                                                       
  
  ## sample 80% of columns per split
  col_sample_rate = 0.8, 
  
  ## fix a random number generator seed for reproducibility
  seed = 1234,                                                             
  
  ## early stopping once the validation AUC doesn't improve by at least 0.01% for 5 consecutive scoring events
  stopping_rounds = 5,
  stopping_tolerance = 1e-4,
  stopping_metric = "AUC", 
  
  ## score every 10 trees to make early stopping reproducible (it depends on the scoring interval)
  score_tree_interval = 10                                                
)

## sort the grid models by decreasing AUC
sortedGrid <- h2o.getGrid("depth_grid", sort_by="auc", decreasing = TRUE)    
sortedGrid

# Higher depths lead to less AUC as do lower depths. So for further optimization 
# we will use only depths between 2 to 10
minDepth = 2
maxDepth = 10

# Final parameter tuning
hyper_params = list( 
  ## restrict the search to the range of max_depth established above
  max_depth = seq(minDepth,maxDepth,1),                                      
  
  ## search a large space of row sampling rates per tree
  sample_rate = seq(0.2,1,0.01),                                             
  
  ## search a large space of column sampling rates per split
  col_sample_rate = seq(0.2,1,0.01),                                         
  
  ## search a large space of column sampling rates per tree
  col_sample_rate_per_tree = seq(0.2,1,0.01),                                
  
  ## search a large space of how column sampling per split should change as a function of the depth of the split
  col_sample_rate_change_per_level = seq(0.9,1.1,0.01),                      
  
  ## search a large space of the number of min rows in a terminal node
  min_rows = 2^seq(0,log2(nrow(train.h2o))-1,1),                                 
  
  ## search a large space of the number of bins for split-finding for continuous and integer columns
  nbins = 2^seq(4,10,1),                                                     
  
  ## search a large space of the number of bins for split-finding for categorical columns
  nbins_cats = 2^seq(4,12,1),                                                
  
  ## search a few minimum required relative error improvement thresholds for a split to happen
  min_split_improvement = c(0,1e-8,1e-6,1e-4),                               
  
  ## try all histogram types (QuantilesGlobal and RoundRobin are good for numeric columns with outliers)
  histogram_type = c("UniformAdaptive","QuantilesGlobal","RoundRobin")       
)

search_criteria = list(
  ## Random grid search
  strategy = "RandomDiscrete",      
  
  ## limit the runtime to 60 minutes
  max_runtime_secs = 3600,         
  
  ## build no more than 100 models
  max_models = 100,                  
  
  ## random number generator seed to make sampling of parameter combinations reproducible
  seed = 1234,                        
  
  ## early stopping once the leaderboard of the top 5 models is converged to 0.1% relative difference
  stopping_rounds = 5,                
  stopping_metric = "AUC",
  stopping_tolerance = 1e-3
)

grid <- h2o.grid(
  ## hyper parameters
  hyper_params = hyper_params,
  
  ## hyper-parameter search configuration (see above)
  search_criteria = search_criteria,
  
  ## which algorithm to run
  algorithm = "gbm",
  
  ## identifier for the grid, to later retrieve it
  grid_id = "final_grid", 
  
  ## standard model parameters
  x = x.indep, 
  y = y.dep, 
  training_frame = train, 
  validation_frame = valid,
  
  ## more trees is better if the learning rate is small enough
  ## use "more than enough" trees - we have early stopping
  ntrees = 10000,                                                            
  
  ## smaller learning rate is better
  ## since we have learning_rate_annealing, we can afford to start with a bigger learning rate
  learn_rate = 0.05,                                                         
  
  ## learning rate annealing: learning_rate shrinks by 1% after every tree 
  ## (use 1.00 to disable, but then lower the learning_rate)
  learn_rate_annealing = 0.99,                                               
  
  ## early stopping based on timeout (no model should take more than 1 hour - modify as needed)
  max_runtime_secs = 3600,                                                 
  
  ## early stopping once the validation AUC doesn't improve by at least 0.01% for 5 consecutive scoring events
  stopping_rounds = 5, stopping_tolerance = 1e-4, stopping_metric = "AUC", 
  
  ## score every 10 trees to make early stopping reproducible (it depends on the scoring interval)
  score_tree_interval = 10,                                                
  
  ## base random number generator seed for each model (automatically gets incremented internally for each model)
  seed = 1234                                                             
)

## Sort the grid models by AUC
sortedGrid <- h2o.getGrid("final_grid", sort_by = "auc", decreasing = TRUE)    
sortedGrid

# Get the best model by AUC
gbm.model <- h2o.getModel(sortedGrid@model_ids[[1]])
gbm.model@parameters

#### Build Final model on entire train data with these parameters ####
gbm_final_model <- h2o.gbm(y = y.dep, x = x.indep, training_frame = train.h2o,
                           ntrees = 10000, learn_rate = 0.05, learn_rate_annealing = 0.99,
                           max_depth = 7, distribution = "bernoulli", sample_rate = 0.57,
                           col_sample_rate = 0.92, col_sample_rate_change_per_level = 1.04,
                           min_split_improvement = 0, histogram_type = "QuantilesGlobal",
                           score_tree_interval = 10, nbins = 256, nbins_cats = 16, stopping_rounds = 5,
                           stopping_metric = "AUC", stopping_tolerance = 0.0001,
                           nfolds = 5, seed = 1234)

# Cross validation parameters
gbm_final_model@model$cross_validation_metrics


# With length_of_service
train_old <- read.csv("train.csv")
test_old <- read.csv("test.csv")
train_data <- cbind(train_data, train_old$length_of_service)
test_data <- cbind(test_data, test_old$length_of_service)

data.table::setnames(train_data, "train_old$length_of_service", "length_of_service")
data.table::setnames(test_data, "test_old$length_of_service", "length_of_service")

train.h2o <- as.h2o(train_data)
test.h2o <- as.h2o(test_data)

colnames(train.h2o)

y.dep <- 12
x.indep <- c(2:11,13)

gbm_final_model <- h2o.gbm(y = y.dep, x = x.indep, training_frame = train.h2o,
                           ntrees = 10000, learn_rate = 0.05, learn_rate_annealing = 0.99,
                           max_depth = 7, distribution = "bernoulli", sample_rate = 0.57,
                           col_sample_rate = 0.92, col_sample_rate_change_per_level = 1.04,
                           min_split_improvement = 0, histogram_type = "QuantilesGlobal",
                           score_tree_interval = 10, nbins = 256, nbins_cats = 16, stopping_rounds = 5,
                           stopping_metric = "AUC", stopping_tolerance = 0.0001,
                           nfolds = 5, seed = 1234)

# Cross validation parameters
gbm_final_model@model$cross_validation_metrics

# Interesting- f1 score actually improved
# Use this as predictions
predictions_gbm <- as.data.frame(h2o.predict(gbm_final_model, test.h2o))

# Submission 7- GBM with learn rate 0.1
submission_7 <- as.data.frame(cbind(as.integer(as.character(test_data$employee_id)), as.character(predictions_gbm$predict)))
colnames(submission_7) = c("employee_id","is_promoted")
submission_7$is_promoted <- ifelse(submission_7$is_promoted == "Yes",1,0)
write.csv(submission_7, "H:/Career Development/Analytics Vidhya/WNS Analytics/Submissions/submission_7_GBM_with_employment_length.csv", row.names = F)

# We will choose this model with length_of_service as final model
#### The End ####