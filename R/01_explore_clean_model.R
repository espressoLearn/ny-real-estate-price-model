#clear global environment
rm(list = ls())

# import kaggle dataset
library(readr)
realEstate <- read_csv("data/realEstate.zip")
View(realEstate)

# explore general
library(dplyr) # load dplyr 
glimpse(realEstate) # view row and column counts, attributes with data types and sample values 
head(realEstate) # view first 6 obs
tail(realEstate) # view last 6 obs 
summary(realEstate) # get summary stats
str(realEstate) # view structure
library(skimr) # load skimr
skim(realEstate) # skim the data: 1,401,066 rows

# get unique values with freq 
table(realEstate$status) # get unique for status 
table(realEstate$state) # get unique for state, NY = 653,061


# state names too long, need abbr 
library(dplyr)
library(usdata) # load usdata for state abbr
real_abbr <- realEstate # new DF for the step
table(real_abbr$state$state) # get unique w freq before 
real_abbr <- filter(real_abbr, state != "Puerto Rico") # exclude PR since not in usdata
real_abbr$state <- as.factor(real_abbr$state) # convert char to factor
table(real_abbr$state) # get unique w freq after convert
real_abbr$state_abbr <- state2abbr(real_abbr$state) # add column for state abbr
head(real_abbr) # check obs 
table(real_abbr$state_abbr) # get unique abbr w freq after convert 
skim(real_abbr) # review after change: all good and 1,376,387 rows excl PR


# filter for NY state only  
library(dplyr)
real_NY <- real_abbr %>%
  filter(state_abbr == "NY")
summary(real_NY)
skim(real_NY)


# new column previously sold year for aggregation 
real_NY$prev_sold_year <- format(as.Date(real_NY$prev_sold_date), "%Y")
head(real_NY) # check it

# new column previously sold decade for aggregation
real_NY$prev_sold_decade <- ifelse(!is.na(real_NY$prev_sold_date), 
                                     paste0(substr(real_NY$prev_sold_date, start = 1, stop = 3),0),
                                     NA)
head(real_NY) # check it



###################################
#########    Outliers     ######### 
###################################

# Clean summary of appropriate numeric features
real_box <- select_if(real_NY, is.numeric) # keep numeric 
drop <- c("zip_code") # named columns to drop
real_box <- real_box[,!(names(real_box) %in% drop)]
summary(real_box)

library(tidyverse) # load tidyverse
real_box <- real_box %>% #exclude NA values
  drop_na(bed, bath, acre_lot, house_size, price)

summary(real_box)


# Visualize outliers 

# box plot 
boxplot(real_box, use.cols = TRUE)

# scaled box plot 
boxplot(scale(real_box), use.cols = TRUE)

######## REMOVE OUTLIERS #1 - BY CATG --- DO NOT USE #####

# DROP obs missing city - small volume 
library(tidyverse) # load tidyverse


# filter for NY state only  
library(dplyr)
real_NY2 <- realEstate %>%
  filter(state == "New York")
summary(real_NY2)


real_NY2 <- real_NY2 %>% 
  drop_na(city) 
nrow(real_NY2) # 653,061 obs in, 652,952 out

# Try detect and exclude by city 
library(dplyr)

# function to remove outliers by category
remove_outliers <- function(df, category_col, value_col, threshold = 1.5) {
  df %>%
    group_by({{ category_col }}) %>%
    filter(abs(scale({{ value_col }})) < threshold)
}

# Call the function to remove outliers by category for price 
real_NY2 <- remove_outliers(real_NY2, city, price)

# Call the function to remove outliers by category for house size 
real_NY2 <- remove_outliers(real_NY2, city, house_size)

# Call the function to remove outliers by category for bath
real_NY2 <- remove_outliers(real_NY2, city, bath)

# Call the function to remove outliers by category for bed
real_NY2 <- remove_outliers(real_NY2, city, bed)

# Call the function to remove outliers by category for acreage
real_NY2 <- remove_outliers(real_NY2, city, acre_lot)

summary(real_NY2) # after = 170,462 (too much removal)



########### FLAG OUTLIERS ONE FEATURE AT A TIME FOR REMOVAL ############


# Create function to flag outliers for treatment
# note - have to do one field at a time so that NAs in other columns do not skip the row 

outliers <- function(x) {
  
  Q1 <- quantile(x, probs=.25, na.rm = TRUE)
  Q3 <- quantile(x, probs=.75, na.rm = TRUE)
  iqr = Q3-Q1
  
  upper_limit = Q3 + (iqr*1.5)
  lower_limit = Q1 - (iqr*1.5)
  
  x > upper_limit | x < lower_limit
}


# flag outliers with new field per feature 

# flag the bed outliers

x1 = real_NY$bed #set var 

real_NY$bed_out <- ifelse(outliers(x1) == TRUE, TRUE, FALSE) # flag it 

table(real_NY$bed_out) # count it 

# flag the bath outliers

x2 = real_NY$bath

real_NY$bath_out <- ifelse(outliers(x2) == TRUE, TRUE, FALSE)

table(real_NY$bath_out) # count it 

# flag the acreage outliers

x3 = real_NY$acre_lot

real_NY$acre_out <- ifelse(outliers(x3) == TRUE, TRUE, FALSE)

table(real_NY$acre_out) # count it 

# flag the house size outliers

x4 = real_NY$house_size

real_NY$house_out <- ifelse(outliers(x4) == TRUE, TRUE, FALSE)

table(real_NY$house_out) # count it 


# flag the price outliers

x5 = real_NY$price

real_NY$price_out <- ifelse(outliers(x5) == TRUE, TRUE, FALSE)

table(real_NY$price_out) # count it 


# replace NA with 0 for outlier flag columns 

library(dplyr)

# Named columns to replace NA with 0
columns_to_replace <- c("bed_out", "bath_out", "acre_out", "house_out", "price_out") 

# Replace NA with 0 for specific columns
real_NY <- real_NY %>%
  mutate_at(vars(columns_to_replace), ~replace(., is.na(.), FALSE))

View(real_NY)

#### summarize outlier flags 

outliers_data <- real_NY[12:16]

library(dplyr)
library(ggplot2)

# Reshape the data frame for plotting
df_long <- real_NY[12:16] %>%
  tidyr::pivot_longer(cols = everything(), names_to = "column", values_to = "value")

# Plot the data
ggplot(df_long, aes(x = column, fill = factor(value))) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("TRUE" = "black", "FALSE" = "grey")) +
  labs(title = "Outliers by Feature", subtitle = "outliers in black",
       x = "Columns",
       y = "Proportion",
       fill = "Value") +
  theme_minimal()


# Exclude Outliers

library(dplyr)

# Columns subset where any value being TRUE should exclude the row
columns_subset <- c("bed_out", "bath_out", "acre_out", "house_out", "price_out") 

# Filter rows where any value in the subset is FALSE
real_NY_no_outlier <- real_NY %>%
  filter(rowSums(.[, columns_subset]) == 0)

View(real_NY_no_outlier)


## Visualize after outliers dropped

# get subset numeric columns 
real_box2 <- real_NY_no_outlier[, c("bed", "bath", "acre_lot", "house_size", "price")]

summary(real_box2)

# Visualize after outliers removed  

# box plot 
boxplot(real_box2, use.cols = TRUE)

# scaled box plot 
boxplot(scale(real_box2), use.cols = TRUE)


###################################
#########   Missingness   ######### 
###################################


### Analyze missingness
pacman::p_load(
  rio,           # import/export
  tidyverse,     # data mgmt and viz
  naniar,        # assess and visualize missingness
  mice           # missing data imputation
)

# Subset features for missingness 
View(real_NY_no_outlier)
NY_sub <- select(real_NY_no_outlier, c("bed","bath", "house_size", "price", "city"))
head(NY_sub)

# percent of ALL data frame values that are missing
pct_miss(NY_sub)

# Percent of rows with any value missing
pct_miss_case(NY_sub) 

# Percent of rows that are complete (no values missing)  
pct_complete_case(NY_sub)

# summary of # and % missing per feature
miss_var_summary(NY_sub)

# visualize % missing per feature
gg_miss_var(NY_sub, show_pct = TRUE)

# Heat plot of missingness across the dataset  
options(scipen = 999)
vis_miss(NY_sub, warn_large_data = FALSE)


ggplot(
  data = NY_sub,
  mapping = aes(x = price, y = bed)) +     
  geom_miss_point()


# shadow to create binary NA vs !NA 
shadowed_NY_sub <- NY_sub %>% 
  bind_shadow()

names(shadowed_NY_sub)

# Line plot of shadowed
ggplot(data = shadowed_NY_sub,          # data frame with shadow columns
       mapping = aes(x = bath, # numeric or date column
                     colour = house_size_NA)) + # shadow column of interest
  geom_density()                          # plots the density curves


##################################
######### HANDLE MISSING #########
##################################

# drop obs missing price or city
library(tidyverse) # load tidyverse

real_NY_drop_NA <- real_NY_no_outlier %>% 
  drop_na(city, zip_code, price) %>%
  filter(price !=0)
nrow(real_NY_drop_NA) # 504,507 remain

summary(real_NY_drop_NA)

# drop obs missing bath and house size

library(tidyverse) # load tidyverse
real_NY_drop_NA <- real_NY_drop_NA %>% 
  drop_na(bath, house_size) 
nrow(real_NY_drop_NA) # 163,646 rows dropped, 340,941 remain

summary(real_NY_drop_NA[1:10])

# Convert all categorical attributes to factor (if required)

library(dplyr)
real_NY_drop_NA <- real_NY_drop_NA %>%
  mutate(across(where(is.character), as.factor))

# Convert prev sold date to factor (misForest requires)
real_NY_drop_NA$prev_sold_date <- as.factor(real_NY_drop_NA$prev_sold_date)

# Convert zip to factor
real_NY_drop_NA$zip_code <- as.factor(real_NY_drop_NA$zip_code)


# remove the outlier flag columns
real_NY_drop_NA <- real_NY_drop_NA[1,1:11]
head(real_NY_drop_NA)


# Impute missing house size --- impute package unavail for my version of R! 

pacman::p_load(impute) 

# Impute missing bath based on multiple columns 
real_NY_Imputed <- impute_missing(
  real_NY_drop_NA, 
  method = "knn",  # Imputation method (e.g., "knn" for k-nearest neighbors)
  k = 5,          # Number of nearest neighbors
  target = c("house_size"),  # Columns to impute
  predictors = c("price", "city", "house_size")  # Factors to consider for imputation
)

# Impute missing house size w/ missForest - doesn't work getting error messages

pacman::p_load(missForest) 

head(real_NY_drop_NA)
View(real_NY_drop_NA)

real_NY_Imp1 <- missForest(real_NY_drop_NA)

# Impute missing house size w/ mice

pacman::p_load(mice) 

head(real_NY_drop_NA)

# Subset features for imputation  

NY_sub2 <- select(real_NY_drop_NA, c("bed","bath", "house_size", "price", "zip_code"))

head(NY_sub2)

real_NY_Imputed <- mice(NY_sub2)

# Generate imputed datasets
imputed_data <- complete(real_NY_Imputed)
View(imputed_data)
summary(imputed_data)

NY_done_clean <- cbind(real_NY_drop_NA, imputed_data)
summary(NY_done_clean)


##################



# exclude obs with missing data except for prev_sold_date and acre_lot
library(tidyverse) # load tidyverse
library(skimr)
real_NY_drop_NA <- real_NY %>% 
  drop_na(status, bed, bath, city, state, zip_code, house_size, price)
skim(real_NY_drop_NA) # skim with exclusions applied: 418,402 rows
summary(real_NY_drop_NA)

# rows dropped 
nrow(real_NY) - nrow(real_NY_drop_NA) #count dropped
(nrow(real_NY_drop_NA) - (nrow(real_NY)))/nrow(real_NY) # percent dropped
      
# export to csv (after removing outliers and missing)
write.csv(real_NY_drop_NA, "data/real_ny_clean.csv", row.names = FALSE)



# View data by prev_sold_year and price 
## filter based on mean for common compare across years 
library(dplyr)
summary(real_abbr)
real_NYf <- real_abbr %>%
  filter(bed == 3, 
         bath == 2, 
         is.na(acre_lot) | acre_lot < 0.5, 
         between(house_size, 1500, 2500),
         #!is.na(prev_sold_year),
         state_abbr == "NY")


# aggregate mean price by prev sold decade
real_aggr <- real_NYf %>%
  dplyr:: group_by (prev_sold_decade) %>%
  dplyr:: summarize (mean_price = mean(price))
print(real_aggr, n = 100)

# visualize bar
library(ggplot2)
library(scales)
options(scipen = 999)
ggplot(real_aggr, aes(x=prev_sold_decade, y=mean_price, group=1)) + 
  geom_bar(stat = "identity", 
           position = position_dodge2(0.5)) + 
  scale_y_continuous(labels = scales::label_number_si(), limits = c(0,750000)) +
  scale_x_discrete() +
  geom_text(aes(label=round(mean_price,digits= -3)), vjust= 1.5, color="white", size = 5) +
  ggtitle("NY Subset - Mean Price by Previously Sold Decade")





################################


# visualize hist - needs work 

library(ggplot2)
library(scales)
options(scipen = 999)
ggplot(real_aggr, aes(x=prev_sold_decade, y=mean_price, group=1)) + 
  geom_histogram(stat = "identity", 
           position = position_dodge2(0.5)) + 
  scale_y_continuous(labels = scales::label_number_si(), limits = c(0,750000)) +
  scale_x_discrete() +
  geom_text(aes(label=round(mean_price,digits= -3)), vjust= 1.5, color="white", size = 5)

# Import zillow median sale price raw all homes weekly
library(readr)
zillow <- read_csv("data/zillow_median_sale_price_week.csv")
View(zillow)

############ ############
########## PCA ################
############ ############

library(dplyr)
library(tidyverse) 
library(skimr)
library(ggplot2)

data(real_NY_drop_NA)
head(real_NY_drop_NA)
summary(real_NY_drop_NA)

# replace missing values with zeros (earlier assumption the data is valid at zero)
real_NY_clean2 <- real_NY_drop_NA
real_NY_clean2$acre_lot <- real_NY_clean2$acre_lot %>% replace (is.na(.),0)
summary(real_NY_clean2)

# note - excluding the target variable from PCA... just looking at predictors
# drop NA values to avoid error in svd(x, nu = 0, nv = k) : infinite or missing values in 'x'
NY_PCA <- select(real_NY_clean2, c("bed","bath", "acre_lot", "house_size"))

# scale the data since they are different measures 
NY_PCA <- scale(NY_PCA)

# supply subset from NY_PCA
NY_Pr <- prcomp(NY_PCA)

NY_Pr

summary(NY_Pr)

str(NY_Pr)
# view eigenvalues 
NY_Pr$x

# scree plot 
plot(NY_Pr, type = "l", main = "Scree Plot")

# bi plot
biplot(NY_Pr, scale = 0)


# add PC1 & 2 onto data and plot 
NY_Pr_Bind <- cbind(NY_PCA, NY_Pr$x[,1:3])
# view
head(NY_Pr_Bind)

# for modeling, add PC1 to original file
NY_clean_wPC1 <- cbind(real_NY_clean2, NY_Pr$x[,1])
# view
View(NY_clean_wPC1)
head(NY_clean_wPC1)

# rename for PC1
names(NY_clean_wPC1)[names(NY_clean_wPC1) == "NY_Pr$x[, 1]"] <- "pc1"

# subset before export to remove outlier flags
NY_clean_wPC1 <- NY_clean_wPC1[, -c(12:16)]

# export to csv for modeling 
write.csv(NY_clean_wPC1, "data/NY_clean_wPC1.csv", row.names = FALSE)



# plot
ggplot(NY_Pr_Bind, aes(PC1, PC2, col = PC3, fill = PC3)) +
  stat_ellipse(geom = "polygon", col = "black", alpha = 0.5) +
  geom_point(shape = 21, col = "black")
  
# Correl between variables and PCs
cor(NY_PCA, NY_Pr$x[,1:3])

#### try PCA w/ different library

library("FactoMineR")
res.pca <- PCA(NY_PCA, graph = FALSE)

library("factoextra")

print(res.pca)
eig.val <- get_eigenvalue(res.pca)
eig.val

# scree plot 2
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))

var <- get_pca_var(res.pca)
var

# Coordinates
head(var$coord)
# Cos2: quality on the factore map
head(var$cos2)
# Contributions to the principal components
head(var$contrib)

# Coordinates of variables
head(var$coord, 4)

# plot the variables
fviz_pca_var(res.pca, col.var = "black")

# quality of representation 
head(var$cos2, 4)

# Plot cos2 for quality of rep 
library("corrplot")
corrplot(var$cos2, is.corr=FALSE)

# same as bar plot
# Total cos2 of variables on Dim.1 and Dim.2
fviz_cos2(res.pca, choice = "var", axes = 1:2)

# Color by cos2 values: quality on the factor map
fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)

############ ############
########## Modeling NN ################
############ ############

# view dataset
nrow(NY_clean_wPC1)
head(NY_clean_wPC1)
str(NY_clean_wPC1)


########### need alt treat for zip code #####

zip <- table(NY_clean_wPC1$zip_code)

# export to csv for modeling 
write.csv(zip, "data/zip.csv", row.names = FALSE)






# new df for modeling
model_data <- NY_clean_wPC1

#encode zip using label encoding 
table(NY_clean_wPC1$zip_code)
model_data$e_zip <- as.numeric(as.factor(model_data$zip_code))

head(model_data)
View(model_data)

#### Split train vs test ####

library(caret)

# set seed so can reproduce 
set.seed(123)  

# split into 80 train/ 20 test
trainIndex <- createDataPartition(model_data$price, p = 0.8, list = FALSE)

# create training and testing subsets
model_train <- model_data[trainIndex, ]
model_test <- model_data[-trainIndex, ]

# check it 
dim(model_train)
dim(model_test)

######################################


#### Grid search for optimal neural network #### 

library(caret)
library(nnet)
library(doParallel) # for parallel processing (optional param in grid control)

# get original sd and mean for unscaling
model_train_stdev <-sd(model_train$price) # sd = 409233.9
model_train_mean <-mean(model_train$price) # mean = 535181.9

# define and scale data source
## subset data to use PC1 instead of bed, bath, house_size

model1_train <- scale(model_train[, c("price", "pc1", "acre_lot", "e_zip")])

head(model_train)
head(model1_train)

## alternate subset NOT using PC1
model2_train <- scale(model_train[, c("price", "bed", "bath", "house_size", "acre_lot", "e_zip")])


#### TEST SUBSETS 

model1_test <- scale(model_test[, c("price", "pc1", "acre_lot", "e_zip")])

## alternate subset NOT using PC1
model2_test <- scale(model_test[, c("price", "bed", "bath", "house_size", "acre_lot", "e_zip")])


# number of CPU cores to use
## I have 6 cores, setting to 5 so system remains stable
num_cores <- 5  

# Initialize parallel backend
cl <- makeCluster(num_cores)

# Register parallel backend
registerDoParallel(cl)

############

# Define the grid
## dropping maxit since getting error
grid <- expand.grid(size = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14,
                             15,16,17,18,19,20),  # Number of hidden units
                    decay = c(0.001, 0.01, 0.1)) # Weight decay parameter
                    # layer1 = c(1,2), # num hidden layers - doesn't work 
                    # layer2 = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14,
                               # 15,16,17,18,19,20))  # num neurons in 2nd hidden layer - doesn't work 


# Define control parameters
ctrl <- trainControl(method = "cv",  # Cross-validation
                     number = 10,  # Number of folds # started w/ 5, trying 10 
                     verboseIter = TRUE,  # Print progress
                     allowParallel = TRUE  # Allow parallel processing
)


# exe grid search for model w/ PC1

### capture start time 
start_time <- proc.time()

set.seed(123)  # Set seed so can reproduce 
model <- train(price ~ .,  # Target variable ~ (Formula defining the model)
               data = model1_train,  # Your dataset
               method = "nnet",  # Neural network method
               maxit = c(50, 100, 500, 1000), #max iterations 
               trControl = ctrl,  # Control parameters  
               tuneGrid = grid,# Grid of hyperparameters
               )  

### capture end time and display total time
end_time <- proc.time()
time_cost <- end_time - start_time
print(time_cost)

# View results
print(model)



# exe grid search for model WITHOUT PC1

### capture start time 
start_time <- proc.time()

set.seed(123)  # Set seed so can reproduce 
altModel <- train(price ~ .,  # Target variable ~ (Formula defining the model)
               data = model2_train,  # Your dataset
               method = "nnet",  # Neural network method
               maxit = c(50, 100, 500, 1000), #max iterations 
               trControl = ctrl,  # Control parameters  
               tuneGrid = grid,# Grid of hyperparameters
)  

### capture end time and display total time
end_time <- proc.time()
time_cost <- end_time - start_time
print(time_cost)

# View results
print(altModel)




##################### 

#### Build the neural network model

## Build with PC1

### capture start time 
start_time <- proc.time()

set.seed(123)

nn_model1 <- nnet(price ~., data = model1_train, size = 10, decay = 0.1, maxit = 2000)


### capture end time and display total time
end_time <- proc.time()
time_cost <- end_time - start_time
print(time_cost)

 
### subset of output below for nn_model1 ###

# > print(time_cost)
# user  system elapsed 
# 24.88    6.10 2679.02 

# RMSE was used to select the optimal model using the smallest value.
# The final values used for the model were size = 10 and decay = 0.1.
#   size  decay  RMSE       Rsquared   MAE   
#   10    0.100  0.8169222  0.4771568  0.6750866

##########

## Build WITHOUT PC1

### capture start time 
start_time <- proc.time()

set.seed(123)

nn_model2 <- nnet(price ~., data = model2_train, size = 10, decay = 0.1, maxit = 2000)

### capture end time and display total time
end_time <- proc.time()
time_cost <- end_time - start_time
print(time_cost)

### subset of output below for nn_model2 ###

# print(time_cost)
# user  system elapsed 
# 34.11    9.40 2659.39 


#   size  decay  RMSE       Rsquared   MAE  
#   10    0.100  0.8169222  0.4771568  0.6750866


# RMSE was used to select the optimal model using the smallest value.
# The final values used for the model were size = 10 and decay = 0.1.

################### evaluate model performance

####### explore model

library(devtools)
library(nnet)
library(caret)
library(NeuralNetTools)
library(grt)

garson(nn_model1)

nn_model1
summary(nn_model1)
str(nn_model1)
plot(nn_model1) # doesn't work

garson(nn_model1) # wow - zip must be way off... need something different encode for nnet

###
### Model1 training data performance - getting error 


pred_train1 <- predict(nn_model1, model1_train)
rmse <- sqrt(mean((pred_train1 - model1_train$price)^2)) # Error in model1_train$price : $ operator is invalid for atomic vectors
cat("RMSE:", rmse)

### Model1 test data predictions  
predicted_labels <- predict(nn_model1, newdata = model1_test)


########### CRISP-DM REWORK THE STEPS TO IMPROVE ACCURACY 

# data for network modeling
nyx <- NY_clean_wPC1[, c("price", "bed", "bath", "house_size", "acre_lot", "pc1", "city", "zip_code")]
nrow(nyx)
str(nyx$zip_code)

# get household income by zip to sub for zip code 
# since zip is nominal 

library(zipcodeR)

# extract all reverse_zipcode features for unique zip codes
nyx$zip_code <- as.character(nyx$zip_code)
# one zip is less than 5 pos - handle here
nyx$zip_code[nyx$zip_code == "6390"] <- "06390"
zipData <- zipcodeR::reverse_zipcode(unique(nyx$zip_code))
names(zipData)


# rename for join and abbr
names(zipData)[names(zipData) == "zipcode"] <- "zip_code"
names(zipData)[names(zipData) == "median_household_income"] <- "hhi"
names(zipData)

# join realtor and zip to get income, keeping only those where zip matches 
# Inner Join: Keep only rows that have matching values in both tables - expecting small fallout

# 4/8/2024 update join to pull multi features: county, lat & lng, "radius_in_miles", "population", 
# "population_density", "housing_units", "occupied_housing_units", "median_home_value", "hhi"

library(dplyr)
dim(zipData)
View(zipData)
head(zipData)
nyx1 <- inner_join(nyx, zipData[, c(1, 6, 8, 9, 11, 13, 14, 17:20)], by = "zip_code")
nrow(nyx1) # no lost obs from start 340941 
summary(nyx1) # 796 NAs on income

# summarize ZipcodeR for data dictionary 

# libraries
library(dplyr)

# Function to create a data dictionary
create_data_dictionary <- function(df) {
  # Convert raw columns to character
  df <- df %>%
    mutate(across(where(is.raw), as.character)) %>%
    mutate(across(where(is.list), as.character))  # Handle list columns, if any
  
  # Create the data dictionary
  data_dict <- data.frame(
    Attribute = names(df),
    Data_Type = sapply(df, class),  # Get data type
    Unique_Values = sapply(df, function(x) length(unique(na.omit(x)))),  # Unique values, omitting NAs
    Sample_Value = sapply(df, function(x) if (length(x) > 0) sample(na.omit(x), 1) else NA)  # Sample value
  )
  
  # Add data scale description based on data type
  data_dict <- data_dict %>%
    mutate(
      Data_Scale = case_when(
        Data_Type %in% c("integer", "numeric") ~ "Continuous",
        Data_Type == "factor" ~ "Categorical",
        TRUE ~ "Other"
      )
    )
  
  return(data_dict)
}

# Create the data dictionary for zipData
data_dictionary <- create_data_dictionary(zipData)

# Define the output directory and file name
output_directory <- "data"
file_name <- "zipcoder.csv"

# Create the full file path
full_path <- file.path(output_directory, file_name)

# Export the data dictionary to CSV
write.csv(data_dictionary, full_path, row.names = FALSE)

# Print a message to confirm the export
cat("Data dictionary exported to:", full_path, "\n")


###################################
####################################
####################





# drop NA values for hhi
nyx1 <- na.omit(nyx1)
nrow(nyx1) # 340104 so lost the expected low vol = 837
head(nyx1)
summary(nyx1) # confirmed no NA values 

# get distance in miles from 10282 (wealthy zip) to represent location rather than zip 
dist <- zipcodeR::zip_distance("10282", unique(nyx1$zip_code))
head(dist)
dist <- as.data.frame(unique(dist[c(2:3)]))
str(dist)
#rename before merge
names(dist)[names(dist) == "zipcode_b"] <- "zip_code"
# merge via left join 
library(dplyr)
nyx1 <- inner_join(nyx1, dist, by = "zip_code")
nrow(nyx1) # 340104 rows, no loss
summary(nyx1) # confirmed no NA values
View(nyx1)


# OLD DONT USE TO FEED PC1 
head(nyx1)
nyxA <- nyx1[, c("price", "acre_lot", "hhi")]
head(nyxA)
str(nyxA)

# potential join on crime data w/ county.. looks like not all are represented on crime 
#table(nyx1$county)

# An earlier exploratory join with a Zillow/crime crosswalk file was dropped in favor of the NYS crime download below.
library(readr)
library(dplyr)
# only 34 rows - missing stuff go direct

#### download of 2022 NYS crime and import 
library(readr)
crime <- read_csv("data/NYS_Index_Crime_2022.csv")
head(crime)

### Merge prep 

# enriched realtor has county with the word " County" appended to value... crime does not 
## remove " County" from realtor
nyx1$County/Region <- gsub(" County", "", nyx1$county)
head(nyx1)
nrow(nyx1) # 340104 rows 

#rename column before merge
names(crime)[names(crime) == "County/Region"] <- "county2"
head(crime)
nrow(crime) # 74 rows

# merge with inner join 
nyxcrime <- inner_join(nyx1, crime, by = "county2")
nrow(nyxcrime) # 337685 rows (lost 2,419)
head(nyxcrime)

########### data 


# simple data alternative to zip and income using distance used for RFmodel3. RFmodel4
nyx5 <- nyx1[, c("price", "bed", "bath", "house_size", "acre_lot", "distance")]


# data for more robust analysis 
## used for linear lm_model1, random forest RFmodel1, RFmodel2, RFmodel3, and base for neural best_nn3  
nyx3 <- nyx1[, c("price", "bed", "bath", "house_size", "acre_lot", "hhi", "radius_in_miles", "population", "population_density",
                 "housing_units", "occupied_housing_units", "median_home_value", "distance")]


# new data including crime
### pulling in crime rates rather than counts as more meaningful 
nyxc1 <- nyxcrime[, c("price", "bed", "bath", "house_size", "acre_lot", "hhi", "radius_in_miles", "population_density",
                      "median_home_value", "distance", "Index Total Rate", "Violent Total Rate", "Murder Rate", "Rape Rate", 
                      "Robbery Rate", "Agg Assault Rate", "Property Total Rate", "Burglary Rate", "Larceny Rate", "MV Theft Rate")]
head(nyxc1)

# new data trimmed based on variable RF variable importance
## not as accurate 
### pulling in crime rates rather than counts as more meaningful 
nyxc2 <- nyxcrime[, c("price", "bed", "bath", "house_size", "acre_lot", "population_density","median_home_value",
                      "distance", "Index Total Rate")]
head(nyxc2)

# swap spaces with underscore in crime data column names
names(nyxc1) <- gsub("\\s+", "_", names(nyxc1))
head(nyxc1)

summary(nyxc1)

################### Linear Regression 


### scale all except response variable 

library(dplyr)

linearData <- nyxc1

target_var <- "price"
target <- linearData[[target_var]]
predictor_var <- select(linearData, -target_var)
scaled_predictor_var <- as.data.frame(scale(predictor_var))
ny_scaled <- cbind(target, scaled_predictor_var)


# split training and testing 

set.seed(92118)

training.prop2  <- 0.70

training.cases2 <- sample(nrow(ny_scaled), nrow(ny_scaled)*training.prop2)

sub.training2 <- ny_scaled[training.cases2, ]
nrow(sub.training2)

sub.testing2  <- ny_scaled[-training.cases2, ]
nrow(sub.testing2)

head(sub.training2)
head(sub.testing2)



lm_model1 <- lm(target ~ ., data = sub.training2)

summary(lm_model1)

# predict w/ test data
lm_predict <- predict(lm_model1, newdata = sub.testing2)

# eval perf
# Mean Absolute Error (MAE)
mae <- mean(abs(sub.testing2$target - lm_predict))

# Mean Squared Error (MSE)
mse <- mean((sub.testing2$target - lm_predict)^2)

# Root Mean Squared Error (RMSE)
rmse <- sqrt(mse)

# R-squared (R²)
rsquared <- summary(lm_model1)$r.squared 

# Print or output the performance metrics
print(paste("Mean Absolute Error (MAE):", mae))
print(paste("Mean Squared Error (MSE):", mse))
print(paste("Root Mean Squared Error (RMSE):", rmse))
print(paste("R-squared (R²):", rsquared))


####################   Random Forest


RFdata <- nyxc1

# split training and testing 

set.seed(92118)

training.prop  <- 0.70

training.cases <- sample(nrow(RFdata), nrow(RFdata)*training.prop)

sub.training <- RFdata[training.cases, ]

sub.testing  <- RFdata[-training.cases, ]


### build RF - taking too long - moving to H2O at bottom 

library(randomForest)

# Very long runtime - canceled and will try parallel + msg
library(textme)
library(foreach)
library(doParallel)

# Choose n cores to use
num_cores <- 4  # 

# Register parallel backend using doParallel
cl <- makeCluster(num_cores)
registerDoParallel(cl)

start_time <- proc.time()

# model w/ parallel 
model <- foreach::foreach(ntree = c(150), .packages = 'randomForest') %dopar% {
  randomForest(price ~ ., data = sub.training, ntree = ntree)
}

### capture end time and display total time model 2
end_time <- proc.time()
time_cost <- end_time - start_time
print(time_cost)

textme(message = "👹 Back to work Forest is done!")     # text me when it's done!

stopCluster(cl)





###########

library(randomForest)

RFmodel4 <- model # simpified just adding distance with ntree 150 
RFmodel4

RFmodel3 <- model # model w/ c(50, 100, 150) no scaling
summary(RFmodel3)
RFmodel3[[1]]
RFmodel3[[2]]
RFmodel3[[3]]

RFmodel2 <- RFmodel1 # model w/ (5,10) no scaling

RFmodel1 <- model # model w/ (15) no scaling
summary(RFmodel1) # not useful

# predictions
#RFpredictions <- predict(RFmodel3[[3]], newdata = sub.testing)
# class(RFpredictions)

# simple predictions
RFpredictions <- predict(RFmodel4[[1]], newdata = sub.testing)



# View the predictions
print(RFpredictions)

# Check data types
class(RFpredictions)
class(sub.testing$price)

RFpredictions <- as.numeric(RFpredictions)

#get performance 
library(caret)
# Mean Absolute Error (MAE)
mae <- mean(abs(RFpredictions - sub.testing$price))
print(paste("Mean Absolute Error (MAE):", mae))

# Mean Squared Error (MSE)
mse <- mean((RFpredictions - sub.testing$price)^2)
print(paste("Mean Squared Error (MSE):", mse))

# Root Mean Squared Error (RMSE)
rmse <- sqrt(mse)
print(paste("Root Mean Squared Error (RMSE):", rmse))

# R2
r_squared <- cor(RFpredictions, sub.testing$price)^2
print(paste("R-squared (R2):", r_squared))


############ ############
########## REDO PCA ################
############ ############

library(dplyr)
library(tidyverse) 
library(skimr)
library(ggplot2)

# select fields for redo PCA
# note - excluding the target variable from PCA... just looking at predictors

# set datasource here to allow quick swap
PCA_data <- nyxc1

# exclude target variable for PCA
PCA_data <- PCA_data[, !names(PCA_data) %in% c("price")]
# check it
head(PCA_data)

# scale the data since they are different measures 
NY_PCA2 <- scale(PCA_data)

# supply subset from NY_PCA
NY_Pr2 <- prcomp(NY_PCA2)

summary(NY_Pr2)
str(NY_Pr2)

# view eigenvalues 
NY_Pr2$x

# scree plot 
plot(NY_Pr2, type = "l", main = "Scree Plot") # dont rerun plots - expensive

# bi plot 
# biplot(NY_Pr2, scale = 0) # dont rerun plots - expensive


# add PC1 & 2 onto data and plot 
NY_Pr_Bind2 <- cbind(NY_PCA2, NY_Pr2$x[,1:3])
# view
head(NY_Pr_Bind2)


#### PCA w/ different library -- much better info and visuals 

library("FactoMineR")
res.pca2 <- PCA(NY_PCA2, graph = FALSE)

library("factoextra")

print(res.pca2)
eig.val <- get_eigenvalue(res.pca2)
eig.val

# scree plot 2
fviz_eig(res.pca2, addlabels = TRUE, ylim = c(0, 50)) # dont rerun plots - expensive

var2 <- get_pca_var(res.pca2)
var2

# Coordinates
head(var2$coord)
# Cos2: quality on the factore map
head(var2$cos2)
# Contributions to the principal components
head(var2$contrib)

# Coordinates of variables 
## note Using the Eigenvalue-one Criterion, we select all the PCs that 
# their Eigenvalues are greater than 1 (in this case, just PC1).
# From those selected PCs, we select the variables that the magnitude 
# of the component loading is at least 0.5 (in this case, bed, bath, house_size)
head(var2$coord, 4)

# plot the variables
fviz_pca_var(res.pca2, col.var = "black") # dont rerun plots - expensive

# quality of representation 
head(var2$cos2, 10)

# Plot cos2 for quality of rep 
library("corrplot")
corrplot(var2$cos2, is.corr=FALSE) # dont rerun plots - expensive

# same as bar plot
# Total cos2 of variables on Dim.1 and Dim.2
fviz_cos2(res.pca2, choice = "var", axes = 1:4) # dont rerun plots - expensive

# Color by cos2 values: quality on the factor map # dont rerun plots - expensive
fviz_pca_var(res.pca2, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)

#### Try H2o PCA


library(h2o)
#h2o.init() # defaults 
h2o.init(nthreads = 10, max_mem_size = "40g") #specify max
library(textme)
# h2o.shutdown()

pca_result <- prcomp(PCA_data, scale. = FALSE)

# Get component loadings
loadings <- pca_result$rotation

# Print component loadings
print(loadings)


######## for modeling, add PC1 to original file

# new dataframe to combine source with new PC1
# includes city and zip
nyxx <- nyx1[-6]
head(nyxx)
summary(nyxx)

# get sd and means for unscaling
library(matrixStats)
means <- colMeans(nyxx[, -c(6:7)])
sds <- colSds(as.matrix(nyxx[, -c(6:7)]))

# Print the means and standard deviations
print("Means:")
print(means)
print("Standard deviations:")
print(sds)

# drop city and zip_code
nyxxx <- nyxx[, -c(6:7)]
summary(nyxxx)

# Separate predictors from target
predictors <- nyxxx[, -which(names(nyxxx) == "price")]

# Scale predictors
scaled_predictors <- scale(predictors)

# Combine scaled predictors w target
nyx_scaled <- cbind(price = nyxxx$price, scaled_predictors, pc1 = NY_Pr2$x[,1])
head(nyx_scaled)

# check class
class(nyx_scaled)
nyx_scaled <- as.data.frame(nyx_scaled)
class(nyx_scaled)

#### define two different datasets for modeling

# Data using PC1 in model 
nyxA <- nyx_scaled[, c("price", "acre_lot", "pc1", "hhi")]
head(nyxA)
str(nyxA)
class(nyxA)

# Alternative data NOT using PC1 
nyxB <- nyx_scaled[, c("price", "bed", "bath", "house_size", "acre_lot", "hhi")]
head(nyxB)
class(nyxB)

########## BUILD MODELS ###########



# # !!!!!!!!!!!! TAKING FOREVER - TRY TEXT INTEGRATION

## FROM https://github.com/richpauloo/textme

devtools::install_github("richpauloo/textme")

library(textme)

# text values - redacted
tm_configure(twilio_sid   = Sys.getenv("TWILIO_SID"),
             twilio_token = Sys.getenv("TWILIO_TOKEN"),
             twilio_phone_number = Sys.getenv("TWILIO_FROM_NUMBER"),
             target_phone_number = Sys.getenv("TWILIO_TO_NUMBER"))


######### REALLY LONG - NEED PARALLEL PROCESSING ####

#### too long - can't use this didn't finish model 1 after 12055!

#### deleting all the lines around NNET and NeuralNet attempts but below are some of the long runtimes

#> time_cost <- end_time - start_time
#> print(time_cost)
#user   system  elapsed 
#10283.31   715.82 12055.02 

# try again - no joy 
#> time_cost <- end_time - start_time
#> print(time_cost)
#user   system  elapsed 
#16874.35  1317.07 24476.84 


####################
############# TRYING H20 FOR MODEL

# source = https://docs.h2o.ai/h2o/latest-stable/h2o-docs/downloading.html#install-in-r


if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }

# download packages that H2O depends on
pkgs <- c("RCurl","jsonlite")
for (pkg in pkgs) {
  if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
}

# install the H2O package for R
install.packages("h2o", type="source", repos=(c("http://h2o-release.s3.amazonaws.com/h2o/latest_stable_R")))


############################# H20 WORK

library(h2o)
h2o.init()
library(textme)


### DATA 
# predictors scaled, target is NOT scaled

# model1 will use nyxA
# model2 will us nyxB

# Define data source up front so can switch to alt input easily 

# new data w/ more features and no PCA
modelData <- nyx3

# set as h2o frame for use w/ h20 package 
modelData <- as.h2o(modelData)
head(modelData)


######

# set the predictor names and the response column name
predictors <- colnames(modelData)[2:4]
head(predictors)

# set the response column 
response <- colnames(modelData)[1]

# split into train and test sets
modelData_splits <- h2o.splitFrame(data = modelData, ratios = 0.7, seed = 1234)
train <- modelData_splits[[1]]
test <- modelData_splits[[2]]

######## H20 BEST MODEL 

# define hyperparam

hyper_params <- list(
  activation = c("Rectifier","Tanh", "Maxout"),
  hidden = list(c(3, 1), c(3), c(7), c(11), c(15)),
  epochs = c(50),
  stopping_rounds = 5,      # Stop if validation error does not improve for 5 consecutive iterations
  stopping_tolerance = 0.001,  # Relative tolerance for improvement
  stopping_metric = "RMSE"      # Metric used for stopping criteria
)

# search criteria
search_criteria <- list(strategy = "RandomDiscrete", 
                        max_models = 10
)


#######################
#ititialize 
rm(nn_grid)

# run grid search

start_time <- proc.time()

nn_grid2 <- h2o.grid(
  algorithm = "deeplearning",
  grid_id = "nn_grid",
  x = predictors,  
  y = response,    
  training_frame = train,
  validation_frame = test,
  search_criteria = search_criteria,
  hyper_params = hyper_params,      # Specify the hyperparameter grid
  nfolds = 5
)

### capture end time and display total time model 2
end_time <- proc.time()
time_cost <- end_time - start_time
print(time_cost)

textme(message = "👹 Back to work neural grid search w/ new features is done!")     # text me when it's done!

#######################

# get all runs
summary(nn_grid2, show_stack_traces = TRUE)

# get best model 1
best_nn3 <- h2o.getModel(nn_grid2@model_ids[[1]])
best_nn3
summary(best_nn3)

# get best model 1a (informed by 1st run)
best_nn1a <- h2o.getModel(nn_grid@model_ids[[1]])
best_nn1a
summary(best_nn1a)


# get best model 2
best_nn2 <- h2o.getModel(nn_grid@model_ids[[1]])
best_nn2
summary(best_nn2)



# use for prediction on model 1a
predictions <- h2o.predict(best_nn1a, newdata = test)
predictions

# use for prediction on model 2
predictions2 <- h2o.predict(best_nn2, newdata = test)
predictions2

# use for prediction on model 3 w/ new features
predictions3 <- h2o.predict(best_nn3, newdata = test)
predictions3

# retrieve the model performance model 3
perf <- h2o.performance(best_nn3, test)
perf


# retrieve the model performance model 1a
perf <- h2o.performance(best_nn1a, test)
perf

# retrieve the model performance model 2
perf2 <- h2o.performance(best_nn2, test)
perf2


# retrieve the r2 value:
r2_train <- h2o.r2(best_nn, valid = FALSE)
r2_train

# retrieve the r2 value for the test data:
r2_test <- h2o.r2(predictions)
r2_test


# S4 method for H2OModel
## doesn't work for regressor
h2o.confusionMatrix(best_nn3, test, valid = FALSE, xval = FALSE)


# Extract observed and predicted values from model 1a (test data)
observed1a <- as.vector(as.list(test[[response]]))
predicted1a <- as.vector(as.list(predictions$predict))


# Create a scatter plot
plot(observed1a, predicted1a, main = "Observed vs. Predicted", sub = "Model 1 with CP1", xlab = "Observed", ylab = "Predicted")

# Add a reference line
abline(a = 0, b = 1, col = "red")

# Shut down the H2O cluster
h2o.shutdown()


##################################
########## H20 Random Forest
##################################


library(h2o)
#h2o.init() # defaults 
h2o.init(nthreads = 10, max_mem_size = "35g") #specify max
# h2o.shutdown()

#check ds
head(nyxc1)
summary(nyxc1)

# Define data source once up front so can switch to alt input easily 
## make H2o

H2oRFdata <- as.h2o(nyxc1)
head(H2oRFdata)

######

# set the predictor names and the response column name
predictors <- colnames(H2oRFdata)[2:ncol(H2oRFdata)]
head(predictors)

# set the response column 
response <- colnames(H2oRFdata)[1]

# split into train and test sets
H2oRFdata_splits <- h2o.splitFrame(data = H2oRFdata, ratios = 0.7, seed = 1234)
train <- H2oRFdata_splits[[1]]
test <- H2oRFdata_splits[[2]]
nrow(train)
nrow (test)

######## Just RF (no grid)

library(textme)

# capture start time
start_time <- proc.time()

# build RF

# cannot run 5K trees exceeds mem. ~max 2K

h20_forest <- h2o.randomForest(x = predictors, 
                               y = response, 
                               training_frame = train,
                               validation_frame = test,
                               nfolds = 5,
                               ntrees = 500,
                               max_depth = 30,
                               seed = 1234,
)

#store model
h20_forest2 <- h20_forest # ntree = 500, fold = 5, max depth = 30

### capture end time and display total time model 2
end_time <- proc.time()
time_cost <- end_time - start_time
print(time_cost)


textme(message = "👹 H2o forest done!")     # text me when it's done

# model2 run time ~ 25min for ntree = 500 w/ 5 k-fold cv max depth
#user  system elapsed 
# 0.76    0.38 1475.55 

# store models 
## comment out so dont accidentally overwrite
## go w/ model 2 since it's nearly best accuracy and much faster than m3

# h20_forest1 <- h20_forest # ntree = 150, fold = 5, max depth = 20
# h20_forest2 <- h20_forest # ntree = 500, fold = 5, max depth = 30
# h20_forest3 <- h20_forest # ntree = 2000, fold = 5, max depth = 30
# h20_forest4 <- h20_forest # ntree = 500, fold = 5, max depth = 30

dim(h20_forest2)

# predict
h20_forest_predict <- h2o.predict(h20_forest2, newdata = test)

# performance
h20_forest_perf <- h2o.performance(h20_forest2, newdata = test)

# feature importance
h20_forest_var_imp <- h2o.varimp(h20_forest2)
print(h20_forest_var_imp)

#plot importance
h2o.varimp_plot(h20_forest2)

# feature interaction 
poh2o.interaction(object = h20_forest3, data = H2oRFdata, 
                  cols = c("price", "bed", "bath", "house_size", "acre_lot")) 



##########

# extract predictions as df
h20_forest_predictions <- as.data.frame(h2o.predict(h20_forest2, newdata = test)$predict)

# test data as df

# df for plot
RF_plot_data <- cbind(as.data.frame(test$price), as.data.frame(h2o.predict(h20_forest2, newdata = test)$predict))

head(RF_plot_data)
str(RF_plot_data)

# scatterplot
library(ggplot2)
library(scales)
ggplot(RF_plot_data, aes(x = price, y = predict)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") + # Add a line of equality
  labs(x = "Actual Price", y = "Predicted Price", title = "Random Forest Results") +
  scale_x_continuous(labels = comma_format(big.mark = ","), name = "Actual Price") +
  scale_y_continuous(labels = comma_format(big.mark = ","), name = "Predicted Price") +
  theme_minimal()


#########

# Get predictions for full set
# predict
h20_forest_predict <- h2o.predict(h20_forest2, newdata = H2oRFdata)

# extract predictions as df
h20_forest_predict_full <- as.data.frame(h2o.predict(h20_forest2, newdata = H2oRFdata)$predict)

# add predictions to end of ds
RF_full <- cbind(as.data.frame(H2oRFdata), as.data.frame(h2o.predict(h20_forest2, newdata = H2oRFdata)$predict))

RF_full <- as.data.frame(RF_full)
dim(RF_full)
head(RF_full)

# export full to csv to explore in tableau
write.csv(RF_full, "data/RF_full.csv", row.names = FALSE)

## NEED TO ADD THE OTHER FIELDS LIKE ZIP AND COUNTY FOR TABLEAU EXPLORE 



# add predictions to end of ds
RF_all <- cbind(nyxcrime, RF_full$predict)
dim(RF_all)
head(RF_all)

# export full to csv to explore in tableau
write.csv(RF_all, "data/RF_all.csv", row.names = FALSE)


#######################

# Shut down the H2O cluster
h2o.shutdown()




