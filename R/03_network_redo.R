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
# View(zipData)
head(zipData)
nyx1 <- inner_join(nyx, zipData[, c(1, 6, 8, 9, 11, 13, 14, 17:20)], by = "zip_code")
nrow(nyx1) # no lost obs from start 340941 
summary(nyx1) # 796 NAs on income


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
