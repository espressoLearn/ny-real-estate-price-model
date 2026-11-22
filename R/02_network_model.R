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

library(dplyr)
View(zipData)
nyx1 <- inner_join(nyx, zipData[, c(1,20)], by = "zip_code")
nrow(nyx1) # no lost obs from start 340941 
summary(nyx1) # 796 NAs on income

# drop NA values for hhi
nyx1 <- na.omit(nyx1)
nrow(nyx1) # 340145 so lost the expected 796
summary(nyx1) # confirmed no NA values 


# select fields for 1st model using PC1
head(nyx1)
nyxA <- nyx1[, c("price", "acre_lot", "pc1", "hhi")]
head(nyxA)
str(nyxA)



############ ############
########## REDO PCA ################
############ ############

library(dplyr)
library(tidyverse) 
library(skimr)
library(ggplot2)

# select fields for redo PCA
# note - excluding the target variable from PCA... just looking at predictors
nyx2 <- nyx1[, c("bed", "bath", "house_size", "acre_lot", "hhi")]
summary(nyx2)

# scale the data since they are different measures 
NY_PCA2 <- scale(nyx2)

# supply subset from NY_PCA
NY_Pr2 <- prcomp(NY_PCA2)

summary(NY_Pr2)
str(NY_Pr2)

# view eigenvalues 
NY_Pr2$x

# scree plot 
# plot(NY_Pr2, type = "l", main = "Scree Plot") # dont rerun plots - expensive

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
# fviz_eig(res.pca2, addlabels = TRUE, ylim = c(0, 50)) # dont rerun plots - expensive

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
# fviz_pca_var(res.pca2, col.var = "black") # dont rerun plots - expensive

# quality of representation 
head(var2$cos2, 4)

# Plot cos2 for quality of rep 
library("corrplot")
# corrplot(var2$cos2, is.corr=FALSE) # dont rerun plots - expensive

# same as bar plot
# Total cos2 of variables on Dim.1 and Dim.2
# fviz_cos2(res.pca2, choice = "var", axes = 1:2) # dont rerun plots - expensive

# Color by cos2 values: quality on the factor map # dont rerun plots - expensive
# fviz_pca_var(res.pca2, col.var = "cos2",
#             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
#             repel = TRUE # Avoid text overlapping
#)


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

# Separate predictor variables from the target variable
predictors <- nyxxx[, -which(names(nyxxx) == "price")]

# Scale only the predictor variables
scaled_predictors <- scale(predictors)

# Combine scaled predictor variables with the target variable
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

# replace the values here with your own
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
modelData <- nyxA

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
  epochs = c(500),
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

nn_grid <- h2o.grid(
  algorithm = "deeplearning",
  grid_id = "nn_grid",
  x = predictors,  # Assuming 'predictors' are your input features
  y = response,    # Assuming 'response' is your target variable
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

textme(message = "👹 Back to work model 1 is done!")     # text me when it's done!

#######################

# get all runs
summary(nn_grid, show_stack_traces = TRUE)

# get best model 1
best_nn1 <- h2o.getModel(nn_grid@model_ids[[1]])
best_nn1
summary(best_nn1)

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
h2o.confusionMatrix(best_nn, test, valid = FALSE, xval = FALSE)


# Extract observed and predicted values from model 1a (test data)
observed1a <- as.vector(as.list(test[[response]]))
predicted1a <- as.vector(as.list(predictions$predict))


# Create a scatter plot
plot(observed1a, predicted1a, main = "Observed vs. Predicted", sub = "Model 1 with CP1", xlab = "Observed", ylab = "Predicted")

# Add a reference line
abline(a = 0, b = 1, col = "red")

# Shut down the H2O cluster
h2o.shutdown()









