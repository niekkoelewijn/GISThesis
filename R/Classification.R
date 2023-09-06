# Course: GRS-80436 / MSc Thesis
# Project: AHN canopy gap detection
# Author: Niek Koelewijn
# Organisation: Wageningen University
# Supervisor: L.C. Vreugdenhil MSc.
# Year: 2022

### Description ###
# In this script, two random forest models will be trained. The first will make 
# a classification of canopy gaps in three categories: 'part of tree', 'one tree' 
# and 'group of trees'. The second model will make a classification of canopy 
# gaps in two categories: managed and unmanaged. The random forest model will 
# list the variables that are most important for the classification.



### RF model with tree classes: part of tree, one tree and group of trees 


## Load data into environment

# Load the canopy gaps with all attributes
metrics_canopy_gaps_raw <- st_read("~/ahncanopygaps/InputData/new_gaps_all_metrics.gpkg")

# Load the canopy gaps with classes
classified_canopy_gaps_raw <- st_read("~/ahncanopygaps/InputData/new_gaps_polygons_classified.gpkg")
classified_canopy_gaps <- classified_canopy_gaps_raw %>% 
  dplyr::select(ID, NewClassMerge, geom) %>% 
  dplyr::rename(class = NewClassMerge) %>% 
  sf::st_drop_geometry() %>% 
  tibble()

# Replace ID 
correct_ID <- st_read("~/ahncanopygaps/InputData/test/is_this_correct.gpkg")
classified_canopy_gaps$ID <- correct_ID$ID

# Convert class column to factor
classified_canopy_gaps$class <- as.factor(classified_canopy_gaps$class)

# Join metrics_canopy_gaps with classified_canopy_gaps
metrics_canopy_gaps <- metrics_canopy_gaps_raw %>% 
  dplyr::left_join(classified_canopy_gaps, by = "ID") %>% 
  sf::st_drop_geometry() %>% 
  tibble()
  
# Replace NA in ForestPlot_DI and ForestPlot_CE
metrics_canopy_gaps_na_replaced <- metrics_canopy_gaps %>% 
  tidyr::replace_na(list(ForestPlot_DI = 0, ForestPlot_CE = 0))


## Prepare data for classifcation

# Set seed for reproducibility
set.seed(123)

# Define repeated cross validation with 5 folds and three repeats
repeat_cv <- caret::trainControl(method='repeatedcv', number=5, repeats=3)

# Split the data so that we use 70% of it for training
train_index <- caret::createDataPartition(y=metrics_canopy_gaps_na_replaced$class, p=0.7, list=FALSE)

# Subset the data
train <- metrics_canopy_gaps_na_replaced[train_index, ] %>% 
  dplyr::select(-ID, -zentropy_AHN3, -zentropy_AHN4, 
                -Buffer5m_zentropy_AHN3, -Buffer5m_zentropy_AHN4,
                -Buffer10m_zentropy_AHN3, -Buffer10m_zentropy_AHN4,
                -ForestPlot_fraq_LC)
  
  
test <- metrics_canopy_gaps_na_replaced[-train_index, ] %>% 
  dplyr::select(-ID, -zentropy_AHN3, -zentropy_AHN4, 
                -Buffer5m_zentropy_AHN3, -Buffer5m_zentropy_AHN4,
                -Buffer10m_zentropy_AHN3, -Buffer10m_zentropy_AHN4,
                -ForestPlot_fraq_LC)


## Random forest classification


# Train model with quick function
forest_ntree_short = randomForest(x = train[-ncol(train)],
                                  y = train$class,
                                  ntree = 500)

# Train a random forest model
forest_ntree <- caret::train(
  
  # Formula. We are using all variables to predict class
  class~., 
  
  # Source of data; Remove the class variable
  data=train, 
  
  # `rf` method for random forest
  method='rf', 
  
  # Add repeated cross validation as trControl
  trControl=repeat_cv,
  
  # Accuracy to measure the performance of the model
  metric='Accuracy')

# Check result
forest_ntree$finalModel

# Get variable importance
var_imp_ntree <- varImp(forest_ntree, scale=FALSE)$importance
var_imp_ntree <- data.frame(variables=row.names(var_imp_ntree), importance=var_imp_ntree$Overall)

# Create a plot of variable importance
var_imp_ntree %>%
  
  ## Sort the data by importance
  arrange(importance) %>%
  
  ## Select most important variables
  dplyr::filter(importance > 8) %>% 
  
  ## Create a ggplot object for aesthetic
  ggplot(aes(x=reorder(variables, importance), y=importance)) + 
  
  ## Plot the bar graph
  geom_bar(stat='identity') + 
  
  ## Flip the graph to make a horizontal bar plot
  coord_flip() + 
  
  ## Add x-axis label
  xlab('Variables') +
  
  ## Add a title
  labs(title='RF model "number of trees" variable importance') + 
  
  ## Some layout for the plot
  theme_minimal() + 
  theme(axis.text = element_text(size = 10), 
        axis.title = element_text(size = 15), 
        plot.title = element_text(size = 15), 
  )


## Check performance on test data

# Generate predictions
y_hats_ntree <- predict(
  
  ## Random forest object
  object=forest_ntree_short, 
  
  ## Data to use for predictions; remove the class
  newdata=test[-ncol(test)])

# Print the accuracy
accuracy_ntree <- mean(y_hats_ntree == test$class)*100
cat('Accuracy on testing data: ', round(accuracy_ntree, 2), '%',  sep='') # Accuracy on testing data: 85.56%

# Save model
save(forest_ntree, file = "~/ahncanopygaps/InputData/RandomForestModels/forest_ntree.RData")


## Random forest classification with most important variables of ntree

# prepare data 
train_top4 <- train %>% 
  dplyr::select(class, shape_AHN3_planarity, ForestPlot_fraq_NGCM,
                ForestPlot_GD)

test_top4 <- test %>% 
  dplyr::select(class, shape_AHN3_planarity, ForestPlot_fraq_NGCM,
                ForestPlot_GD)

# Check colinearlity
corrplot(cor(train_top4[-1]), method = "number")

# Train model with quick function
forest_ntree_top4_short = randomForest(x = train_top4[-1],
                                       y = train_top4$class,
                                       ntree = 500)

# Train a random forest model
forest_ntree_top4 <- caret::train(
  
  # Formula. We are using all variables to predict class
  class~., 
  
  # Source of data; Remove the class variable
  data=train_top4, 
  
  # `rf` method for random forest
  method='rf', 
  
  # Add repeated cross validation as trControl
  trControl=repeat_cv,
  
  # Accuracy to measure the performance of the model
  metric='Accuracy')

# Check result
forest_ntree_top4$finalModel

# Get variable importance
var_imp_ntree_top4 <- varImp(forest_ntree_top4, scale=FALSE)$importance
var_imp_ntree_top4 <- data.frame(variables=row.names(var_imp_ntree_top4), importance=var_imp_ntree_top4$Overall)

# Create a plot of variable importance
var_imp_ntree_top4 %>%
  
  ## Sort the data by importance
  arrange(importance) %>%
  
  ## Create a ggplot object for aesthetic
  ggplot(aes(x=reorder(variables, importance), y=importance)) + 
  
  ## Plot the bar graph
  geom_bar(stat='identity') + 
  
  ## Flip the graph to make a horizontal bar plot
  coord_flip() + 
  
  ## Add x-axis label
  xlab('Variables') +
  
  ## Add a title
  labs(title='Random forest variable importance') + 
  
  ## Some layout for the plot
  theme_minimal() + 
  theme(axis.text = element_text(size = 10), 
        axis.title = element_text(size = 10), 
        plot.title = element_text(size = 15), 
  )


## Check performance on test data

# Generate predictions
y_hats_ntree_top4 <- predict(
  
  ## Random forest object
  object=forest_ntree_top4, 
  
  ## Data to use for predictions; remove the class
  newdata=test_top4[-1])

# Print the accuracy
accuracy_ntree_top4 <- mean(y_hats_ntree_top4 == test_top4$class)*100
cat('Accuracy on testing data: ', round(accuracy_ntree_top4, 2), '%',  sep='') # Accuracy on testing data: 77.19%

# Save model
save(forest_ntree_top4, file = "~/ahncanopygaps/InputData/RandomForestModels/forest_ntree_top4.RData")


## Random forest classification with most important variables of ntree without
## the forestplot variables

# prepare data 
train_top4_non_forestplot <- train %>% 
  dplyr::select(class, shape_AHN3_planarity,
                shape_AHN3_linearity, shape_AHN3_eigen_medium,
                ipcumzq90_AHN3)

test_top4_non_forestplot <- test %>% 
  dplyr::select(class, shape_AHN3_planarity,
                shape_AHN3_linearity, shape_AHN3_eigen_medium,
                ipcumzq90_AHN3)

# Check colinearlity
corrplot(cor(train_top4_non_forestplot[-1]), method = "number")

# Train model with quick function
forest_ntree_top4_non_forestplot_short = randomForest(x = train_top4_non_forestplot[-1],
                                                      y = train_top4_non_forestplot$class,
                                                      ntree = 500)

# Train a random forest model
forest_ntree_top4_non_forestplot <- caret::train(
  
  # Formula. We are using all variables to predict class
  class~., 
  
  # Source of data; Remove the class variable
  data=train_top4_non_forestplot, 
  
  # `rf` method for random forest
  method='rf', 
  
  # Add repeated cross validation as trControl
  trControl=repeat_cv,
  
  # Accuracy to measure the performance of the model
  metric='Accuracy')

# Check result
forest_ntree_top4_non_forestplot$finalModel

# Get variable importance
var_imp_ntree_top4_non_forestplot <- varImp(forest_ntree_top4_non_forestplot, scale=FALSE)$importance
var_imp_ntree_top4_non_forestplot <- data.frame(variables=row.names(var_imp_ntree_top4_non_forestplot), importance=var_imp_ntree_top4_non_forestplot$Overall)

# Create a plot of variable importance
var_imp_ntree_top4_non_forestplot %>%
  
  ## Sort the data by importance
  arrange(importance) %>%
  
  ## Create a ggplot object for aesthetic
  ggplot(aes(x=reorder(variables, importance), y=importance)) + 
  
  ## Plot the bar graph
  geom_bar(stat='identity') + 
  
  ## Flip the graph to make a horizontal bar plot
  coord_flip() + 
  
  ## Add x-axis label
  xlab('Variables') +
  
  ## Add a title
  labs(title='Random forest variable importance') + 
  
  ## Some layout for the plot
  theme_minimal() + 
  theme(axis.text = element_text(size = 10), 
        axis.title = element_text(size = 10), 
        plot.title = element_text(size = 15), 
  )


## Check performance on test data

# Generate predictions
y_hats_ntree_top4_non_forestplot <- predict(
  
  ## Random forest object
  object=forest_ntree_top4_non_forestplot, 
  
  ## Data to use for predictions; remove the class
  newdata=test_top4_non_forestplot)

# Print the accuracy
accuracy_ntree_top4_non_forestplot <- mean(y_hats_ntree_top4_non_forestplot == test_top4_non_forestplot$class)*100
cat('Accuracy on testing data: ', round(accuracy_ntree_top4_non_forestplot, 2), '%',  sep='') # Accuracy on testing data: 74.76%

# Save model
save(forest_ntree_top4_non_forestplot, file = "~/ahncanopygaps/InputData/RandomForestModels/forest_ntree_top4_non_forestplot.RData")


### RF model with two classes: managed and unmanaged


## Load the data into environment
# The variable new_gaps_plot_code_class is created in Histograms.R


## Prepare data for classifcation

# Set seed for reproducibility
set.seed(123)

# Define repeated cross validation with 5 folds and three repeats
repeat_cv_managed <- caret::trainControl(method='repeatedcv', number=5, repeats=3)

# Split the data so that we use 70% of it for training
train_index_managed <- caret::createDataPartition(y=new_gaps_plot_code_class$managed_class, p=0.7, list=FALSE)

# Subset the data
train_managed <- new_gaps_plot_code_class[train_index, ] %>% 
  sf::st_drop_geometry() %>% 
  tibble() %>% 
  dplyr::select(-ID, -geom, -zentropy_AHN3, -zentropy_AHN4, 
                -Buffer5m_zentropy_AHN3, -Buffer5m_zentropy_AHN4,
                -Buffer10m_zentropy_AHN3, -Buffer10m_zentropy_AHN4,
                -ForestPlot_DI, -ForestPlot_CE, -ForestPlot_fraq_LC)

test_managed <- new_gaps_plot_code_class[-train_index, ] %>% 
  sf::st_drop_geometry() %>% 
  tibble() %>% 
  dplyr::select(-ID, -geom, -zentropy_AHN3, -zentropy_AHN4, 
                -Buffer5m_zentropy_AHN3, -Buffer5m_zentropy_AHN4,
                -Buffer10m_zentropy_AHN3, -Buffer10m_zentropy_AHN4,
                -ForestPlot_DI, -ForestPlot_CE, -ForestPlot_fraq_LC)


## Random forest classification

# Train model with quick function
forest_managed_short = randomForest(x = train_managed[-1],
                                    y = train_managed$managed_class,
                                    ntree = 500)

# Train a random forest model
forest_managed <- caret::train(
  
  # Formula. We are using all variables to predict class
  managed_class~., 
  
  # Source of data; Remove the class variable
  data=train_managed, 
  
  # `rf` method for random forest
  method='rf', 
  
  # Add repeated cross validation as trControl
  trControl=repeat_cv_managed,
  
  # Accuracy to measure the performance of the model
  metric='Accuracy')

# Check result
forest_managed$finalModel

# Get variable importance
var_imp_managed <- varImp(forest_managed, scale=FALSE)$importance
var_imp_managed <- data.frame(variables=row.names(var_imp_managed), importance=var_imp_managed$Overall)

# Create a plot of variable importance
var_imp_managed %>%
  
  ## Sort the data by importance
  arrange(importance) %>%
  
  ## Select most important 20 rows
  dplyr::filter(importance > 1.32882685) %>% 
  
  ## Create a ggplot object for aesthetic
  ggplot(aes(x=reorder(variables, importance), y=importance)) + 
  
  ## Plot the bar graph
  geom_bar(stat='identity') + 
  
  ## Flip the graph to make a horizontal bar plot
  coord_flip() + 
  
  ## Add x-axis label
  xlab('Variables') +
  
  ## Add a title
  labs(title='Random forest variable importance') + 
  
  ## Some layout for the plot
  theme_minimal() + 
  theme(axis.text = element_text(size = 10), 
        axis.title = element_text(size = 15), 
        plot.title = element_text(size = 20), 
  )

# Generate predictions
y_hats_managed <- predict(
  
  ## Random forest object
  object=forest_managed, 
  
  ## Data to use for predictions; remove the class
  newdata=test_managed)

# Print the accuracy
accuracy_managed <- mean(y_hats_managed == test_managed$managed_class)*100
cat('Accuracy on testing data: ', round(accuracy_managed, 2), '%',  sep='') # Accuracy on testing data: 99.46%

# Save model
save(forest_managed, file = "~/ahncanopygaps/InputData/RandomForestModels/forest_managed.RData")



### RF model with three classes: managed, unmanaged and pseudo_unmanaged


## Load the data into environment
# The variable new_gaps_plot_code_three_management_classes is created in Histograms.R


## Prepare data for classifcation

# Set seed for reproducibility
set.seed(123)

# Define repeated cross validation with 5 folds and three repeats
repeat_cv_three_management_classes <- caret::trainControl(method='repeatedcv', number=5, repeats=3)

# Replace NA in ForestPlot_DI and ForestPlot_CE
new_gaps_plot_code_three_management_classes_na_replaced <- new_gaps_plot_code_three_management_classes %>% 
  tidyr::replace_na(list(ForestPlot_DI = 0, ForestPlot_CE = 0))

# Split the data so that we use 70% of it for training
train_index_three_management_classes <- caret::createDataPartition(y=new_gaps_plot_code_three_management_classes$managed_class, p=0.7, list=FALSE)

# Subset the data
train_three_management_classes <- new_gaps_plot_code_three_management_classes_na_replaced[train_index, ] %>% 
  sf::st_drop_geometry() %>% 
  tibble() %>% 
  dplyr::select(-ID, -geom, -zentropy_AHN3, -zentropy_AHN4, 
                -Buffer5m_zentropy_AHN3, -Buffer5m_zentropy_AHN4,
                -Buffer10m_zentropy_AHN3, -Buffer10m_zentropy_AHN4,
                -ForestPlot_fraq_LC)

test_three_management_classes <- new_gaps_plot_code_three_management_classes_na_replaced[-train_index, ] %>% 
  sf::st_drop_geometry() %>% 
  tibble() %>% 
  dplyr::select(-ID, -geom, -zentropy_AHN3, -zentropy_AHN4, 
                -Buffer5m_zentropy_AHN3, -Buffer5m_zentropy_AHN4,
                -Buffer10m_zentropy_AHN3, -Buffer10m_zentropy_AHN4,
                -ForestPlot_fraq_LC)


## Random forest classification all metrics

# Train model with quick function
forest_three_management_classes_short = randomForest(x = train_three_management_classes[-1],
                                                     y = train_three_management_classes$managed_class,
                                                     ntree = 500)

# Train a random forest model
forest_three_management_classes <- caret::train(
  
  # Formula. We are using all variables to predict class
  managed_class~., 
  
  # Source of data; Remove the class variable
  data=train_three_management_classes, 
  
  # `rf` method for random forest
  method='rf', 
  
  # Add repeated cross validation as trControl
  trControl=repeat_cv_managed,
  
  # Accuracy to measure the performance of the model
  metric='Accuracy')

# Check result
forest_three_management_classes$finalModel

# Get variable importance
var_imp_three_management_classes <- varImp(forest_three_management_classes, scale=FALSE)$importance
var_imp_three_management_classes <- data.frame(variables=row.names(var_imp_three_management_classes), importance=var_imp_three_management_classes$Overall)

# Create a plot of variable importance
var_imp_three_management_classes %>%
  
  ## Sort the data by importance
  arrange(importance) %>%
  
  ## Select most important 20 rows
  dplyr::filter(importance > 3.7) %>% 
  
  ## Create a ggplot object for aesthetic
  ggplot(aes(x=reorder(variables, importance), y=importance)) + 
  
  ## Plot the bar graph
  geom_bar(stat='identity') + 
  
  ## Flip the graph to make a horizontal bar plot
  coord_flip() + 
  
  ## Add x-axis label
  xlab('Variables') +
  
  ## Add a title
  labs(title='RF model management type variable importance') + 
  
  ## Some layout for the plot
  theme_minimal() + 
  theme(axis.text = element_text(size = 10), 
        axis.title = element_text(size = 15), 
        plot.title = element_text(size = 15), 
  )

# Generate predictions
y_hats_three_management_classes <- predict(
  
  ## Random forest object
  object=forest_three_management_classes_short, 
  
  ## Data to use for predictions; remove the class
  newdata=test_three_management_classes)

# Print the accuracy
accuracy_three_management_classes <- mean(y_hats_three_management_classes == test_three_management_classes$managed_class)*100
cat('Accuracy on testing data: ', round(accuracy_three_management_classes, 2), '%',  sep='') # Accuracy on testing data: 99.6%%

# Save model
save(forest_three_management_classes, file = "~/ahncanopygaps/InputData/RandomForestModels/forest_three_management_classes.RData")

# Confusion matrix on test data
confusionMatrix(y_hats_three_management_classes, reference = as.vector(test_three_management_classes[,1])$managed_class)

## Random forest classification 5 most important classes

# prepare data. TMC = tree management classes
train_TMC_top5 <- train_three_management_classes %>% 
  dplyr::select(managed_class, ForestPlot_gini_CHM4,
                ForestPlot_fraq_NoG, ForestPlot_fraq_NGCM,
                ForestPlot_CE, ForestPlot_PIG)

test_TMC_top5 <- test_three_management_classes %>% 
  dplyr::select(managed_class, ForestPlot_gini_CHM4,
                ForestPlot_fraq_NoG, ForestPlot_fraq_NGCM,
                ForestPlot_CE, ForestPlot_PIG)

# Check colinearlity
corrplot(cor(train_TMC_top5[-1]), method = "number")

# Train model with quick function
forest_TMC_top5_short = randomForest(x = train_TMC_top5[-1],
                                                     y = train_TMC_top5$managed_class,
                                                     ntree = 500)

# Train a random forest model
forest_TMC_top5 <- caret::train(
  
  # Formula. We are using all variables to predict class
  managed_class~., 
  
  # Source of data; Remove the class variable
  data=train_TMC_top5, 
  
  # `rf` method for random forest
  method='rf', 
  
  # Add repeated cross validation as trControl
  trControl=repeat_cv_managed,
  
  # Accuracy to measure the performance of the model
  metric='Accuracy')

# Check result
forest_TMC_top5$finalModel

# Get variable importance
var_imp_TMC_top5 <- varImp(forest_TMC_top5, scale=FALSE)$importance
var_imp_TMC_top5 <- data.frame(variables=row.names(var_imp_TMC_top5), importance=var_imp_TMC_top5$Overall)

# Create a plot of variable importance
var_imp_TMC_top5 %>%
  
  ## Sort the data by importance
  arrange(importance) %>%
  
  ## Create a ggplot object for aesthetic
  ggplot(aes(x=reorder(variables, importance), y=importance)) + 
  
  ## Plot the bar graph
  geom_bar(stat='identity') + 
  
  ## Flip the graph to make a horizontal bar plot
  coord_flip() + 
  
  ## Add x-axis label
  xlab('Variables') +
  
  ## Add a title
  labs(title='Random forest variable importance') + 
  
  ## Some layout for the plot
  theme_minimal() + 
  theme(axis.text = element_text(size = 10), 
        axis.title = element_text(size = 15), 
        plot.title = element_text(size = 20), 
  )

# Generate predictions
y_hats_TMC_top5 <- predict(
  
  ## Random forest object
  object=forest_TMC_top5, 
  
  ## Data to use for predictions; remove the class
  newdata=test_TMC_top5)

# Print the accuracy
accuracy_TMC_top5 <- mean(y_hats_TMC_top5 == test_TMC_top5$managed_class)*100
cat('Accuracy on testing data: ', round(accuracy_TMC_top5, 2), '%',  sep='') # Accuracy on testing data: 99.87%

# Save model
save(forest_TMC_top5, file = "~/ahncanopygaps/InputData/RandomForestModels/forest_TMC_top5.RData")


## Random forest classification 4 most important classes

# prepare data (TMC = tree management classes)
train_TMC_top4 <- train_three_management_classes %>% 
  dplyr::select(managed_class, ForestPlot_gini_CHM4,
                ForestPlot_fraq_NoG, ForestPlot_fraq_NGCM,
                ForestPlot_GD)

test_TMC_top4 <- test_three_management_classes %>% 
  dplyr::select(managed_class, ForestPlot_gini_CHM4,
                ForestPlot_fraq_NoG, ForestPlot_fraq_NGCM,
                ForestPlot_GD) %>% 
  drop_na()

# Check colinearlity
corrplot(cor(train_TMC_top4[-1]), method = "number")

# Train model with quick function
forest_TMC_top4_short = randomForest(x = train_TMC_top4[-1],
                                     y = train_TMC_top4$managed_class,
                                     ntree = 500)

# Train a random forest model
forest_TMC_top4 <- caret::train(
  
  # Formula. We are using all variables to predict class
  managed_class~., 
  
  # Source of data; Remove the class variable
  data=train_TMC_top4, 
  
  # `rf` method for random forest
  method='rf', 
  
  # Add repeated cross validation as trControl
  trControl=repeat_cv_managed,
  
  # Accuracy to measure the performance of the model
  metric='Accuracy')

# Check result
forest_TMC_top4$finalModel

# Get variable importance
var_imp_TMC_top4 <- varImp(forest_TMC_top4, scale=FALSE)$importance
var_imp_TMC_top4 <- data.frame(variables=row.names(var_imp_TMC_top4), importance=var_imp_TMC_top4$Overall)

# Create a plot of variable importance
var_imp_TMC_top4 %>%
  
  ## Sort the data by importance
  arrange(importance) %>%
  
  ## Create a ggplot object for aesthetic
  ggplot(aes(x=reorder(variables, importance), y=importance)) + 
  
  ## Plot the bar graph
  geom_bar(stat='identity') + 
  
  ## Flip the graph to make a horizontal bar plot
  coord_flip() + 
  
  ## Add x-axis label
  xlab('Variables') +
  
  ## Add a title
  labs(title='Random forest variable importance') + 
  
  ## Some layout for the plot
  theme_minimal() + 
  theme(axis.text = element_text(size = 10), 
        axis.title = element_text(size = 15), 
        plot.title = element_text(size = 14), 
  )

# Generate predictions
y_hats_TMC_top4 <- predict(
  
  ## Random forest object
  object=forest_TMC_top4, 
  
  ## Data to use for predictions; remove the class
  newdata=test_TMC_top4[-1])

# Print the accuracy
accuracy_TMC_top4 <- mean(y_hats_TMC_top4 == test_TMC_top4$managed_class)*100
cat('Accuracy on testing data: ', round(accuracy_TMC_top4, 2), '%',  sep='') # Accuracy on testing data: 99.73%

# Save model
save(forest_TMC_top4, file = "~/ahncanopygaps/InputData/RandomForestModels/forest_TMC_top4.RData")


## Random forest classification without forest plot classes

# prepare data. TMC = tree management classes
train_TMC_non_forestplot <- train_three_management_classes %>% 
  dplyr::select(-contains("ForestPlot"))

test_TMC_non_forestplot <- test_three_management_classes %>% 
  dplyr::select(-contains("ForestPlot"))

# Check colinearlity
corrplot(cor(train_TMC_non_forestplot[-1]), method = "number")

# Train model with quick function
forest_TMC_non_forestplot_short = randomForest(x = train_TMC_non_forestplot[-1],
                                               y = train_TMC_non_forestplot$managed_class,
                                               ntree = 500)

# Train a random forest model
forest_TMC_non_forestplot <- caret::train(
  
  # Formula. We are using all variables to predict class
  managed_class~., 
  
  # Source of data; Remove the class variable
  data=train_TMC_non_forestplot, 
  
  # `rf` method for random forest
  method='rf', 
  
  # Add repeated cross validation as trControl
  trControl=repeat_cv_managed,
  
  # Accuracy to measure the performance of the model
  metric='Accuracy')

# Check result
forest_TMC_non_forestplot$finalModel

# Get variable importance
var_imp_TMC_non_forestplot <- varImp(forest_TMC_non_forestplot, scale=FALSE)$importance
var_imp_TMC_non_forestplot <- data.frame(variables=row.names(var_imp_TMC_non_forestplot), importance=var_imp_TMC_non_forestplot$Overall)

# Create a plot of variable importance
var_imp_TMC_non_forestplot[- grep("ntree_class", var_imp_TMC_non_forestplot$variables),] %>%
  
  ## Sort the data by importance
  arrange(importance) %>%
  
  ## Select most important 20 rows
  dplyr::filter(importance > 3.18) %>%  
  
  ## Create a ggplot object for aesthetic
  ggplot(aes(x=reorder(variables, importance), y=importance)) + 
  
  ## Plot the bar graph
  geom_bar(stat='identity') + 
  
  ## Flip the graph to make a horizontal bar plot
  coord_flip() + 
  
  ## Add x-axis label
  xlab('Variables') +
  
  ## Add a title
  labs(title='RF model management type without forest plot, variable importance') + 
  
  ## Some layout for the plot
  theme_minimal() + 
  theme(axis.text = element_text(size = 7), 
        axis.title = element_text(size = 10), 
        plot.title = element_text(size = 12), 
  )

# Generate predictions
y_hats_TMC_non_forestplot <- predict(
  
  ## Random forest object
  object=forest_TMC_non_forestplot, 
  
  ## Data to use for predictions; remove the class
  newdata=test_TMC_non_forestplot[-1])

# Print the accuracy
accuracy_TMC_non_forestplot <- mean(y_hats_TMC_non_forestplot == test_TMC_non_forestplot$managed_class)*100
cat('Accuracy on testing data: ', round(accuracy_TMC_non_forestplot, 2), '%',  sep='') # Accuracy on testing data: 89.88%

# Save model
save(forest_TMC_non_forestplot, file = "~/ahncanopygaps/InputData/RandomForestModels/forest_TMC_non_forestplot.RData")


## Random forest classification 4 most non forest plot important classes

# prepare data. TMC = tree management classes
train_TMC_non_forestplot_top4 <- train_three_management_classes %>% 
  dplyr::select(managed_class, Buffer10m_ipcumzq30_AHN4,
                Buffer10m_ipcumzq10_AHN4, Buffer10m_ipcumzq70_AHN3,
                Buffer10m_zmean_AHN4)

test_TMC_non_forestplot_top4 <- test_three_management_classes %>% 
  dplyr::select(managed_class, Buffer10m_ipcumzq30_AHN4,
                Buffer10m_ipcumzq10_AHN4, Buffer10m_ipcumzq70_AHN3,
                Buffer10m_zmean_AHN4)

# Check colinearlity
corrplot(cor(train_TMC_non_forestplot_top4[-1]), method = "number")

# Train model with quick function
forest_TMC_non_forestplot_top4_short = randomForest(x = train_TMC_non_forestplot_top4[-1],
                                                    y = train_TMC_non_forestplot_top4$managed_class,
                                                    ntree = 500)

# Train a random forest model
forest_TMC_non_forestplot_top4 <- caret::train(
  
  # Formula. We are using all variables to predict class
  managed_class~., 
  
  # Source of data; Remove the class variable
  data=train_TMC_non_forestplot_top4, 
  
  # `rf` method for random forest
  method='rf', 
  
  # Add repeated cross validation as trControl
  trControl=repeat_cv_managed,
  
  # Accuracy to measure the performance of the model
  metric='Accuracy')

# Check result
forest_TMC_non_forestplot_top4$finalModel

# Get variable importance
var_imp_TMC_non_forestplot_top4 <- varImp(forest_TMC_non_forestplot_top4, scale=FALSE)$importance
var_imp_TMC_non_forestplot_top4 <- data.frame(variables=row.names(var_imp_TMC_non_forestplot_top4), importance=var_imp_TMC_non_forestplot_top4$Overall)

# Create a plot of variable importance
var_imp_TMC_non_forestplot_top4 %>%
  
  ## Sort the data by importance
  arrange(importance) %>%
  
  ## Create a ggplot object for aesthetic
  ggplot(aes(x=reorder(variables, importance), y=importance)) + 
  
  ## Plot the bar graph
  geom_bar(stat='identity') + 
  
  ## Flip the graph to make a horizontal bar plot
  coord_flip() + 
  
  ## Add x-axis label
  xlab('Variables') +
  
  ## Add a title
  labs(title='Random forest variable importance') + 
  
  ## Some layout for the plot
  theme_minimal() + 
  theme(axis.text = element_text(size = 7), 
        axis.title = element_text(size = 10), 
        plot.title = element_text(size = 10), 
  )

# Generate predictions
y_hats_TMC_non_forestplot_top4 <- predict(
  
  ## Random forest object
  object=forest_TMC_non_forestplot_top4, 
  
  ## Data to use for predictions; remove the class
  newdata=test_TMC_non_forestplot_top4[-1])

# Print the accuracy
accuracy_TMC_non_forestplot_top4 <- mean(y_hats_TMC_non_forestplot_top4 == test_TMC_non_forestplot_top4$managed_class)*100
cat('Accuracy on testing data: ', round(accuracy_TMC_non_forestplot_top4, 2), '%',  sep='') # Accuracy on testing data: 89.88%

# Save model
save(forest_TMC_non_forestplot_top4, file = "~/ahncanopygaps/InputData/RandomForestModels/forest_TMC_non_forestplot_top4.RData")


## Create Random Forest classification of canopy gaps in managed and unmanaged 
## forest plots without the ForestPlot metrics

# Remove ForestPlot metrics from train and test data
train_managed_noForestPlots <- train_managed %>% 
  dplyr::select(-dplyr::matches("ForestPlot"))

test_managed_noForestPlots <- test_managed %>% 
  dplyr::select(-dplyr::matches("ForestPlot"))

# Train a random forest model without the ForestPlot metrics
forest_managed_noForestPlot <- caret::train(
  
  # Formula. We are using all variables to predict class
  managed_class~., 
  
  # Source of data; Remove the class variable
  data=train_managed_noForestPlots, 
  
  # `rf` method for random forest
  method='rf', 
  
  # Add repeated cross validation as trControl
  trControl=repeat_cv_managed,
  
  # Accuracy to measure the performance of the model
  metric='Accuracy')

# Check result
forest_managed_noForestPlot$finalModel

# Get variable importance
var_imp_managed_noForestPlot <- varImp(forest_managed_noForestPlot, scale=FALSE)$importance
var_imp_managed_noForestPlot <- data.frame(variables=row.names(var_imp_managed_noForestPlot), 
                                           importance=var_imp_managed_noForestPlot$Overall)

# Create a plot of variable importance
var_imp_managed_noForestPlot %>%
  
  ## Sort the data by importance
  arrange(importance) %>%
  
  ## Select most important 20 rows
  dplyr::filter(importance > 2.5) %>% 
  
  ## Create a ggplot object for aesthetic
  ggplot(aes(x=reorder(variables, importance), y=importance)) + 
  
  ## Plot the bar graph
  geom_bar(stat='identity') + 
  
  ## Flip the graph to make a horizontal bar plot
  coord_flip() + 
  
  ## Add x-axis label
  xlab('Variables') +
  
  ## Add a title
  labs(title='Random forest variable importance') + 
  
  ## Some layout for the plot
  theme_minimal() + 
  theme(axis.text = element_text(size = 10), 
        axis.title = element_text(size = 15), 
        plot.title = element_text(size = 20), 
  )

# Generate predictions
y_hats_managed_noForestPlot <- predict(
  
  ## Random forest object
  object=forest_managed_noForestPlot, 
  
  ## Data to use for predictions; remove the class
  newdata=test_managed_noForestPlots)

# Print the accuracy
accuracy_managed_noForestPlot <- mean(y_hats_managed_noForestPlot == test_managed_noForestPlots$managed_class)*100
cat('Accuracy on testing data: ', round(accuracy_managed_noForestPlot, 2), '%',  sep='') # Accuracy on testing data: 95.41%

# Save model
save(forest_managed_noForestPlot, file = "~/ahncanopygaps/InputData/RandomForestModels/forest_managed_noForestPlot.RData")


## Create Random Forest classification of canopy gaps in managed and unmanaged 
## forest plots without the ForestPlot metrics and without buffer metrics

# Remove ForestPlot metrics from train and test data
train_managed_noForestPlots_noBuffer <- train_managed %>% 
  dplyr::select(-dplyr::matches("ForestPlot")) %>% 
  dplyr::select(-dplyr::matches("Buffer5m")) %>% 
  dplyr::select(-dplyr::matches("Buffer10m"))

test_managed_noForestPlots_noBuffer <- test_managed %>% 
  dplyr::select(-dplyr::matches("ForestPlot")) %>% 
  dplyr::select(-dplyr::matches("Buffer5m")) %>% 
  dplyr::select(-dplyr::matches("Buffer10m"))

# Train a random forest model without the ForestPlot metrics and without
# buffer metrics
forest_managed_noForestPlot_noBuffer <- caret::train(
  
  # Formula. We are using all variables to predict class
  managed_class~., 
  
  # Source of data; Remove the class variable
  data=train_managed_noForestPlots_noBuffer, 
  
  # `rf` method for random forest
  method='rf', 
  
  # Add repeated cross validation as trControl
  trControl=repeat_cv_managed,
  
  # Accuracy to measure the performance of the model
  metric='Accuracy')

# Check result
forest_managed_noForestPlot_noBuffer$finalModel

# Get variable importance
var_imp_managed_noForestPlot_noBuffer <- varImp(forest_managed_noForestPlot_noBuffer, scale=FALSE)$importance
var_imp_managed_noForestPlot_noBuffer <- data.frame(variables=row.names(var_imp_managed_noForestPlot_noBuffer), 
                                           importance=var_imp_managed_noForestPlot_noBuffer$Overall)

# Create a plot of variable importance
var_imp_managed_noForestPlot_noBuffer %>%
  
  ## Sort the data by importance
  arrange(importance) %>%
  
  ## Select most important 20 rows
  dplyr::filter(importance > 3) %>% 
  
  ## Create a ggplot object for aesthetic
  ggplot(aes(x=reorder(variables, importance), y=importance)) + 
  
  ## Plot the bar graph
  geom_bar(stat='identity') + 
  
  ## Flip the graph to make a horizontal bar plot
  coord_flip() + 
  
  ## Add x-axis label
  xlab('Variables') +
  
  ## Add a title
  labs(title='Random forest variable importance') + 
  
  ## Some layout for the plot
  theme_minimal() + 
  theme(axis.text = element_text(size = 10), 
        axis.title = element_text(size = 15), 
        plot.title = element_text(size = 15), 
  )

# Generate predictions
y_hats_managed_noForestPlot_noBuffer <- predict(
  
  ## Random forest object
  object=forest_managed_noForestPlot_noBuffer, 
  
  ## Data to use for predictions; remove the class
  newdata=test_managed_noForestPlots_noBuffer[-1])

# Print the accuracy
accuracy_managed_noForestPlot_noBuffer <- mean(y_hats_managed_noForestPlot_noBuffer == test_managed_noForestPlots_noBuffer$managed_class)*100
cat('Accuracy on testing data: ', round(accuracy_managed_noForestPlot_noBuffer, 2), '%',  sep='') # Accuracy on testing data: 92.71%

# Save model
save(forest_managed_noForestPlot_noBuffer, file = "~/ahncanopygaps/InputData/RandomForestModels/forest_managed_noForestPlot_noBuffer.RData")

