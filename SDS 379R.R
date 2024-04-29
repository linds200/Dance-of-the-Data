dwts = read.csv("DWTS.csv") # import dataset

## Prepare/Clean Data ##

library(fastDummies)
dwts_dummy = dummy_cols(dwts, select_columns = c("Race", "Gender", "Dance.Role","Category", "Professional.Partner")) # create dummy variables

dwts_dummy["Race_Indeigenous"] = 0 # create lone indigenous column

for (i in 1:nrow(dwts_dummy)) {
  if(dwts_dummy[i, "Race_Asian, Black"] == 1) {
    dwts_dummy[i, "Race_Asian"] = 1
    dwts_dummy[i, "Race_Black"] = 1
  }
  if(dwts_dummy[i, "Race_Asian, Pacific Islander"] == 1) {
    dwts_dummy[i, "Race_Asian"] = 1
    dwts_dummy[i, "Race_Pacific Islander"] = 1
  }
  if(dwts_dummy[i, "Race_Asian, Pacific Islander, Hispanic/Latino"] == 1) {
    dwts_dummy[i, "Race_Asian"] = 1
    dwts_dummy[i, "Race_Pacific Islander"] = 1
    dwts_dummy[i, "Race_Hispanic/Latino"] = 1
  }
  if(dwts_dummy[i, "Race_Hispanic/Latino, Indigenous"] == 1) {
    dwts_dummy[i, "Race_Hispanic/Latino"] = 1
    dwts_dummy[i, "Race_Indeigenous"] = 1
  }
}

for (i in 1:nrow(dwts_dummy)) {
  if(dwts_dummy[i, "Category_Acting, Music"] == 1 | dwts_dummy[i, "Category_Music, Acting"] == 1) {
    dwts_dummy[i, "Category_Acting"] = 1
    dwts_dummy[i, "Category_Music"] = 1
  }
  if(dwts_dummy[i, "Category_Acting, Talk Show & News"] == 1) {
    dwts_dummy[i, "Category_Acting"] = 1
    dwts_dummy[i, "Category_Talk Show & News"] = 1
  }
}

dwts_dummy = dwts_dummy[-c(3:5, 7, 8, 13:15, 18, 26, 27, 32)] # drop unnecessary columns
dwts_dummy = dwts_dummy[c(1:11, 79, 12:78)] # reorder columns so each category is together

dwts_dummy = dwts_dummy[!(dwts_dummy$Finish == "WD"),] # drop NA
dwts = dwts[!(dwts$Finish == "WD"),]
dwts_dummy$Finish = as.numeric(as.character(dwts_dummy$Finish)) # convert finish column to numeric
dwts$Finish = as.numeric(as.character(dwts$Finish))

## Test Train Split ##

test = dwts[dwts$Season %in% c("Season 19", "Season 20", "Season 21", "Season 23","Season 24", "Season 29", "Season 30", "Season 31", "Season 32"),]

train = dwts[dwts$Season %in% c("Season 1", "Season 2", "Season 3", "Season 4",  "Season 5", "Season 6", "Season 7", "Season 8", "Season 9", "Season 10", "Season 11", "Season 12", "Season 13", "Season 14", "Season 15", "Season 16", "Season 17", "Season 18", "Season 22", "Season 25", "Season 26", "Season 27", "Season 28"),]

test_dummy = dwts_dummy[dwts_dummy$Season %in% c("Season 19", "Season 20", "Season 21", "Season 23", "Season 24", "Season 29", "Season 30", "Season 31", "Season 32"),]

train_dummy = dwts_dummy[dwts_dummy$Season %in% c("Season 1", "Season 2", "Season 3", "Season 4", "Season 5", "Season 6", "Season 7", "Season 8",  "Season 9", "Season 10", "Season 11", "Season 12", "Season 13", "Season 14", "Season 15", "Season 16", "Season 17", "Season 18", "Season 22", "Season 25", "Season 26", "Season 27", "Season 28"),]

x_train = data.matrix(train_dummy[, c(2, 6:79)])
y_train = train_dummy[, "Finish"]

x_test = data.matrix(test_dummy[, c(2, 6:79)])
y_test = test_dummy[, "Finish"]

x = data.matrix(dwts_dummy[, c(2, 6:79)])
y = dwts_dummy[, "Finish"]

## Clustering ##

dwts_cluster = dwts[c(2:5, 7, 8, 11)]
dwts_cluster_num = dwts[c(2, 11)]

# K-Means Clustering
library(cluster)
library(factoextra)

fviz_nbclust(dwts_cluster_num, kmeans, method = "wss")

set.seed(123)
kmean_results = kmeans(dwts_cluster_num, 3, nstart = 25) #k-means cluster with k = 3
print(kmean_results)

aggregate(dwts_cluster_num, by = list(cluster = kmean_results$cluster), mean)
fviz_cluster(kmean_results, dwts_cluster_num, ellipse.type = "norm", main = "K-Means Cluster Plot", ylab = "Average Judge Score", show_legend = F)
dwts["Kmean Cluster"] = kmean_results$cluster
mean(dwts$Finish[dwts$`Kmean Cluster` == 1])
mean(dwts$Finish[dwts$`Kmean Cluster` == 2])
mean(dwts$Finish[dwts$`Kmean Cluster` == 3])

# K-Modes Clustering
library(klaR)
k.max = 15
wss = sapply(1:k.max, 
             function(k){set.seed(100000)
               sum(kmodes(dwts_cluster, k, iter.max = 100 ,weighted = FALSE)$withindiff)})

plot(1:k.max, wss, type = "b", xlab = "Number of clusters k", ylab = "Within-clusters sum of squares (WCSS)")
title("Optimal number of clusters", font.main = 1, line = 1, adj = 0)

kmode.results = kmodes(dwts_cluster, 6, iter.max = 10, weighted = FALSE, fast = TRUE)
print(kmode.results)
dwts["Kmode Cluster"] = kmode.results$cluster

library(gridExtra)

for (i in 1:6){
  cluster = dwts[dwts$`Kmode Cluster` == i, ]
  par(mfrow = c(3,3), mar = c(5.1, 4.1, 4.1, 2.1))
  p1 = hist(cluster$Age, main = "Age", xlab = ' ')
  p2 = hist(cluster$Average.Judge.Score, main = "Average Judge Score", xlab = ' ')
  p3 = hist(cluster$Finish, main = "Finish", xlab = ' ')
  p4 = barplot(table(cluster$Gender), main = "Gender", ylab = "Frequency")
  p5 = barplot(table(cluster$Race), main = "Race", ylab = "Frequency", las = 2)
  p6 = barplot(table(cluster$Category), main  = "Notability Category", ylab = "Frequency", las = 2)
  par(mfrow = c(1,1), mar = c(12, 4.1, 4.1, 2.1))
  p7 = barplot(table(cluster$Professional.Partner), main = "Professional Partner", ylab = "Frequency", las = 2)
}

par(mfrow = c(1,1), mar = c(5.1, 4.1, 4.1, 2.1)) #reset plot settings

## Multiple Linear Regression (with all dependent variables) ##
Finish_train = data.matrix(y_train)
linear_model = lm(Finish_train ~. , data = data.frame(x_train, Finish_train))

summary(linear_model) 
Finish_test = data.matrix(y_test)
linear_pred = predict(linear_model, newdata = data.frame(x_test, Finish_test))
linear_mse = mean((linear_pred - y_test)^2) # MSE
linear_rmse = sqrt(mean((linear_pred - y_test)^2)) # RMSE
linear_AIC = AIC(linear_model)

## Multiple Linear Regression (with significant variables) ##
x_train_sig = data.matrix(train_dummy[, c(6, 36, 43, 46, 66)])
x_test_sig = data.matrix(test_dummy[, c(6, 36, 43, 46, 66)])

linearsig_model =  lm(Finish_train ~. , data = data.frame(x_train_sig, Finish_train))

summary(linearsig_model)
Finish_test = data.matrix(y_test)
linearsig_pred = predict(linearsig_model, newdata = data.frame(x_test_sig, Finish_test))
linearsig_mse = mean((linearsig_pred - y_test)^2) # MSE
linearsig_rmse = sqrt(mean((linearsig_pred - y_test)^2)) # RMSE
linearsig_AIC = AIC(linearsig_model)

## AIC-based Stepwise Regression ##
forward_model = lm(Finish_train ~ 1, data = data.frame(x_train, Finish_train))
forwardAIC_model = step(forward_model, direction = "forward", scope = formula(linear_model))
summary(forwardAIC_model)
forwardAIC_pred = predict(forwardAIC_model, newdata = data.frame(x_test, Finish_test))
forwardAIC_mse = mean((forwardAIC_pred - y_test)^2) # MSE
forwardAIC_rmse = sqrt(mean((forwardAIC_pred - y_test)^2)) # RMSE
forwardAIC_AIC = AIC(forwardAIC_model)

backwardAIC_model = step(linear_model, direction = "backward")
summary(backwardAIC_model)
backwardAIC_pred = predict(backwardAIC_model, newdata = data.frame(x_test, Finish_test))
backwardAIC_mse = mean((backwardAIC_pred - y_test)^2) # MSE
backwardAIC_rmse = sqrt(mean((backwardAIC_pred - y_test)^2)) # RMSE
backwardAIC_AIC = AIC(backwardAIC_model)

bothAIC_model = step(linear_model, direction = "both")
summary(bothAIC_model)
bothAIC_pred = predict(bothAIC_model, newdata = data.frame(x_test, Finish_test))
bothAIC_mse = mean((bothAIC_pred - y_test)^2) # MSE
bothAIC_rmse = sqrt(mean((bothAIC_pred - y_test)^2)) # RMSE
bothAIC_AIC = AIC(bothAIC_model)

## p-value-based Stepwise Regression ##
library(olsrr)
forwardp_model = ols_step_forward_p(linear_model)$model
summary(forwardp_model)
forwardp_pred = predict(forwardp_model, newdata = data.frame(x_test, Finish_test))
forwardp_mse = mean((forwardp_pred - y_test)^2) # MSE
forwardp_rmse = sqrt(mean((forwardp_pred - y_test)^2)) # RMSE
forwardp_AIC = AIC(forwardp_model)

backwardp_model = ols_step_backward_p(linear_model)$model
summary(backwardp_model)
backwardp_pred = predict(backwardp_model, newdata = data.frame(x_test, Finish_test))
backwardp_mse = mean((backwardp_pred - y_test)^2) # MSE
backwardp_rmse = sqrt(mean((backwardp_pred - y_test)^2)) # RMSE
backwardp_AIC = AIC(backwardp_model)

bothp_model = ols_step_both_p(linear_model)$model 
summary(bothp_model)
bothp_pred = predict(bothp_model, newdata = data.frame(x_test, Finish_test))
bothp_mse = mean((bothp_pred - y_test)^2) # MSE
bothp_rmse = sqrt(mean((bothp_pred - y_test)^2)) # RMSE
bothp_AIC = AIC(bothp_model)

## Linear Regression Conditions ##

# distribution of residuals is nearly normal
hist(bothAIC_model$residuals, main = 'Histogram of Residuals', xlab = 'Residuals', ylab = 'Frequency')
qqnorm(bothAIC_model$residuals)

# constant variability of the residuals
plot(bothAIC_model, 1)

# independence of residuals
index = c(x_train['index'])
plot(index, linear_model$residuals)
length(linear_model$residuals)
length(index)

# each variable is linearly related to the outcome
plot(train$Average.Judge.Score, train$Finish, main = 'Average Judge Score vs. Finish', xlab = 'Average Judge Score', ylab = 'Finish')

boxplot(train$Finish ~ train$Gender,  xlab = 'Gender', ylab = 'Finish')

par(mar = c(12, 4.1, 4.1, 2.1))
boxplot(train$Finish ~ train$Professional.Partner, xlab = "", ylab = 'Finish', las = 2)
mtext('Professional Dance Partner', side = 1, line = 9)

par(mar = c(18, 4.1, 4.1, 2.1))
boxplot(train$Finish ~ train$Race,  xlab = '', ylab = 'Finish', las = 2)
mtext("Race", side = 1, line = 16)

par(mar = c(12, 4.1, 4.1, 2.1))
boxplot(train$Finish ~ train$Category,  xlab = '', ylab = 'Finish', las = 2)
mtext("Notability Category", side = 1, line = 10)
par(mar = c(5.1, 4.1, 4.1, 2.1))

## Ridge Regression ##
library(glmnet)
grid = 10^seq(10, -2, length = 100)
ridge_model = glmnet(x_train, y_train, alpha = 0, lambda = grid, thresh = 1e-12) # build the ridge regression model with training data
plot(ridge_model, xvar = "lambda", xlim = c(-5, 7))
title("Number of Variables", font.main = 1, line = 2.5, adj = 0)

## Cross Validation ##
set.seed(1)
ridge_CV = cv.glmnet(x_train, y_train, alpha = 0)
plot(ridge_CV)
title("Number of Variables", font.main = 1, line = 2.5, adj = 0)
ridge_bestlam = ridge_CV$lambda.min
ridge_bestlam

ridge_pred = predict(ridge_model, s = ridge_bestlam, newx = x_test)
ridge_mse = mean((ridge_pred - y_test)^2) # MSE
ridge_rmse = sqrt(mean((ridge_pred - y_test)^2)) #RMSE

ridge_best_model = glmnet(x, y, alpha = 0, lambda = ridge_bestlam)
ridge_coef = predict(ridge_best_model, type = "coefficients", s = ridge_bestlam)
ridge_coef

aic = function(model){
  tLL = deviance(model)
  k = model$dim[1]
  AIC = -tLL + 2*(k + 1)
  return(AIC)
}

ridge_AIC = aic(ridge_best_model)
ridge_AIC

## Lasso Regression ##
lasso_model = glmnet(x_train, y_train, alpha = 1, lambda = grid) #  build the lasso regression model with training data
par(mfrow = c(1,1))
plot(lasso_model, xvar = "lambda", xlim = c(-5, 3))
title("Number of Variables", font.main = 1, line = 2.5, adj = 0)

## Cross Validation ##
set.seed(1)
lasso_CV = cv.glmnet(x_train, y_train, alpha = 1)
plot(lasso_CV)
title("Number of Variables", font.main = 1, line = 2.5, adj = 0)
lasso_bestlam = lasso_CV$lambda.min
lasso_bestlam

lasso_pred = predict(lasso_model, s = lasso_bestlam, newx = x_test)
lasso_mse = mean((lasso_pred - y_test)^2) # MSE
lasso_rmse = sqrt(mean((lasso_pred - y_test)^2)) # RMSE

lasso_best_model = glmnet(x, y, alpha = 1, lambda = lasso_bestlam)
lasso_coef = predict(lasso_best_model, type = "coefficients", s = lasso_bestlam)

lasso_AIC = aic(lasso_best_model)
lasso_AIC

## Regression Tree (with all independent variables) ##
library(tree)
tree = tree(Finish_train ~., data = data.frame(x_train, Finish_train))
summary(tree)
plot(tree)
text(tree, pretty = 0)

## Cross Validation ##
set.seed(1)
tree_CV = cv.tree(tree)
plot(tree_CV$size, tree_CV$dev, type = "b", ylab = "Within-node Sum of Squares", xlab = "Number of Nodes")
title("Optimal Number of Nodes", font.main = 1, line = 1, adj = 0)

prune_tree = prune.tree(tree, best = 3)
plot(prune_tree)
text(prune_tree, pretty = 0)

tree_pred = predict(prune_tree, newdata = data.frame(x_test, Finish_test))

## Evaluation ##
tree_mse = mean((tree_pred - Finish_test)^2) # MSE
tree_rmse = sqrt(mean((tree_pred - Finish_test)^2)) # RMSE

## Regression Tree (without average judge's score) ##
tree_cat = tree(Finish_train ~., data = data.frame(x_train[, c(1, 3:75)], Finish_train))
summary(tree_cat)
plot(tree_cat)
text(tree_cat, pretty = 0)

## Cross Validation ##
set.seed(1)
tree_cat_CV = cv.tree(tree_cat)
plot(tree_cat_CV$size, tree_cat_CV$dev, type = "b",  ylab = "Within-node Sum of Squares", xlab = "Number of Nodes")
title("Optimal Number of Nodes", font.main = 1, line = 1, adj = 0)

prune_tree_cat = prune.tree(tree_cat, best = 6)
plot(prune_tree_cat)
text(prune_tree_cat, pretty = 0)

tree_cat_pred = predict(prune_tree_cat, newdata = data.frame(x_test, Finish_test))

## Evaluation ##
tree_cat_mse = mean((tree_cat_pred - Finish_test)^2) # MSE
tree_cat_rmse = sqrt(mean((tree_cat_pred - Finish_test)^2)) # RMSE
