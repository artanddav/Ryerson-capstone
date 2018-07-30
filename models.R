library(catboost)
library(xgboost)
library(lightgbm)
library(Matrix)
library(caret)
library("pROC")


train=read.csv("F:\\Ryerson\\CKME136\\Porto Seguro\\train_rand_forest_selected.csv")

folds<-createFolds(train$target,k=2)

test<-train[folds$Fold1,]
train<-train[folds$Fold2,]

y_train<-as.integer(train$target)
y_test<-as.integer(test$target)
class(y_train)
train<-train[,!(names(train) %in% c("id","X", "target"))]
test<-test[,!(names(test) %in% c("id","X", "target"))]

#========  Catboost  =======#
train_pool <- catboost.load_pool(data=train, 
                                 label = y_train)
test_pool <- catboost.load_pool(data=test,# best 0.8194586979 0.6251485243 0.8203588994 0.8196464848 0.8199473654 0.8191229621 0.8190388572 0.8200160915 0.6248274252  0.6248700572
                                label = y_test)# iter269           449                                               378          357           334         544           441
c_params <- list(iterations = 1000,           # 1000
                   thread_count = 4,            # 4
                   loss_function = 'Logloss',   # logloss
                   eval_metric = 'AUC',         # auc
                   border_count = 32,           # 32
                   depth = 5,                   # 5
                   learning_rate = 0.03,        # 0.03
                   l2_leaf_reg = 3)           # 3.5       = 3             = 2.5        =2           =1.5          =1          =.5          =4            =4.5          =5
c_model <- catboost.train(train_pool,params = c_params)
c_pred<-catboost.predict(c_model,test_pool,verbose = T)
roc_obj <- roc(y_test, c_pred)
auc(roc_obj)

#========  XGBoost ========#
x_train<-Matrix(as.matrix(train), sparse=TRUE)
x_test<-Matrix(as.matrix(test), sparse=TRUE)
x_params = list(objective = "binary:logistic",
             #metrics = "auc",
             min_child_weight = 50,
             max_delta_step = 7,#===================
             tree_method="approx",
             eta = 0.05,
             nthread = 4,
             gamma = 0.005,
             max_depth  = 5,
             alpha = 1,
             lambda = 1.3,
             min_data_in_bin=100,
             min_gain_to_split = 10,
             min_data_in_leaf = 30,
             early_stopping_rounds = 25,
             eval_metric = "auc",
             is_unbalance = TRUE,
             verbose = T,
             print_every_n = 20,
             maximize = TRUE
             )
x_model<-xgboost(data = x_train, 
                 label = y_train,
                 params = x_params,
                 nrounds = 1000
                 )
x_pred<-predict(x_model,x_test)
roc_obj <- roc(y_test, x_pred)
auc(roc_obj)

#========  LightGBM  ========#
l_train<-Matrix(as.matrix(train), sparse=TRUE)
l_test<-Matrix(as.matrix(test), sparse=TRUE)

l_params = list(objective = "binary",
                metric = "auc",
                min_sum_hessian_in_leaf = 1,
                feature_fraction = 0.3,
                bagging_fraction = 0.7,
                bagging_freq = 5,
                max_bin = 50,
                lambda_l1 = 8,
                lambda_l2 = 1.3,
                min_data_in_bin=100,
                min_gain_to_split = 10,
                min_data_in_leaf = 30,
                is_unbalance = TRUE,
                learning_rate = 0.01,
                num_leaves = 15,
                early_stopping_rounds = 50,
                num_threads = 4)
d_train = lgb.Dataset(data=l_train,
                      label=y_train)

l_model <- lightgbm(data = d_train,
                    params = l_params,
                    eval_freq = 20, 
                    nrounds = 2000)
l_pred<-predict(l_model,l_test)
roc_obj <- roc(y_test, l_pred)
auc(roc_obj)
