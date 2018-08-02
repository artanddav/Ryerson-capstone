library("caret")
library("randomForest")
library("dplyr")

# reading training and testing data
train=read.csv("F:\\Ryerson\\CKME136\\Porto Seguro\\train.csv")
test  = read.csv("F:\\Ryerson\\CKME136\\Porto Seguro\\test.csv")

# drop columns ps_car_03_cat  and  ps_car_05_cat 
train<-train[,!(names(train) %in% c("ps_car_03_cat","ps_car_05_cat"))]
test<-test[,!(names(test) %in% c("ps_car_03_cat","ps_car_05_cat"))]

# custom function for replacing NA-s by mean
NA_to_mean = function(df){
  for (col in 1:ncol(df)){
    df[which(df[[col]]== -1),col] = NA
  }
  
  df = df %>% mutate_all(~ifelse(is.na(.), mean(., na.rm = TRUE), .)) %>% as.data.frame()
  return(df)
}
# custom function for replacing NA-s by median
NA_to_median = function(df){
  for (col in 1:ncol(df)){
    df[which(df[[col]]== -1),col] = NA
  }
  
  df = df %>% mutate_all(~ifelse(is.na(.), median(., na.rm = TRUE), .)) %>% as.data.frame()
  return(df)
}

# numerical part of data
train_num<-train[,c(grep("reg",colnames(train)),
                    grep("car_\\d{2}$",colnames(train)),
                    grep("calc_\\d{2}$",colnames(train)))]
test_num<-test[,c(grep("reg",colnames(test)),
                    grep("car_\\d{2}$",colnames(test)),
                    grep("calc_\\d{2}$",colnames(test)))]

# non numerical part of data
train_rest<-train[,!(names(train) %in% colnames(train_num))]
test_rest<-test[,!(names(test) %in% colnames(test_num))]

# implementing mean imput for numerical data
train_num<-NA_to_mean(train_num)
test_num<-NA_to_mean(test_num)

# implementing median imput for categorical data
train_rest<-NA_to_median(train_rest)
test_rest<-NA_to_median(test_rest)

# joining parts together
train<-cbind(train_rest,train_num)
test<-cbind(test_rest,test_num)

# handling imbalanced data
imbalance_ratio<-0.1

index_0<-as.numeric(rownames(train[train$target==0,]))
index_1<-as.numeric(rownames(train[train$target==1,]))

n_0<-length(index_0) # nrow = 573518
n_1<-length(index_1) # nrow = 21694

# selecting right amount of target = 0 cases
undersampled_n_0<-as.integer(n_0 * (1-imbalance_ratio)*n_1/(imbalance_ratio*n_0))

# target = 0 cases 195246 rows
undersampled_index_0<-sample(index_0,undersampled_n_0)

# creating new dataset from selected target = 0 and 1 cases
total_index<-c(undersampled_index_0,index_1)
train<-train[total_index,] # 216940 rows

# writing to the csv file
write.csv(train,
          "F:\\Ryerson\\CKME136\\Porto Seguro\\train_balanced.csv")

# creating dummy variables using one hot encoding 
# using dummyVars function
# selecting categorical variables from new dataset
ctg_train<-train[,c(grep("cat",colnames(train)))]
ctg_test<-test[,c(grep("cat",colnames(test)))]

# converting to factor
ctg_train<-apply(ctg_train,2,as.factor)
ctg_test<-apply(ctg_test,2,as.factor)

# numerical part of new dataset
num_train<-train[,!(colnames(train) %in% colnames(ctg_train))]
num_test<-test[,!(colnames(test) %in% colnames(ctg_test))]

# creating dummy variables
dummy_train<-dummyVars(~.,data=ctg_train,fullRank = T)
one_hot_train<-data.frame(predict(
  dummy_train,
  newdata = ctg_train)) #166 cols

# joining dummy and numerical parts together
train<-cbind(num_train,one_hot_train) #211 cols

# creating dummy variables for  testing set
dummy_test<-dummyVars(~.,data=ctg_test,fullRank = T)#test
one_hot_test<-data.frame(predict(
  dummy_test,
  newdata = ctg_test)) #166 cols

# joining dummy and numerical parts together
test<-cbind(num_test,one_hot_test) #210 cols

# making interaction variables from interval variables
# using model.matrix
# 22 col
# selecting numerical part
train_interval<-train[,c(grep("reg",colnames(num_train)),
                         grep("car",colnames(num_train)),
                         grep("calc_\\d{2}$",colnames(num_train)))]

# selcting non numerical part
train_rest<-train[,!(colnames(train) %in% colnames(train_interval))]

# creating interaction variables
train_interaction<-as.data.frame(
  model.matrix(~.^2-1,train_interval))#253 col
colnames(train)

# joining two parts together
train<-cbind(train_rest,train_interaction)#442

# writing to the csv file
write.csv(train,"F:\\Ryerson\\CKME136\\Porto Seguro\\train_dummy.csv")

# selecting numerical part for test set
test_interval<-test[,c(grep("reg",colnames(num_test)),
                         grep("car",colnames(num_test)),
                         grep("calc_\\d{2}$",colnames(num_test)))]

# selcting non numerical part for test set
test_rest<-test[,!(colnames(test) %in% colnames(test_interval))]

# creating interaction variables for test set
test_interaction<-as.data.frame(
  model.matrix(~.^2-1,test_interval))#253 col

#joining to parts together
test<-cbind(test_rest,test_interaction)#441

# writing to the csv file
write.csv(test,"F:\\Ryerson\\CKME136\\Porto Seguro\\test_dummy.csv")

# reading the data
train=read.csv("F:\\Ryerson\\CKME136\\Porto Seguro\\train_dummy.csv")

# implementing low variance filter
# removing columns with low variance < 1%
train_low_var<-train[,apply(train,2,function(x){
  ifelse(var(x)<0.01,F,T)
})] #341 cols, 101 dropped

# number of columns
ncol(train_low_var)

# writing to the csv file
write.csv(train_low_var,
          "F:\\Ryerson\\CKME136\\Porto Seguro\\train_low_var.csv")

# reading the test data
test=read.csv("F:\\Ryerson\\CKME136\\Porto Seguro\\test_dummy.csv")

# implementing low variance filter for test data
# removing columns with low variance < 1%
test_low_var<-test[,apply(test,2,function(x){
  ifelse(var(x)<0.01,F,T)
})] #336 cols, 101 dropped

# number of columns
ncol(test_low_var)

# writing to the csv file
write.csv(test_low_var,
          "F:\\Ryerson\\CKME136\\Porto Seguro\\test_low_var.csv")


# doing feature selection by random forest
#reading the data
train=read.csv("F:\\Ryerson\\CKME136\\Porto Seguro\\train_low_var.csv")
test=read.csv("F:\\Ryerson\\CKME136\\Porto Seguro\\test_low_var.csv")

# creating hyperparameters for random forest
X_train<-train[,!(colnames(train) %in% c("id","target"))]
Y_train<-train[,"target"]
labels<-colnames(X_train)

# random forest implementation
rf<-randomForest(X_train,as.factor(Y_train),
                 ntree=1000,
                 importance=T,
                 nodesize=50)

# creating a matrix of importance measure
imp<-importance(rf,type=2)

# creating a dataframe for keeping importance measures
d_imp<-as.data.frame(matrix(nrow = nrow(imp),ncol = 2))
colnames(d_imp)<-c("var","gini")

# filling with data
d_imp$var<-rownames(imp)
d_imp$gini<-as.vector(imp)

# sorting by importance
sorted_imp<-d_imp[order(-d_imp$gini),]

# writing to the csv file
write.csv(sorted_imp,
          "F:\\Ryerson\\CKME136\\Porto Seguro\\var_rand_forest_importance.csv")

# reading importance dataframe
sorted_imp<-read.csv("F:\\Ryerson\\CKME136\\Porto Seguro\\var_rand_forest_importance.csv")

# selecting 99 the most important columns
rand_forest_selected_col<-sorted_imp$var[2:100]
rand_forest_selected_col<-c("id","target",rand_forest_selected_col)

# subsetting a dataframe
train<-train_low_var[,rand_forest_selected_col]

# writing to the csv file
write.csv(train,
          "F:\\Ryerson\\CKME136\\Porto Seguro\\train_rand_forest_selected.csv")

# reading from csv
train<-read.csv("F:\\Ryerson\\CKME136\\Porto Seguro\\train_rand_forest_selected.csv")
test<-read.csv("F:\\Ryerson\\CKME136\\Porto Seguro\\test_low_var.csv")

# creating the same set of feutures for testing data
rand_forest_selected_col<-sorted_imp$var[2:100]
rand_forest_selected_col<-c("id",rand_forest_selected_col)

# writing to the csv file
write.csv(test,
          "F:\\Ryerson\\CKME136\\Porto Seguro\\test_rand_forest_selected.csv")
