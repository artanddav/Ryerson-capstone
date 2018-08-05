library("ggplot2")
library("Matrix")

# reading the data
train<-read.csv("F:\\Ryerson\\CKME136\\Porto Seguro\\train_mean_median_imputed.csv")

# subsetting binary columns + target column and creating sparse matrix
bin<-Matrix(as.matrix(train[,c(grep("bin",colnames(train)),
              grep("target",colnames(train)))]), sparse=TRUE)

# number of rows and columns
n_row<-nrow(bin)
n_col<-ncol(bin)

# probability of of having target = 1
p_tar_1<-length(bin[bin[,"target"]==1,"target"])/n_row

for(i in 1:(n_col-1)){
  
  # number of binary 0 cases
  n_bin_0<-length(bin[bin[,i]==0,i])
  
  # number of binary 1 cases
  n_bin_1<-length(bin[bin[,i]==1,i])
  
  # probability of having target = 1 and binary 0 cases
  p_tar_1_bin_0<-length(bin[bin[,i]==0&bin[,"target"]==1,i])/length(bin[bin[,i]==0,i])
  
  # probability of having target = 1 in binary 1 cases
  p_tar_1_bin_1<-length(bin[bin[,i]==1&bin[,"target"]==1,i])/length(bin[bin[,i]==1,i])
  
  # standard error for binary 0 cases
  SE_bin_0<-sqrt((p_tar_1*(1-p_tar_1)/n_row)+(p_tar_1_bin_0*(1-p_tar_1_bin_0)/n_bin_0))
  
  # standard error for binary 0 cases
  SE_bin_1<-sqrt((p_tar_1*(1-p_tar_1)/n_row)+(p_tar_1_bin_1*(1-p_tar_1_bin_1)/n_bin_1))
  
  # point estimator for binary 0 cases
  point_est_bin_0<-abs(p_tar_1 - p_tar_1_bin_0)
  
  # point estimator for binary 1 cases
  point_est_bin_1<-abs(p_tar_1 - p_tar_1_bin_1)
  
  # margin of error for binary 0 cases
  margin_bin_0<-1.96*SE_bin_0
  
  # margin of error for binary 1 cases
  margin_bin_1<-1.96*SE_bin_1
  
  # lower boundary for binary 0 cases
  lower_bin_0<-point_est_bin_0 - margin_bin_0
  
  # lower boundary for binary 1 cases
  lower_bin_1<-point_est_bin_1 - margin_bin_1
  
  # upper boundary for binary 0 cases
  upper_bin_0<-point_est_bin_0 + margin_bin_0
  
  # upper boundary for binary 1 cases
  upper_bin_1<-point_est_bin_1 + margin_bin_1
  
  # indicator for probability 
  prob_bin_0<-ifelse(p_tar_1_bin_0>p_tar_1,"higher","lower")
  prob_bin_1<-ifelse(p_tar_1_bin_1>p_tar_1,"higher","lower")
  
  # testing null hypothesis for binary 0 and 1 cases
  print(cat(colnames(bin)[i],
            paste("- binary 0 case, 95% of confidence - null hypothesis rejected = ",
              !(lower_bin_0<0 & upper_bin_0>0),
              ",  probability of target = 1 is ",
              prob_bin_0),
            paste("- binary 1 case, 95% of confidence - null hypothesis rejected = ",
              !(lower_bin_1<0 & upper_bin_1>0),
              ",  probability of target = 1 is ",
              prob_bin_1),
              sep = "\n"))
}
