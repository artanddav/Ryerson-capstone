# Custom function for outliers treatment
impute_out <- function(df){
  for(cl in 1:ncol(df)){
    q <- quantile(df[[cl]],c(0.05,0.95))
    df[df[[cl]]<q[1],cl]<-q[1]
    df[df[[cl]]>q[2],cl]<-q[2]
  }
  return(df)
}

# Loading dataset
train=read.csv("F:\\Ryerson\\CKME136\\Porto Seguro\\train.csv")

# Numerical part
train_num<-train[,c(grep("reg",colnames(train)),
                               grep("car_\\d{2}$",colnames(train)),
                               grep("calc_\\d{2}$",colnames(train)))]
# Non numerical part
train_non_num<-train[,!(names(train) %in% colnames(train_num))]

# Imputing
tr_out_imputed<-impute_out(train_num)

# Rejoining
tr_out_imputed<-cbind(train_non_num,tr_out_imputed)

# Saving
write.csv(tr_out_imputed,"F:\\Ryerson\\CKME136\\Porto Seguro\\tr_out_imputed.csv")
