library("ggplot2")

# reading the data
train=read.csv("F:\\Ryerson\\CKME136\\Porto Seguro\\train.csv")

# selecting binary and target columns
train_bin<-train[,c(grep("bin",colnames(train)),
                    c(grep("target",colnames(train))))]

# custom function for drawing barplots and saving it
save_bars<-function(df,x){
  ggplot(data = df,aes(x=df[[x]],fill=factor(df$target)))+
    geom_bar()+
    labs(x=colnames(df)[x],y="Frequency",fill = "Target")+ 
    scale_fill_manual(values=rep(c("red", "green", "blue","orange","darkgrey","purple","deepskyblue3","brown4","chartreuse3"),20))
  ggsave(paste("F:\\Ryerson\\CKME136\\Porto Seguro\\plots\\bin\\tr_",colnames(df)[x],".jpeg"))
}

# applying the funcion on all binary and target columns
df<-train_bin
for(col in 1:ncol(df)){
  save_bars(df,col)
}


# Custom function for barplot creation using vector of levels and 
# the frequency number of each level
barplot_create<-function(cl,val,col_name){
  x=character()
  for(i in 1:length(cl)){
    x<-append(x,rep(cl[i],val[i]))
  }
  
  df<-data.frame(x)
  
  ggplot(data = df,aes(x=df$x,fill=df$x))+
    geom_bar()+
    labs(x=col_name,y="Frequency",fill = "Levels")+ 
    scale_fill_manual(values=rep(c("red", "green", "blue","orange","darkgrey","purple","deepskyblue3","brown4","chartreuse3"),20))
  
}

# function implementation for ps_ind_08_bin column
cl<-as.character(c(0,	1))
val<-c(497644,	97568)
barplot_create(cl,val,"ps_ind_08_bin")
  
# ================  Outliers ================== #

# reading the data
train=read.csv("F:\\Ryerson\\CKME136\\Porto Seguro\\train.csv")

# numerical part of the data
train_num<-train[,c(grep("reg",colnames(train)),
                         grep("car_\\d{2}$",colnames(train)),
                         grep("calc_\\d{2}$",colnames(train)))]

# creating a dataframe for boxplot
bxplt<-data.frame(matrix(nrow = nrow(train_num)*ncol(train_num),
                        ncol = 2))

# seting up colnames
colnames(bxplt)<-c("attr","value")

# filling with values
for(col in 21:24){
  df<-data.frame("attr"=colnames(train_num)[col],
                 "value"=train_num[[col]])
  bxplt<-rbind(bxplt,df)
}

# creating a boxplot
ggplot(data = bxplt, aes(x=bxplt$attr,y=bxplt$value, fill=bxplt$attr))+
  geom_boxplot(outlier.color = "red")+
  coord_cartesian(ylim = c(-0.5, 25))+
  labs(x="",y="",fill = "Attributes")+
  theme(axis.text.x = element_blank())+
  scale_fill_manual(values=c("red", "green", "blue","orange",
                             "darkgrey","purple","deepskyblue3",
                             "brown4","chartreuse3","red", "green", "blue","orange",
                             "darkgrey","purple","deepskyblue3",
                             "brown4","chartreuse3","red", "green", "blue","orange",
                             "darkgrey","purple","deepskyblue3",
                             "brown4","chartreuse3","red"))

# ============================================================ #
# data from models tuning
lgbm<-c(0.6272,	0.6272,	0.6273,0.6292,	0.6274,	0.6275,	0.621,	0.621,
        0.6285,	0.6278,0.6201,	0.6287,	0.5869,	0.6283,	0.6245,
        0.6244,	0.6238,	0.6241,0.6226,0.6292,	0.6227,	0.6226,	0.624,
        0.6237,	0.6236,	0.6243,	0.6242,0.624,	0.6242,	0.6242,
        0.6241,	0.6196,	0.6131,	0.5691,	0.6239,0.6241,	0.6222,
        0.6222,	0.6236,	0.6237,0.6292)
cbst<-c(0.6289,	0.6282,	0.6289,	0.6281,	0.6285,	0.6288,	0.6285,
        0.624901682,	0.624827425,0.6281,	0.624870057,0.6282,	0.6283,
        0.6273,	0.6284,0.6283,	0.6283,	0.6281,	0.6284,	0.6280,	0.6285,	0.6284,
        0.6249,	0.6248,0.6280,	0.6251,0.6282,	0.6283,
        0.6273,	0.6284)
xgb<-c(0.5479,	0.557,	0.6099,	0.6133,	0.622,	0.5398,	0.5398,
       0.6125,	0.6125,0.6125,	0.612,	0.6125,	0.6135,	0.6139,
       0.6124,	0.6124,	0.6135,	0.6138,0.6056,	0.6056,	0.6056,
       0.6056,	0.606,	0.6054,	0.6068,	0.6076,0.6076,	0.6098,
       0.6119,	0.6124,0.6119,	0.6109,	0.6123)

# dataframe for models data
d1<-data.frame(value=lgbm)
d1["model"]<-"LightGBM"
d2<-data.frame(value=cbst)
d2["model"]<-"CatBoost"
d3<-data.frame(value=xgb)
d3["model"]<-"XGBoost"
d<-rbind(d1,d2)
d<-rbind(d,d3)     

 #plotting
ggplot(data=d,aes(x=1:nrow(d),y=d$value,color=factor(d$model)))+
  geom_point()+
  scale_color_manual(values=c("red", "green", "blue"))+
  labs(x="", y="AUC",color="Model")
  


