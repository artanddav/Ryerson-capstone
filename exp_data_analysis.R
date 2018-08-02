library("ggplot2")

# loading data
train=read.csv("F:\\Ryerson\\CKME136\\Porto Seguro\\tr_out_imputed.csv")

# get rid of unnecessary columns
train<-train[,!(names(train) %in% c("X","id"))]

# target = 0 cases
target_0<-train[train$target==0,]

# target = 1 cases
target_1<-train[train$target==1,]

# numeric part target = 0 cases
target_0_num<-target_0[,c(grep("reg",colnames(target_0)),
                    grep("car_\\d{2}$",colnames(target_0)),
                    grep("calc_\\d{2}$",colnames(target_0)))]
# non_numeric part target = 0 cases
target_0_non_numeric<-target_0[,!(names(target_0) %in% colnames(target_0_num))]

# numeric part target = 1 cases
target_1_num<-target_1[,c(grep("reg",colnames(target_1)),
                          grep("car_\\d{2}$",colnames(target_1)),
                          grep("calc_\\d{2}$",colnames(target_1)))]
# non_numeric part target = 1 cases
target_1_non_numeric<-target_1[,!(names(target_1) %in% colnames(target_1_num))]

# scale numerical part
target_0_num_scaled<-scale(target_0_num)
target_1_num_scaled<-scale(target_1_num)

# number of Within SS
wss_target_0<-(nrow(target_0_num_scaled)-1)*sum(apply(target_0_num_scaled,2,var))
for(i in 2:15){
  wss_target_0[i]<-sum(kmeans(target_0_num_scaled,centers = i)$withinss)
}

wss_target_1<-(nrow(target_1_num_scaled)-1)*sum(apply(target_1_num_scaled,2,var))
for(i in 2:15){
  wss_target_1[i]<-sum(kmeans(target_1_num_scaled,centers = i)$withinss)
}

# custom function for drawing elbow method
plot_elbow<-function(wss,case){
  df=data.frame(x=1:15,y=wss)
  ggplot(data = df,aes(x=df$x,y=df$y))+
    geom_point()+
    labs(x = paste("Number of clusters for taget = ",case),y = "Within group Sum of Squares")
  
}

# drawing elbow for target = 0
plot_elbow(wss_target_0,0) #number of cluster centers 4

# drawing elbow for target = 1
plot_elbow(wss_target_1,1) #number of cluster centers 4

# kmeans clustering for target = 0
fit_target_0<-kmeans(target_0_num_scaled,4)

# kmeans clustering for target = 1
fit_target_1<-kmeans(target_1_num_scaled,4)

# get cluster means
cl_means_target_0<-aggregate(target_0_num,by=list(fit_target_0$cluster),FUN=mean)
cl_means_target_1<-aggregate(target_1_num,by=list(fit_target_1$cluster),FUN=mean)

# add column with cluster centers
with_cluster_target_0<-data.frame(target_0,fit_target_0$cluster)
with_cluster_target_1<-data.frame(target_1,fit_target_1$cluster)

# write in CSV format
write.csv(with_cluster_target_0,"F:\\Ryerson\\CKME136\\Porto Seguro\\with_cluster_target_0.csv")
write.csv(with_cluster_target_1,"F:\\Ryerson\\CKME136\\Porto Seguro\\with_cluster_target_1.csv")

# Combining
bar1<-with_cluster_target_0
bar1["target"]<-0
colnames(bar1)[59]<-"cluster"

bar2<-with_cluster_target_1
bar2["target"]<-1
colnames(bar2)[59]<-"cluster"

bar<-rbind(bar1,bar2)

# write in CSV
write.csv(bar,"F:\\Ryerson\\CKME136\\Porto Seguro\\with_cluster_target_0_1.csv" )

# read file
bar<-read.csv("F:\\Ryerson\\CKME136\\Porto Seguro\\with_cluster_target_0_1.csv")

# barplot
ggplot(data = bar,aes(x=bar$cluster,
                      y=bar$ps_ind_15,fill=factor(bar$target)))+
  geom_bar(position="dodge",stat = "identity")+
  labs(x="Clusters",y="Attribute",fill="Status")
