library("ggplot2")

# reading the data
#train=read.csv("F:\\Ryerson\\CKME136\\Porto Seguro\\train.csv")
train_no_na=read.csv("F:\\Ryerson\\CKME136\\Porto Seguro\\train_no_na.csv")
train<-train_no_na
#train<-read.csv("F:\\Ryerson\\CKME136\\Porto Seguro\\train_balanced.csv")
#================== Data Preparation ======================#

# subsetting target = 0 and 1 cases
train_filed<-train[train$target==1,-which(names(train) %in% c("target","id","X"))]
train_not_filed<-train[!rownames(train) %in% rownames(train_filed),-which(names(train) %in% c("target","id","X"))]

# selecting numerical part for target = 1
train_filed_num<-train_filed[,c(grep("reg",colnames(train_filed)),
                          grep("car_\\d{2}$",colnames(train_filed)),
                          grep("calc_\\d{2}$",colnames(train_filed)))]

# selecting numerical part for target = 0
train_not_filed_num<-train_not_filed[,c(grep("reg",colnames(train_not_filed)),
                                    grep("car_\\d{2}$",colnames(train_not_filed)),
                                    grep("calc_\\d{2}$",colnames(train_not_filed)))]

# selecting non numerical part
train_filed_cat<-train_filed[,!(names(train_filed) %in% colnames(train_filed_num))]
train_not_filed_cat<-train_not_filed[,!(names(train_not_filed) %in% colnames(train_not_filed_num))]

# scaling the data
sc_train_filed_num<-scale(train_filed_num)
sc_train_not_filed_num<-scale(train_not_filed_num)

# implementing kmeans clustering algorithm
# within SS for target = 1
wss_train_filed_num<-(nrow(sc_train_filed_num)-1)*sum(apply(sc_train_filed_num,2,var))
for(i in 2:15){
  wss_train_filed_num[i]<-sum(kmeans(sc_train_filed_num,centers = i)$withinss)
}

# custom function for plotting elbow  method
plot_elbow<-function(wss,case){
  df=data.frame(x=1:15,y=wss)
  ggplot(data = df,aes(x=df$x,y=df$y))+
    geom_point()+
    labs(x = paste("Number of clusters for taget = ",case),y = "Within group Sum of Squares")
  
}

plot_elbow(wss_train_filed_num,1)

# chosen cluster number 4
fit<-kmeans(sc_train_filed_num,4)

# getting cluster means
cl_means_filed_num<-aggregate(train_filed_num,by=list(fit$cluster),FUN=mean)

# adding a column to the dataset with cluster numbers
train_filed_cluster<-data.frame(train_filed,fit$cluster)

# writing to the CSV file
write.table(cl_means_filed_num, 
            "F:\\Ryerson\\CKME136\\Porto Seguro\\cl_means_filed_num.csv", append = FALSE, sep = ",", dec = ".",
            row.names = FALSE, col.names = TRUE)

# writing to the CSV file
write.table(train_filed_cluster, 
            "F:\\Ryerson\\CKME136\\Porto Seguro\\train_filed_cluster.csv", append = FALSE, sep = ",", dec = ".",
            row.names = FALSE, col.names = TRUE)

# implementing weights applied the cluster centers
cl_means_filed_weighted<-read.csv("F:\\Ryerson\\CKME136\\Porto Seguro\\cl_means_filed_num.csv")
train_filed_cluster<-read.csv("F:\\Ryerson\\CKME136\\Porto Seguro\\train_filed_cluster.csv")
f_fit.clusters<-train_filed_cluster$fit.cluster
f_clusters<-table(f_fit.clusters)
f_all<-nrow(train_filed_cluster)
f_weight<-vector(length = 4)
for(cl in 1:4){
  f_weight[cl]<-f_clusters[names(clusters)==cl]/f_all
  cl_means_filed_weighted[cl_means_filed_weighted$Group.1==cl,-which(names(cl_means_filed_weighted) %in% c("Group.1"))]<-cl_means_filed_weighted[cl_means_filed_weighted$Group.1==cl,-which(names(cl_means_filed_weighted) %in% c("Group.1"))]*f_weight[cl]
}

# writing to the CSV file
write.table(cl_means_filed_weighted, 
            "F:\\Ryerson\\CKME136\\Porto Seguro\\cl_means_filed_weighted.csv", append = FALSE, sep = ",", dec = ".",
            row.names = FALSE, col.names = TRUE)

# drawing a histogram of cluster centers by their freaquencies
hist(train_filed_cluster$fit.cluster)

# kmenas for target = 0 cases
# within SS for target = 0
wss_train_not_filed_num<-(nrow(sc_train_not_filed_num)-1)*sum(apply(sc_train_not_filed_num,2,var))
for(i in 2:15){
  wss_train_not_filed_num[i]<-sum(kmeans(sc_train_not_filed_num,centers = i)$withinss)
}

# plotting elbow method to get  cluster means
plot_elbow(wss_train_not_filed_num,0)

# chosen number of clusters 4
fit<-kmeans(sc_train_not_filed_num,4)

# getting cluster means
cl_means_not_filed_num<-aggregate(train_not_filed_num,by=list(fit$cluster),FUN=mean)

# drawing a histogram of cluster centers by their freaquencies
hist(train_not_filed_cluster$fit.cluster)

# dataset with cluster numbers
train_not_filed_cluster<-data.frame(train_not_filed,fit$cluster)

# writing cluster means to CSV  file
write.table(cl_means_not_filed_num, 
            "F:\\Ryerson\\CKME136\\Porto Seguro\\cl_means_not_filed_num.csv", append = FALSE, sep = ",", dec = ".",
            row.names = FALSE, col.names = TRUE)

#writing to the CSV file
write.table(train_not_filed_cluster, 
            "F:\\Ryerson\\CKME136\\Porto Seguro\\train_not_filed_cluster.csv", append = FALSE, sep = ",", dec = ".",
            row.names = FALSE, col.names = TRUE)

# calculating cluster centers multiplied by their frequencies
cl_means_not_filed_weighted<-read.csv("F:\\Ryerson\\CKME136\\Porto Seguro\\cl_means_not_filed_num.csv")
train_not_filed_cluster<-read.csv("F:\\Ryerson\\CKME136\\Porto Seguro\\train_not_filed_cluster.csv")
nf_fit.clusters<-train_not_filed_cluster$fit.cluster
nf_clusters<-table(nf_fit.clusters)# number of occurancies of clusters
nf_all<-nrow(train_not_filed_cluster)
nf_weight<-vector(length = 4)
for(cl in 1:4){
  nf_weight[cl]<-nf_clusters[names(nf_clusters)==cl]/nf_all
  cl_means_not_filed_weighted[cl_means_not_filed_weighted$Group.1==cl,-which(names(cl_means_not_filed_weighted) %in% c("Group.1"))]<-cl_means_not_filed_weighted[cl_means_not_filed_weighted$Group.1==cl,-which(names(cl_means_not_filed_weighted) %in% c("Group.1"))]*nf_weight[cl]
}

# checking
head(cl_means_not_filed_weighted$ps_ind_01)

# saving
write.table(cl_means_not_filed_weighted, 
            "F:\\Ryerson\\CKME136\\Porto Seguro\\cl_means_not_filed_weighted.csv", append = FALSE, sep = ",", dec = ".",
            row.names = FALSE, col.names = TRUE)

# drawing a histogram of cluster centers by their freaquencies
hist(train_not_filed_cluster$fit.cluster)

# comparison of attributes and threshold
thres<-data.frame(matrix(nrow =ncol(cl_means_not_filed_weighted)-1,ncol = 3 ))
colnames(thres)<-c("attr_name","bigger","threshold")
for(col in 2:ncol(cl_means_not_filed_weighted)){
  nf_col<-sort(cl_means_not_filed_weighted[,col],decreasing = T)
  f_col<-sort(cl_means_filed_weighted[,col],decreasing = T)
  if(nf_col[2]>f_col[1]){
    nf_claster<-cl_means_not_filed_weighted[cl_means_not_filed_weighted[[col]]==nf_col[2],1]
    f_claster<-cl_means_filed_weighted[cl_means_filed_weighted[[col]]==f_col[1],1]
    threshold<-(nf_col[2]/nf_weight[nf_claster]-f_col[1]/f_weight[f_claster])/2
    thres[col-1,"attr_name"]<-colnames(cl_means_not_filed_weighted)[col]
    thres[col-1,"bigger"]<-"Not Filed"
    thres[col-1,"threshold"]<-threshold
    print("Not Filed bigger than Filed")
    print("Threshold is ")
    print(threshold)
  }else  if(f_col[2]>nf_col[1]){
    nf_claster<-cl_means_not_filed_weighted[cl_means_not_filed_weighted[[col]]==nf_col[1],1]
    f_claster<-cl_means_filed_weighted[cl_means_filed_weighted[[col]]==f_col[2],1]
    threshold<-(f_col[2]/f_weight[f_claster]-nf_col[1]/nf_weight[nf_claster])/2
    thres[col-1,"attr_name"]<-colnames(cl_means_not_filed_weighted)[col]
    thres[col-1,"bigger"]<-"Filed"
    thres[col-1,"threshold"]<-threshold
    print("Filed bigger than Not Filed")
    print("Threshold is ")
    print(threshold)
  }
}
# writing threshold to the CSV file
write.table(thres, 
            "F:\\Ryerson\\CKME136\\Porto Seguro\\thres.csv", append = FALSE, sep = ",", dec = ".",
            row.names = FALSE, col.names = TRUE)

# drawing a barplot
bar1<-train_filed_cluster
bar1["status"]<-"Filed"
bar2<-train_not_filed_cluster
bar2["status"]<-"Not Filed"
colnames(bar1)
bar1<-rbind(bar1,bar2)
ggplot(data=train_filed_cluster,
       aes(x=train_filed_cluster$fit.cluster,
          fill=factor(train_filed_cluster$fit.cluster)))+
  geom_bar()+
  labs(x="Clusters for target = 1", y="Frequency", fill="Cluster")
ggplot(data=bar1,aes(x=bar1$fit.cluster,
                     y=bar1$ps_reg_03,
                     fill=bar1$status))+
  geom_bar(position="dodge",stat = "identity")

# reading the data
train_filed_cluster<-read.csv("F:\\Ryerson\\CKME136\\Porto Seguro\\train_filed_cluster.csv")
train_not_filed_cluster<-read.csv("F:\\Ryerson\\CKME136\\Porto Seguro\\train_not_filed_cluster.csv")

# Clusters barplots
ggplot(data = train_filed_cluster,aes(x=train_filed_cluster$fit.cluster))+
  geom_bar()+
  labs(x="Clusters",y="Frequency")
ggplot(data = train_not_filed_cluster,aes(x=train_not_filed_cluster$fit.cluster))+
  geom_bar()+
  labs(x="Clusters",y="Frequency")

cl_means_filed_num<-read.csv("F:\\Ryerson\\CKME136\\Porto Seguro\\cl_means_filed_num.csv")
cl_means_not_filed_num<-read.csv("F:\\Ryerson\\CKME136\\Porto Seguro\\cl_means_not_filed_num.csv")
# drawing weighted approach
bar1<-cl_means_filed_weighted#apply(cl_means_filed_num,2,sum)
bar1["status"]<-"Filed"
bar2<-cl_means_not_filed_weighted#apply(cl_means_not_filed_num,2,sum)
bar2["status"]<-"Not filed"
bar<-rbind(bar1,bar2)
ggplot(data = bar,aes(x=bar$Group.1,
                            y=bar$ps_calc_14,fill=bar$status))+
  geom_bar(position="dodge",stat = "identity")+
  labs(x="Clusters",y="ps_calc_14",fill="Status")
# drawing not weighted
bar1<-cl_means_filed_num#apply(cl_means_filed_num,2,sum)
bar1["status"]<-"Filed"
bar2<-cl_means_not_filed_num#apply(cl_means_not_filed_num,2,sum)
bar2["status"]<-"Not filed"
bar<-as.data.frame(rbind(bar1,bar2))
ggplot(data = bar,aes(x=bar$Group.1,
                      y=bar$ps_ind_14,fill=bar$status))+
  geom_bar(position="dodge",stat = "identity")+
  labs(x="Clusters",y="Attribute",fill="Status")

ggplot(data=bar,aes(x=bar$status,y=bar$ps_ind_03,fill=bar$status))+
  geom_bar(position="dodge",stat = "identity")+
  labs(x="Clusters",y="Attribute",fill="Status")
