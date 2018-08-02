library("ggplot2")
library("reshape2")

# reading the data
train<-read.csv("F:\\Ryerson\\CKME136\\Porto Seguro\\train_balanced.csv")

#selecting numerical part
train_num<-train[,c(grep("reg",colnames(train)),
                    grep("car_\\d{2}$",colnames(train)),
                    grep("calc_\\d{2}$",colnames(train)))]

# calculating correlation matrix
cor_matrix<-round(cor(train_num),2)

# converting to the row
cor_row<-melt(cor_matrix)

# making upper part from diagonal blank
upper_half<-cor_matrix
upper_half[upper.tri(upper_half)] <- NA

# drawing the plot
ggplot(data = melt(upper_half, na.rm = T), aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue",
                       high = "red",
                       mid = "white",
                       midpoint = 0)+
  theme(axis.text.x = element_text(angle = 45,
                                   vjust = 1, 
                                   size = 12,
                                   hjust = 1))+
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4)+
  labs(x="",y="", fill = "Pearson\nCorrelation")


