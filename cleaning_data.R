housing.dataset <- read.csv("melbourne_data.csv")

housing.dataset[,2]=as.Date(housing.dataset[,2],"%d/%m/%y")
housing.dataset[,13]=as.double(housing.dataset[,13])
housing.dataset[,11]=as.double(housing.dataset[,11])
str(housing.dataset)
is.na(housing.dataset)

####################REMOVENA#######################
###################################################
summary(housing.dataset$YearBuilt)
summary(housing.dataset$BuildingArea)
removenacolumn <- housing.dataset
removenacolumn$YearBuilt <- NULL
removenacolumn$BuildingArea <- NULL
removenacolumn

removena <- na.omit(removenacolumn)
is.na(removena)
summary(removena)
###################################################
###################################################


#################OUTLIERS##########################
###################################################
#pairs(removena)####pas ouf, peu visible
###################################################VISUALISATION DES OUTLIERS#################
library(ggplot2)
ggplot(removena, aes(x=removena$Type,y=removena$Price)) + 
  geom_boxplot() + xlab(label = "Type") + 
  ylab(label = "Price") + 
  theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1))+
  theme(legend.position="none")+
  ggtitle("Boxplot Price with type")
###################################################
########VALEURS OUTLIERS POUR LE PRIX##############
outliers_val <- boxplot.stats(removena$Price)$out
outliers_val
######################Outliers index#######################
outlier_idx <-which(removena$Price %in% c(outliers_val))
outlier_idx
########################TABLE DES OUTLIERS#######################
removena[outlier_idx,]
###########Voir les outliers dans les boxplots###################
ggplot(removena, aes( x=removena$Type,y=removena$Price, fill=removena$Type, colour=removena$Type)) +
  geom_jitter(width=0.25)+ 
  geom_boxplot(alpha=0.5, outlier.shape=NA)+    
  xlab(label = "Type") +
  ylab(label = "Price") +
  theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1))+
  theme(legend.position="none")+
  theme_classic()+
  ggtitle("Boxplot with Types")
#########Test retirer les outliers du dataset#####################
removeoutliers <- subset(removena, removena$Price < 5000000)
removeoutliers <- subset(removena, removena$Price < 3000000)

ggplot(removeoutliers, aes( x=removeoutliers$Type,y=removeoutliers$Price, fill=removeoutliers$Type, colour=removeoutliers$Type)) +
  geom_jitter(width=0.25)+ 
  geom_boxplot(alpha=0.5, outlier.shape=NA)+    
  xlab(label = "Type") +
  ylab(label = "Price") +
  theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1))+
  theme(legend.position="none")+
  theme_classic()+
  ggtitle("Boxplot with Types")
#########################################################################################################




##########################################################
######################QUESTION 2##########################
summary(removeoutliers)
##########################################################
hist(removeoutliers$Car, main = "Histogram", xlab = "Cars")
###########PLOT###########################################
plot(removeoutliers$Distance,removeoutliers$Price, main = "Link between Price and landsize")
###########PIE############################################
mytable <- table(removeoutliers$Type)
lbls <- paste(names(mytable), "\n", mytable, sep="")
pie(mytable, labels = lbls, col=rainbow(length(lbls)), main="Pie Chart of Types")
###########Bar chat######################################
counts <- table(removeoutliers$Rooms)
barplot(counts, main="House Distribution", xlab="Number of Bathrooms", ylab = "House's frequency")
#########################################################


#########################################################
################Question 3###############################
hist(removeoutliers$Price)
mean(removeoutliers$Price)
var(removeoutliers$Price)
median(removeoutliers$Price)
#########################################################
summary(removeoutliers)
limitlow <- quantile(removeoutliers$Price,probs = 0.33)
limithigh <- quantile(removeoutliers$Price,probs = 0.66)

low <- subset(removeoutliers$Price, removeoutliers$Price < limitlow)
medium <- subset(removeoutliers$Price, removeoutliers$Price >= limitlow & removeoutliers$Price <= limithigh)
high <- subset(removeoutliers$Price, removeoutliers$Price > limithigh)

summary(low)
summary(medium)
summary(high)
########################################################
ggplot(removeoutliers, aes( x=removeoutliers$Type,y=removeoutliers$Price, fill=removeoutliers$Type, colour=removeoutliers$Type)) +
  #geom_jitter(width=0.25)+ 
  geom_boxplot(alpha=0.5, outlier.shape=NA)+    
  xlab(label = "Type") +
  ylab(label = "Price") +
  theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1))+
  theme(legend.position="none")+
  theme_classic()+
  ggtitle("Boxplot with Types")
#########################################################

correlation.table <- removeoutliers
correlation.table$X <- NULL
correlation.table$Date <- NULL
correlation.table$Regionname <- NULL
correlation.table$Type <- NULL

cor(correlation.table)
#########################################################
#########################################################
removeoutlierlandsize <- subset(removeoutliers, removeoutliers$Landsize<25000)
qplot(removeoutliers$Type, xlab = "Type", ylab="Frequency")
qplot(removeoutlierlandsize$Landsize, removeoutlierlandsize$Price, colour = removeoutlierlandsize$Type)
qplot(removeoutlierlandsize$Type, removeoutlierlandsize$Price, colour = removeoutlierlandsize$Landsize)
