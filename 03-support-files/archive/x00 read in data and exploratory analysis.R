##################################################
###read in and check dataset 
##################################################
#read in dataset
infews = read.csv("./01-data/INFEWS Sample Sheet 16DEC2022- Sheet1.csv")
#check out data structure
str(infews)
#get unique values for a particular field to see what might be going on
unique(infews$pH)
unique(infews$Lead)

##################################################
###exploratory data analysis
##################################################
#generate box plot
boxplot(infews$pH,main="pH")
boxplot(infews$Total_Carbon)
boxplot(infews$Olsen_P)
boxplot(infews$Lead)
boxplot(infews$Bulk_Density)
#generate scatterplot
plot(infews$Total_Carbon, infews$Bulk_Density)
## why are there so many around 7% carbon that have weird bulk densities?

plot(infews$Total_Carbon, infews$LOI_OM, ylim = c(0,20))
plot(infews$Top, infews$Total_Carbon)
plot(infews$Top, infews$pH)
plot(infews$Top, infews$Olsen_P)

par(mfrow = c(1,4))
plot(infews$Olsen_P, infews$Top, ylim = c(100,0))
plot(infews$Total_Carbon, infews$Top, ylim = c(100,0))
plot(infews$pH, infews$Top, ylim = c(100,0))
plot(infews$Lead, infews$Top, ylim = c(100,0))

check <- subset(infews, infews$Bulk_Density > 3)
check2 <- subset(infews, infews$Bulk_Density > 3 & infews$Total_Carbon > 7)
check3 <- subset(infews, infews$Total_Carbon > 6.5 & infews$Total_Carbon < 8.5)
unique (check3$Year)
check3$Year
