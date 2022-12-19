## Session 1, basics of coding, Nick's cohort
## 07/12/2022
##Can be combined with powerpoint and excel sheets for a general overview of R. All materials kept with Nick Edmondson

#----Getting help----------------------------------------------------
help(mean)
?(mean)

help.search("mean")
??"mean"


#----Basics----------------------------------------------------------
#Use R as a calculator
25/5 * 100
pi
pi^3

#Creating objects
Name<-"James"
Name
Name="James R"
Name
x = 1.25
x * pi


#----Install and load packages---------------------------------------
#Install from CRAN
install.packages("dpylr", type="binary")
library("dpylr")

#Install from bioconductor
source("https://bioconductor.org/biocLite.R")
biocLite("GenomicFeatures")

#Check what GenomicFeatures does on Google as demonstration of documentation - can search any package for a similar summary https://bioconductor.org/packages/release/bioc/html/GenomicFeatures.html


#----Loading data into R---------------------------------------------
#Define working directory
setwd("C:/Users/lby76f/OneDrive - Ministry of Justice/Personal")
all_rounders<-read.csv("All_rounders.csv", header=TRUE)
all_rounders<-read.csv("All_rounders.csv", header=TRUE, row.names = 1)


#----Data types------------------------------------------------------
##Vector
#Create vector x
x<-c(1,6,7,10,12)
#Interegate vector
typeof(x)
length(x)

##Matrix
t<-matrix(c(1,6,7,10,12), nrow = 3, ncol = 2)
t
words<-matrix(c("Green", "Blue", "Red", "Brown"), nrow = 3, ncol = 5)
words
#Interegate matrix
t[2,1]
t[3,]
t[,2]

##List - can be a combination of anything
firstlist<-list("House", "Flat", "Garden")
firstlist
#Interegate list
firstlist[2]

Everything<-list(t, words, firstlist)
Everything

##Dataframe - best way to address data
Big_cat <- data.frame(
  Animal = c("Lion", "Jaguar", "Leopard", "Tiger", "Cheetah"),
  Height = c(120,76,70,110,94),
  Weight = c(190,96,27,310,72),
  Speed = c(80, 80, 58, 65, 130)
)
Big_cat
summary(Big_cat)
#Interegate dataframe
Big_cat[2]
Big_cat[["Height"]]
Big_cat$Height

#----Creating own dataframe------------------------------------------
id <- c(1:26)
name <- c('Ava',	'Ben',	'Charlotte',	'Daniel',	'Emma',	'Finn',	'Grace',	'Henry',	
          'Isabel',	'James',	'Kelly',	'Liam',	'Melissa',	'Ned',	'Olivia',	'Phillip',	
          'Quinn',	'Ryan',	'Sophie',	'Theo',	'Umar',	'Victoria',	'William',	'Xavier',	
          'Yusuf',	'Zoe')
height <- c(150, 142, 162, 160, 168, 181, 161, 141, 175, 176, 140, 150, 155, 180, 195,
            163, 171, 174, 451, 145, 153, 151, 193, 155, 159, 148)
weight <- c(73, 51, 78, 76, 68, 64, 60, 71, 56, 73, 63, 68, 72, 76, 67, 68, 68, 69,
            72, 58, 71, 60, 75, 56, 50, 71)
colour <- c('Blue',	'Red',	'Red',	'Blue',	'Yellow',	'Yellow',	'Red',	'Yellow',	'Green',	
            'Red',	'Yellow',	'Red',	'Green',	'Blue',	'Red',	'Blue',	'Yellow',	'Green',	
            'Red',	'Blue',	'Yellow',	'Red',	'Red',	'Green',	'Rod',	'Yellow')
animal <- c('Dog',	'Cat',	'Dog',	'Cat',	NA,	'Dog',	'Cat',	'Dog',	NA,	'Dog',	
            'Dog',	'Dog',	NA,	NA,	NA,	'Dog',	'Dog',	NA,	NA,	NA,	'Dog',	'Dog',	
            'Dog',	'Dog',	'Cat',	'Cat')
rating <- c(7, 10, 7,	10, 3, 3,	5, 1, 6, 4,	4, 2, 2, 1, 8, 5, 10, 1, 2,	5, 3,	6, 5,	2, 
            8,	7)

df <- data.frame(id, name, height, weight, colour, animal, rating)
df


#----if else statements---------------------------------------------------------
x<-"34"

if (x < 0) {
  print("Negative number")
}

if (x < 0) {
  print("Negative number")
} else {("Positive number")
}

#Give everyone over 180cm tall a hard hat, give everyone else a ladder
df$hat <- if(df$height > 180) {("Hard hat")
} else {("Ladder")
}
df
#Correct way of giving everyone a hard hat
df$hat <- ifelse(df$height > 180,'Hard hat', 'Ladder')
df

#The if_else ladder (nested if else)
if (is.character(x) == TRUE) {
  print("Should those speech marks be there?")
} else if (x < 0) {
  print("Negative number")
} else if (x > 0) {
  print("Positive number")
} else if (x == 0) {
  print("Zero")
} else print ("You've broken me")


#----for loops-------------------------------------------------------
#Basic loop as demonstration
for (year in 2015:2022) {
  print(paste("The year is", year))
}
#for loop can use any word as long as it is in the loop

for (elephant in 2015:2022) {
  print(paste("The year is", elephant))
}

#for loop using our data and nested for loops. Finding the median for each column
#Could do
median(all_rounders$HS)
median(all_rounders$Runs)

#Easier if we do a for loop
output <- vector()
for (i in 1:nrow(all_rounders)) {
  output[[i]] <- median(all_rounders[[i]])
}
output


#----Creating Functions----------------------------------------------
#Functions to convert pounds to dollars, pounds to euros, pounds to australian dollars, and pounds to Costa Rican Colon
Dollar<-function(pounds) {
  dollar<-pounds*1.22
  return(dollar)
}
Dollar(20)

Euro<-function(pounds) {
  euro<-pounds*1.16
  return(paste('£',euro))
}
Euro(20)

Aus<-function(pounds) {
  aus<-pounds*1.81
  return(aus)
}

Costa<-function(pounds) {
  col<-pounds*735.20
  return  (col)
}

#Combining functions and if else statements to create a working currency calculator
currency<-function(pounds,conv) {
  if (conv == "dollar") {
    money<-Dollar(pounds)
  } else if (conv == "euro") {
    money<-Euro(pounds)
  } else if (conv == "aus") {
    money<-Aus(pounds)
  } else if (conv == "Colones") {
    money<-Costa(pounds)
  } else {money<-"Incorrect entry"}
  return(money)
}
currency(50, "euro")
currency(70, "Colones")
currency(50, "Yen")


#----Stretching R------------------------------------
#Can all agree the above if pretty boring. To show greater ability of R some k-means clustering (unsupervised machine learning) with all_rounder dataset
install.packages(c("factoextra", "tidyverse", "cluster", "cowplot"))
library(factoextra)
library(tidyverse)
library(cluster)
library(cowplot)
#K means clustering
p1<-fviz_nbclust(all_rounders, kmeans, method = "silhouette")
p2<-fviz_nbclust(all_rounders, kmeans, method = "wss")
gap_stat<-clusGap(all_rounders, FUN = kmeans, nstart = 25, K.max = 10, B = 50)
p3<-fviz_gap_stat(gap_stat)
cluster<-kmeans(all_rounders, 2, nstart = 25)
p4<-fviz_cluster(cluster, data = all_rounders, labelsize = 8, main = "Test all rounders", xlab = "PCA1", ylab = "PCA2", ggtheme = theme_bw())
top_row<-plot_grid(p1, p2, p3, labels = c("A", "B", "C"), label_size = 12, nrow = 1)
bottom<-plot_grid(p4, labels = c("D"), label_size = 12)
plot_grid(top_row, bottom, nrow = 2)