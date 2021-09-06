#-----------Panel Reg--------------------#
setwd("C:/protein")
set.seed(123)

#---------------Libraries requires (make sure to install these before begin)---------------#
library(plm)
library(mice)
library(scatterplot3d)
library(plot3D)
library(png)
library(tidyverse)

per_urban<-31.16

#-------------data import---------------------------
data_res<-as.data.frame(read.csv("data_res.csv", header = T)) #importing data of residential area
data_ind<-as.data.frame(read.csv("data_ind.csv", header = T)) #importing data of industrial area
area<-"residentia"

#data<-data[-which(data$states==""),] # getting rid of blank entries
data_res[,3:ncol(data_res)]<-apply(data_res[,3:ncol(data_res)], 2, as.numeric) # converting to numeric type
data_ind[,3:ncol(data_ind)]<-apply(data_ind[,3:ncol(data_ind)], 2, as.numeric)
#data[which(data==0,arr.ind = T)]<-NA 

my_data_res<-data_res
my_data_ind<-data_ind

#---------------STATE FILTERING-----------------------

mat1<-matrix(my_data_res$X..URBAN, nrow=length(2004:2014))
state1<-unique(my_data_res$STATE.UT)[which(colMeans(mat1) >= per_urban)] #urban

mat2<-matrix(my_data_res$POPULATION, nrow=length(2004:2014))
state2<-unique(my_data_res$STATE.UT)[which(colMeans(mat2) >=10^7)] #population strength

state<-intersect(state1, state2) # final states to be analyzed
missing<-table(my_data_res$STATE.UT[which(is.na(my_data_res$NO2), arr.ind = T)])
miss_state<-names(missing[which(missing > 3)])
state<-state[!state%in%miss_state]



index<-NULL
for(i in 1:length(state)){
  index<-c(index, which(my_data_res$STATE.UT == state[i]))
}

my_data_res<-my_data_res[index,] #the final data to be analyzed
my_data_ind<-my_data_ind[index,] #the final data to be analyzed


#-------------missing value imputation-----------------------
data1<-mice(my_data_res[,3:ncol(my_data_res)], 5, method = "cart") 
data1<-complete(data1,1)
my_data_res[,3:ncol(my_data_res)]<-data1

data2<-mice(my_data_ind[,3:ncol(my_data_ind)], 5, method = "cart") 
data2<-complete(data2,1)
my_data_ind[,3:ncol(my_data_ind)]<-data2




plot_data_ind<-as.matrix(cbind(as.numeric(my_data_ind$SO2), 
                 as.numeric(my_data_ind$GDP.PER.CAPITA)))
plot_data_ind<-plot_data_ind[order(plot_data_ind[,2], decreasing = F),]

plot_data_res<-as.matrix(cbind(as.numeric(my_data_res$SO2), 
                               as.numeric(my_data_res$GDP.PER.CAPITA)))
plot_data_res<-plot_data_res[order(plot_data_res[,2], decreasing = F),]


par(mfrow=c(2,4))
par(bg="grey85")
for(i in 1:7){ 
  
  xaxis_ind<-plot_data_ind[,2][((i-1)*11+1):(11*i)]
  yaxis_ind<-plot_data_ind[,1][((i-1)*11+1):(11*i)]
  xaxis_res<-plot_data_res[,2][((i-1)*11+1):(11*i)]
  yaxis_res<-plot_data_res[,1][((i-1)*11+1):(11*i)]
  
  plot(NA, xlab="log GDP/Capita ",
       main = state[i],  
       ylab="SO2 level", font.axis=2,
       type = "p", col="red", pch=19,
       font.lab=2,
       xlim=c(min(log(xaxis_ind),log(xaxis_res)), 
              max(log(xaxis_ind),log(xaxis_res))),
       ylim=c(min(yaxis_ind,yaxis_res), max(yaxis_ind,yaxis_res)))
  
  grid( col = "black")
  points(log(xaxis_ind), yaxis_ind, pch=19, col="red")
  points(log(xaxis_res), yaxis_res, pch=19, col="blue")
  
  lines(lowess(log(xaxis_ind), yaxis_ind), lwd=2, col="red")
  lines(lowess(log(xaxis_res), yaxis_res), lwd=2, col="blue")
  
  box(lwd= 2)
  
}





plot_data_ind<-as.matrix(cbind(as.numeric(my_data_ind$NO2), 
                               as.numeric(my_data_ind$GDP.PER.CAPITA)))
plot_data_ind<-plot_data_ind[order(plot_data_ind[,2], decreasing = F),]

plot_data_res<-as.matrix(cbind(as.numeric(my_data_res$NO2), 
                               as.numeric(my_data_res$GDP.PER.CAPITA)))
plot_data_res<-plot_data_res[order(plot_data_res[,2], decreasing = F),]

par(bg="khaki")
par(mfrow=c(2,4))

for(i in 1:7){ 
  
  xaxis_ind<-plot_data_ind[,2][((i-1)*11+1):(11*i)]
  yaxis_ind<-plot_data_ind[,1][((i-1)*11+1):(11*i)]
  xaxis_res<-plot_data_res[,2][((i-1)*11+1):(11*i)]
  yaxis_res<-plot_data_res[,1][((i-1)*11+1):(11*i)]
  
  plot(NA, xlab="log GDP/Capita ",
       main = state[i],  
       ylab="NO2 level", font.axis=2,
       type = "p", col="red", pch=19,
       font.lab=2,
       xlim=c(min(log(xaxis_ind),log(xaxis_res)), 
              max(log(xaxis_ind),log(xaxis_res))),
       ylim=c(min(yaxis_ind,yaxis_res), max(yaxis_ind,yaxis_res)))
  
  grid( col = "black")
  points(log(xaxis_ind), yaxis_ind, pch=19, col="red")
  points(log(xaxis_res), yaxis_res, pch=19, col="blue")
  
  lines(lowess(log(xaxis_ind), yaxis_ind), lwd=2, col="red")
  lines(lowess(log(xaxis_res), yaxis_res), lwd=2, col="blue")
  
  box(lwd= 2)
  
}


