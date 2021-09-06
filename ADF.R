
#-----------Panel Reg--------------------#
setwd("C:/protein")
set.seed(123)

#---------------Libraries requires (make sure to install these before begin)---------------#
library(plm)
library(mice)


per_urban<-31.16

#-------------data import---------------------------
data<-as.data.frame(read.csv("data_ind.csv", header = T)) #importing data
#data<-data[-which(data$states==""),] # getting rid of blank entries
data[,3:ncol(data)]<-apply(data[,3:ncol(data)], 2, as.numeric) # converting to numeric type
#data[which(data==0,arr.ind = T)]<-NA 

my_data<-data
mean(my_data$X..URBAN[!is.na(my_data$X..URBAN)])
#---------------STATE FILTERING-----------------------

mat1<-matrix(my_data$X..URBAN, nrow=length(2004:2014))
state1<-unique(my_data$STATE.UT)[which(colMeans(mat1) >= per_urban)] #urban

mat2<-matrix(my_data$POPULATION, nrow=length(2004:2014))
state2<-unique(my_data$STATE.UT)[which(colMeans(mat2) >=10^7)] #population strength

state<-intersect(state1, state2) # final states to be analyzed
missing<-table(my_data$STATE.UT[which(is.na(my_data$NO2), arr.ind = T)])
miss_state<-names(missing[which(missing > 3)])
state<-state[!state%in%miss_state]



index<-NULL
for(i in 1:length(state)){
  index<-c(index, which(my_data$STATE.UT == state[i]))
}

my_data<-my_data[index,] #the final data to be analyzed


#-------------missing value imputation-----------------------
data1<-mice(my_data[,3:ncol(my_data)], 5, method = "cart") 
data1<-complete(data1,1)
my_data[,3:ncol(my_data)]<-data1


#---------Log transformation--------------------
my_data[,3:ncol(my_data)]<-apply(my_data[,3:ncol(my_data)], 2,log)



panel_data<-pdata.frame(my_data, index = c("YEAR", "STATE.UT")) #panel data format


exo1 <- c("none" , "intercept", "trend") # method initialize
exo2 <- c("intercept" , "intercept", "trend") # method initialize

ADF<-list(NULL,NULL,NULL) # Augmented Diker Fuller test initialization
names(ADF)<-c("none (??=0, ??=0)" , "intercept (??=0, ?????0)", "trend (?????0, ?????0)") 

kpss<-list(NULL,NULL,NULL) # Augmented Diker Fuller test initialization
names(kpss)<-c("intercept (??=0, ?????0)", "intercept (??=0, ?????0)", "trend (?????0, ?????0)") 



#------------------Implemetation--------------

for(j in seq_along(exo1)){
  result1<-NULL
  result2<-NULL
  
  for(i in 3:ncol(panel_data)){
    lag<-1 #you can tweak but everything is working good with 1. Make sure you add lad=2, lag=3 results in supplimentry
    
    test1<-purtest(panel_data[,i], data = panel_data, lags = lag,
                  exo=exo1[j], test="invnormal", ips.stat = "tbar") # this article should be referred as the testing method is based on this paper https://www.sciencedirect.com/science/article/abs/pii/S0261560600000486
    
    result1<-rbind(result1, c(names(panel_data)[i], 
                            test1$statistic$statistic, 
                            lags = "AIC", 
                            test1$statistic$p.value,
                            ifelse(test1$statistic$p.value < 0.05, 
                                   "Stationary",
                                   "Non-Stationary")))
    
    test2<-purtest(panel_data[,i], data = panel_data, lags = lag,
                   exo=exo2[j], test="hadri", ips.stat = "tbar") # this article should be referred as the testing method is based on this paper https://www.sciencedirect.com/science/article/abs/pii/S0261560600000486
    
    result2<-rbind(result2, c(names(panel_data)[i], 
                            test2$statistic$statistic, 
                            lags = "AIC", 
                            test2$statistic$p.value,
                            ifelse(test2$statistic$p.value < 0.01, 
                                  "Not-Stationary",
                                   "Stationary")))
    
  }
  colnames(result1)<-c("variable", "Dickey-Fuller (DF-GLS)", "Lag order", "P-Value", "Verdict")
  ADF[[j]]<-result1
  
  colnames(result2)<-c("variable", "Dickey-Fuller (DF-GLS)", "Lag order", "P-Value", "Verdict")
  kpss[[j]]<-result2
  
}

#write.csv(as.data.frame(do.call(rbind, ADF)), "ADF_main_ind.csv", row.names = F)

#a<-list(NULL)
#a[[2]]<-as.data.frame(do.call(rbind, ADF))


