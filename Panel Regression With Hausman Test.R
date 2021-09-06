#-----------Panel Reg--------------------#
setwd("C:/protein")
set.seed(123)

#---------------Libraries requires (make sure to install these before begin)---------------#
library(plm)
library(mice)


per_urban<-31.16

#-------------data import---------------------------
data_res<-as.data.frame(read.csv("data_res.csv", header = T)) #importing data of residential area
data_ind<-as.data.frame(read.csv("data_ind.csv", header = T)) #importing data of industrial area
area<-"residentia"

if(area=="residential"){ data <- data_res} else{data <- data_ind}

#data<-data[-which(data$states==""),] # getting rid of blank entries
data[,3:ncol(data)]<-apply(data[,3:ncol(data)], 2, as.numeric) # converting to numeric type
#data[which(data==0,arr.ind = T)]<-NA 

my_data<-data
mean(my_data$X..URBAN[!is.na(my_data$X..URBAN)])
#---------------STATE FILTERING-----------------------

mat1<-matrix(my_data$X..URBAN, nrow=length(2004:2014))
state1<-unique(my_data$STATE.UT)[which(colMeans(mat1) >= per_urban)] #urban

write.csv(as.data.frame(cbind(unique(my_data$STATE.UT), colMeans(mat1))),
          "avg_urb.csv", row.names = F)

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

#write.csv(cor(my_data[3:ncol(my_data)]), "correlation.csv", row.names = F)

#---------Log transformation--------------------
my_data[,3:ncol(my_data)]<-apply(my_data[,3:ncol(my_data)], 2,log)


#------------ FOR SO2 Analysis----------------------#

my_data_so2<-subset(my_data, select=-NO2)
panel_data_so2<-pdata.frame(my_data_so2, index = c("YEAR", "STATE.UT")) #panel data format

names(panel_data_so2)

so2_reg_re<-plm(SO2 ~ SERVICES...GDP 
                  +INDUSTRY...GDP
                  +GDP.PER.CAPITA
                  +POPULATION
                  +X..URBAN,
                data = panel_data_so2, model = "random",
                effect = "individual") #random effect model

so2_reg_fd<-plm(SO2 ~ SERVICES...GDP 
                +INDUSTRY...GDP
                +GDP.PER.CAPITA
                +POPULATION
                +X..URBAN,
                data = panel_data_so2, model = "within",
                effect = "individual") #Fixed effect

hausman_test_so2<-phtest(so2_reg_fd,so2_reg_re) #hausman test



predict(so2_reg_fd)

#----------------------NO2 Analysis---------------------------#

my_data_no2<-subset(my_data, select=-SO2)
panel_data_no2<-pdata.frame(my_data_no2, index = c("YEAR", "STATE.UT")) #panel data format


no2_reg_re<-plm(NO2 ~ SERVICES...GDP 
                +INDUSTRY...GDP
                +GDP.PER.CAPITA
                +POPULATION
                +X..URBAN,
                data = panel_data_no2, model = "random",
                effect = "individual") #random effect model

no2_reg_fd<-plm(NO2 ~ SERVICES...GDP 
                +INDUSTRY...GDP
                +GDP.PER.CAPITA
                +POPULATION
                +X..URBAN,
                data = panel_data_no2, model = "within",
                effect = "individual") #Fixed effect


hausman_test_no2<-phtest(no2_reg_fd,no2_reg_re) #hausman test



#----------------REsults--------------------------
paste0(c("States included in study:", state), collapse = ", ")


#--------------------------- SO2 Results--------------------------------------------------

hausman_test_so2 #hausman test result for so2

ifelse(hausman_test_so2$p.value < 0.05, 
       print(summary(so2_reg_fd)), 
       print(summary(so2_reg_re)))

# lets firts do for so2-----

so2_summary<-summary(so2_reg_fd)

# this is how you import

#write.csv( so2_summary$coefficients, "coefficient_of_so2.csv", row.names = F)

#---------------------------------- NO2 Results-------------------------------------------

hausman_test_no2 #hausman test result for no2

ifelse(hausman_test_no2$p.value < 0.05, 
       print(summary(no2_reg_fd)), 
       print(summary(no2_reg_re)))


no2_summary<-summary(no2_reg_fd)
#write.csv( no2_summary$coefficients, "coefficient_of_no2.csv", row.names = F)

#car::vif(no2_reg_re)
#a<-list(NULL)
#a[[2]]<-rbind(summary(so2_reg_re)[[1]],
              #summary(no2_reg_re)[[1]])

'write.csv(do.call(rbind, a),"summary_all.csv", row.names=T)'

