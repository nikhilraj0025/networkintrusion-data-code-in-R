ntw<-read.csv("C:/Users/AKHIL/Desktop/New folder/NetworkIntrusionTrainData.csv")
View(ntw)
dim(ntw)
library(dplyr)
##############################DATA CLEANING PART###########################################
levels(factor(ntw$protocol_type))
ntw$protocol_type1<-as.numeric(as.factor(ntw$protocol_type))
View(ntw)
levels(factor(ntw$service))
ntw$service1<-as.numeric(as.factor(ntw$service))
ntw<-mutate(ntw,service2=case_when(service1<=22~1,(service1>22 & service1<=44)~2,(service1>44~3)))
View(ntw)
levels(factor(ntw$flag))
ntw$flag1<-as.numeric(as.factor(ntw$flag))
ntw<-mutate(ntw,flag2=case_when((flag1<=6~1),(flag1>6~2)))
View(ntw1)
ntw$land<-factor(ntw$land)
ntw$logged_in<-factor(ntw$logged_in)
View(ntw)
ntw$root_shell<-factor(ntw$root_shell)
ntw$su_attempted<-factor(ntw$su_attempted)
ntw$is_host_login<-factor(ntw$is_host_login)
ntw$is_guest_login<-factor(ntw$is_guest_login)
colnames(ntw)
ntw$class<-factor(ntw$class)
ntw_na<-na.omit(ntw)
ntw1<-ntw[,c(-2,-3,-4,-14,-15,-22,-23,-44,-46)]
View(ntw1)

#library(Boruta)
#ac<-Boruta(class~.,data=ntw_na)


#####################Sampling##################################################################
set.seed(121)
modn<-sample(2,nrow(ntw1),replace=TRUE,prob=c(0.90,0.10))
modn_Train<-ntw1[modn==1,]
modn_Test<-ntw1[modn==2,]

################################MODEL BUILDING ON RANDOM FOREST#######################################33333
library(randomForest)
modntw<-randomForest(class~.,data=modn_Train,ntree=64)
prdntw<-predict(modntw,modn_Test,type="response")
prd_df_mdntw<-data.frame(prdntw,modn_Test$class)
colnames(prd_df_mdntw)<-c("predict","actual")
tntw<-table(prd_df_mdntw$predict,prd_df_mdntw$actual)
tntw
acc_neww<-sum(diag(tntw))/sum(tntw)
acc_neww
library(pROC)
roc<-roc(prd_df_mdntw$actual,as.ordered(prd_df_mdntw$predict))
plot(roc,col='red')
auc(roc)
