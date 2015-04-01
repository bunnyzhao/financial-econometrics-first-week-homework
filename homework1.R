#######################################################################
#数据来源：WIND数据库                                                 #
#所选取的指数：沪深300                                                #
#所选取的股票：太钢不锈                                               #
#所选取的时间：2010.4.1-2015.4.1                                      #
#######################################################################




###沪深300指数日数据
###Test 1 :顺序和反转
HS300<-read.csv("d:/study/SUFE/金融计量学/homework1/HS300Daily.csv")
hs300<-HS300[,5]
return.hs300<-diff(log(hs300))
n<-length(return.hs300)
I<-rep(0,times=1210)
I[return.hs300>0]<-1
##数据基本统计性质
#折线图
library(ggplot2)
x<-1:1210
qplot(x,return.hs300,color=I+1,geom=c("point","path"))
#均值
mean(return.hs300)
#方差
var(return.hs300)
#偏度
library(e1071)
skewness(return.hs300)
#峰度
kurtosis(return.hs300)
##Tests of RW1
#sequences and reversals，
n<-length(return.hs300)
I<-rep(0,times=1210)
I[return.hs300>0]<-1
Y<-I[-1]*I[-length(I)]+(1-I[-1])*(1-I[-length(I)])
(Ns<-sum(Y))#number of sequences in sample
(Nr<-length(Y)-Ns)#number of reversals in sample
#Cowless-Jones Ratio in sample算法一
(CJhat<-Ns/Nr)
(pishat<-Ns/n)
(pihat<-sum(I)/length(I))
(Asy.Var.CJhat<-(pishat*(1-pishat)+2*(pihat^3+(1-pihat)^3-pishat^2))/(n*(1-pishat)^4))
(Asy.E.CJhat<-pishat/(1-pishat))
1-pnorm(CJhat,mean=Asy.E.CJhat,sd=sqrt(Asy.Var.CJhat))#do not reject the H0,which means the market is efficient
#Cowless-Jones Ratio in sample算法二
alpha<-0;beta<-0
for(i in 1:1209){
    j<-i+1
    if(I[i]==0 & I[j]==1){
        beta<-beta+1
    }
    if(I[i]==1 & I[j]==0){
        alpha<-alpha+1
    }
}
(alpha<-alpha/(sum(I[-length(I)]==1)))
(beta<-beta/(sum(I[-length(I)]==0)))
(CJhat<-((1-alpha)*beta+(1-beta)*alpha)/(2*alpha*beta))
1-pnorm(CJhat,mean=Asy.E.CJhat,sd=sqrt(Asy.Var.CJhat))#do not reject the H0,which means the market is efficient
#runs
Nruns<-sum((diff(I))!=0)+1#numbers of runs
(z<-(Nruns+1/2-2*n*pihat*(1-pihat))/(2*sqrt(n*pihat*(1-pihat)*(1-3*pihat*(1-pihat)))))
pnorm(z)#do not reject the H0,which means that the market is efficient
               

###沪深300指数月数据
###Test 1 :顺序和反转
HS300<-read.csv("d:/study/SUFE/金融计量学/homework1/HS300Monthly.csv")
hs300<-HS300[,5]
return.hs300<-diff(log(hs300))
n<-length(return.hs300)
I<-rep(0,times=59)
I[return.hs300>0]<-1
##数据基本统计性质
#折线图
library(ggplot2)
x<-1:59
qplot(x,return.hs300,color=I+1,geom=c("point","path"))
#均值
mean(return.hs300)
#方差
var(return.hs300)
#偏度
library(e1071)
skewness(return.hs300)
#峰度
kurtosis(return.hs300)
##Tests of RW1
#sequences and reversals，
n<-length(return.hs300)
I<-rep(0,times=59)
I[return.hs300>0]<-1
Y<-I[-1]*I[-length(I)]+(1-I[-1])*(1-I[-length(I)])
(Ns<-sum(Y))#number of sequences in sample
(Nr<-length(Y)-Ns)#number of reversals in sample
#Cowless-Jones Ratio in sample算法一
(CJhat<-Ns/Nr)
(pishat<-Ns/n)
(pihat<-sum(I)/length(I))
(Asy.Var.CJhat<-(pishat*(1-pishat)+2*(pihat^3+(1-pihat)^3-pishat^2))/(n*(1-pishat)^4))
(Asy.E.CJhat<-pishat/(1-pishat))
1-pnorm(CJhat,mean=Asy.E.CJhat,sd=sqrt(Asy.Var.CJhat))#do not reject the H0,which means the market is efficient
#Cowless-Jones Ratio in sample算法二
alpha<-0;beta<-0
for(i in 1:58){
    j<-i+1
    if(I[i]==0 & I[j]==1){
        beta<-beta+1
    }
    if(I[i]==1 & I[j]==0){
        alpha<-alpha+1
    }
}
(alpha<-alpha/(sum(I[-length(I)]==1)))
(beta<-beta/(sum(I[-length(I)]==0)))
(CJhat<-((1-alpha)*beta+(1-beta)*alpha)/(2*alpha*beta))
1-pnorm(CJhat,mean=Asy.E.CJhat,sd=sqrt(Asy.Var.CJhat))#do not reject the H0,which means the market is efficient
#runs
Nruns<-sum((diff(I))!=0)+1#numbers of runs
(z<-(Nruns+1/2-2*n*pihat*(1-pihat))/(2*sqrt(n*pihat*(1-pihat)*(1-3*pihat*(1-pihat)))))
pnorm(z)#do not reject the H0,which means that the market is efficient



###太钢不锈股票日数据
###Test 1 :顺序和反转
TGBX<-read.csv("d:/study/SUFE/金融计量学/homework1/TGBXDaily.csv")
tgbx<-TGBX[,5]
return.tgbx<-diff(log(tgbx))
n<-length(return.tgbx)
I<-rep(0,times=1210)
I[return.tgbx>0]<-1
##数据基本统计性质
#折线图
library(ggplot2)
x<-1:1210
qplot(x,return.tgbx,color=I+1,geom=c("point","path"))
#均值
mean(return.tgbx)
#方差
var(return.tgbx)
#偏度
library(e1071)
skewness(return.tgbx)
#峰度
kurtosis(return.tgbx)
##Tests of RW1
#sequences and reversals，
n<-length(return.tgbx)
I<-rep(0,times=1210)
I[return.tgbx>0]<-1
Y<-I[-1]*I[-length(I)]+(1-I[-1])*(1-I[-length(I)])
(Ns<-sum(Y))#number of sequences in sample
(Nr<-length(Y)-Ns)#number of reversals in sample
#Cowless-Jones Ratio in sample算法一
(CJhat<-Ns/Nr)
(pishat<-Ns/n)
(pihat<-sum(I)/length(I))
(Asy.Var.CJhat<-(pishat*(1-pishat)+2*(pihat^3+(1-pihat)^3-pishat^2))/(n*(1-pishat)^4))
(Asy.E.CJhat<-pishat/(1-pishat))
1-pnorm(CJhat,mean=Asy.E.CJhat,sd=sqrt(Asy.Var.CJhat))#do not reject the H0,which means the market is efficient
#Cowless-Jones Ratio in sample算法二
alpha<-0;beta<-0
for(i in 1:1209){
    j<-i+1
    if(I[i]==0 & I[j]==1){
        beta<-beta+1
    }
    if(I[i]==1 & I[j]==0){
        alpha<-alpha+1
    }
}
(alpha<-alpha/(sum(I[-length(I)]==1)))
(beta<-beta/(sum(I[-length(I)]==0)))
(CJhat<-((1-alpha)*beta+(1-beta)*alpha)/(2*alpha*beta))
1-pnorm(CJhat,mean=Asy.E.CJhat,sd=sqrt(Asy.Var.CJhat))#do not reject the H0,which means the market is efficient
#runs
Nruns<-sum((diff(I))!=0)+1#numbers of runs
(z<-(Nruns+1/2-2*n*pihat*(1-pihat))/(2*sqrt(n*pihat*(1-pihat)*(1-3*pihat*(1-pihat)))))
1-pnorm(z)#do not reject the H0,which means that the market is efficient



###太钢不锈股票月数据
###Test 1 :顺序和反转
TGBX<-read.csv("d:/study/SUFE/金融计量学/homework1/TGBXMonthly.csv")
tgbx<-TGBX[,5]
return.tgbx<-diff(log(tgbx))
n<-length(return.tgbx)
I<-rep(0,times=59)
I[return.tgbx>0]<-1
##数据基本统计性质
#折线图
library(ggplot2)
x<-1:59
qplot(x,return.tgbx,color=I+1,geom=c("point","path"))
#均值
mean(return.tgbx)
#方差
var(return.tgbx)
#偏度
library(e1071)
skewness(return.tgbx)
#峰度
kurtosis(return.tgbx)
##Tests of RW1
#sequences and reversals，
n<-length(return.tgbx)
I<-rep(0,times=59)
I[return.tgbx>0]<-1
Y<-I[-1]*I[-length(I)]+(1-I[-1])*(1-I[-length(I)])
(Ns<-sum(Y))#number of sequences in sample
(Nr<-length(Y)-Ns)#number of reversals in sample
#Cowless-Jones Ratio in sample算法一
(CJhat<-Ns/Nr)
(pishat<-Ns/n)
(pihat<-sum(I)/length(I))
(Asy.Var.CJhat<-(pishat*(1-pishat)+2*(pihat^3+(1-pihat)^3-pishat^2))/(n*(1-pishat)^4))
(Asy.E.CJhat<-pishat/(1-pishat))
1-pnorm(CJhat,mean=Asy.E.CJhat,sd=sqrt(Asy.Var.CJhat))#do not reject the H0,which means the market is efficient
#Cowless-Jones Ratio in sample算法二
alpha<-0;beta<-0
for(i in 1:58){
    j<-i+1
    if(I[i]==0 & I[j]==1){
        beta<-beta+1
    }
    if(I[i]==1 & I[j]==0){
        alpha<-alpha+1
    }
}
(alpha<-alpha/(sum(I[-length(I)]==1)))
(beta<-beta/(sum(I[-length(I)]==0)))
(CJhat<-((1-alpha)*beta+(1-beta)*alpha)/(2*alpha*beta))
1-pnorm(CJhat,mean=Asy.E.CJhat,sd=sqrt(Asy.Var.CJhat))#do not reject the H0,which means the market is efficient
#runs
Nruns<-sum((diff(I))!=0)+1#numbers of runs
(z<-(Nruns+1/2-2*n*pihat*(1-pihat))/(2*sqrt(n*pihat*(1-pihat)*(1-3*pihat*(1-pihat)))))
1-pnorm(z)#do not reject the H0,which means that the market is efficient



#######################################################################
#疑问：既然以上不论是指数或者股票的日或月数据都表明市场是有效的，也即 #
#我们无法基于历史数据预测未来数据，那我们如何套利呢？难道就随机猜嘛？ #
#######################################################################