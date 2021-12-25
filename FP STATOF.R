getwd()
setwd("C:/Users/User/Downloads")
library(spdep)
library(lmtest)
library(plm)
library(car)
library(GWmodel)
#datapanel=read.table("statof.xlsx", sep = ";", fill = TRUE, header = TRUE)
#datapanel
statof

datapanel=as.data.frame(statof)
str(datapanel)
cor(datapanel[,3:7])
colnames(datapanel)

#MODEL I
#model random
modelpanel1<-plm(Y~X1+X2+X3+X4,
                datapanel,model='random',index=c('provinsi','thn'))
summary(modelpanel1)
#model within
modelpanel2<-plm(Y~X1+X2+X3+X4,
                 datapanel,model='within',index=c('provinsi','thn'))
summary(modelpanel2)
#model pooling
modelpanel3<-plm(Y~X1+X2+X3+X4,
                 datapanel,model='pooling',index=c('provinsi','thn'))
summary(modelpanel3)

#uji chow
pFtest(modelpanel2,modelpanel3)
#uji hausman
phtest(modelpanel2,modelpanel1)
#efek dua arah
plmtest(modelpanel1,effect="twoways",type="bp")
#efek individu/cross section
plmtest(modelpanel1,effect="individual",type="bp")
#efek waktu/time
plmtest(modelpanel1,effect="time",type="bp")


m1<-plm(Y~X1+X2+X3+X4,
                 datapanel,model='random',effect='individual',index=c('provinsi','thn'))
summary(m1)

#MODEL II
#model random
modelpanel1<-plm(Y~X1+X2+X4,
                 datapanel,model='random',index=c('provinsi','thn'))
summary(modelpanel1)
#model within
modelpanel2<-plm(Y~X1+X2+X4,
                 datapanel,model='within',index=c('provinsi','thn'))
summary(modelpanel2)
#model pooling
modelpanel3<-plm(Y~X1+X2+X4,
                 datapanel,model='pooling',index=c('provinsi','thn'))
summary(modelpanel3)

#uji chow
pFtest(modelpanel2,modelpanel3)
#uji hausman
phtest(modelpanel2,modelpanel1)
#efek dua arah
plmtest(modelpanel2,effect="twoways",type="bp")
#efek individu/cross section
plmtest(modelpanel2,effect="individual",type="bp")
#efek waktu/time
plmtest(modelpanel2,effect="time",type="bp")

m2<-plm(Y~X1+X2+X4,
        datapanel,model='within',effect='individual',index=c('provinsi','thn'))
summary(m2)

#UJI ASUMSI
bptest(m1)
bptest(m2)

#==================

m2<-plm(Y~X1+X2+X4,
        datapanel,model='random',effect='individual',index=c('provinsi','thn'))
summary(m2)

m3<-plm(Y~X1+X4,
        datapanel,model='random',effect='individual',index=c('provinsi','thn'))
summary(m3)

pbgtest(m1)
pbgtest(m2)
pbgtest(m3)



bptest(m1)
bptest(m2)
bptest(m3)

#==================

#pdwtest(m4)
#vif(m4)
#r.squared(m4, dfcor = TRUE)
#r.squared(m4)
