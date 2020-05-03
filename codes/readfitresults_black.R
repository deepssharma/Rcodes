#!/usr/bin/env Rscript

BresultsData<-read.csv(file="/Users/deepalisharma/Rtest/codes/blackresultsfile2.csv",header=TRUE, sep=',')

attach(BresultsData)
patients<-unique(name)


BlackDdata<-subset(BresultsData, (selectedFit=='dx' | selectedFit=='gd' | selectedFit=='gdphi'))
BlackDvalues<-data.frame("black",BlackDdata$d)
colnames(BlackDvalues)<-c("Race","Dvalue")


BlackGdata<-subset(BresultsData, (selectedFit=='gx' | selectedFit=='gd' | selectedFit=='gdphi'))
BlackGvalues<-data.frame("black",BlackGdata$g)
colnames(BlackGvalues)<-c("Race","Gvalue")



BlackPhidata<-subset(BresultsData, (selectedFit=='gdphi'))
BlackPhivalues<-data.frame("black",BlackPhidata$phi)
colnames(BlackPhivalues)<-c("Race","Phivalue")



WresultsData<-read.csv(file="/Users/deepalisharma/Rtest/codes/whiteresultsfile2.csv",header=TRUE, sep=',')

WhiteDdata<-subset(WresultsData, (selectedFit=='dx' | selectedFit=='gd' | selectedFit=='gdphi'))
WhiteDvalues<-data.frame("white",WhiteDdata$d)
colnames(WhiteDvalues)<-c("Race","Dvalue")


WhiteGdata<-subset(WresultsData, (selectedFit=='gx' | selectedFit=='gd' | selectedFit=='gdphi'))
WhiteGvalues<-data.frame("white",WhiteGdata$g)
colnames(WhiteGvalues)<-c("Race","Gvalue")


WhitePhidata<-subset(WresultsData, (selectedFit=='gdphi'))
WhitePhivalues<-data.frame("white",WhitePhidata$phi)
colnames(WhitePhivalues)<-c("Race","Phivalue")

print(" Mood test for Dvalues ")
mood.test(BlackDvalues$Dvalue, WhiteDvalues$Dvalue)


print(" Mood test for Gvalues ")
mood.test(BlackGvalues$Gvalue, WhiteGvalues$Gvalue)


print(" Mood test for Phivalues ")
mood.test(BlackPhivalues$Phivalue, WhitePhivalues$Phivalue)
