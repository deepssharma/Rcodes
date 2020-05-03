#!/usr/bin/env Rscript
library(tumgr)

my_data <- read.table("/Users/deepalisharma/Rtest/datasets/ascent/data/psa.txt", header=T, sep="",stringsAsFactors=FALSE)

#my_data is a data frame structure
#summary will list the names of all the columns and row numbers
#names(my_data) will print the column names only.
#my_data$SUBID will print all the values for the column SUBID
# to print first 10 rows my_data[1:10,]

my_data$PSANEW=as.numeric(my_data$PSA)

attach(my_data)
categories <- unique(my_data$SUBID)
count=0
blackcount=0
for(i in categories)
{
    count<-count+1
    plotdata <-subset(my_data, my_data$SUBID==i)
    #plotdata$tum_qu <- plotdata$PSANEW/plotdata$PSANEW[1]

    for(ii in 1:nrow(plotdata))
    {
        #plotdata$tum_qu[ii] <- plotdata$PSANEW[ii]/data$PSANEW[1]
        plotdata$Days[ii] <- ii*21
    }
    name<-c(plotdata$SUBID)
    date<-c(plotdata$Days)
    size<-c(plotdata$PSANEW)

    if(count==1) {
        fitdata<-data.frame(name,date,size)
        fitdata$name=count
    }
    else if(count>1 & count!=159)
    {
        tmpdata<-data.frame(name,date,size)
        tmpdata$name=count
        fitdata<-rbind(fitdata,tmpdata)
    }


    if(i=='001-0003'||i=='009-0008'||i=='014-0001'||i=='027-0001'||i=='030-0004'||i=='037-0002'||i=='037-0003'||i=='041-0036'||i=='069-0001'||i=='069-0004'||i=='075-0002'||i=='094-0002'||i=='097-0002'||i=='104-0002'||i=='104-0008'||i=='110-0014'||i=='117-0011'||i=='123-0001'||i=='123-0002'||i=='123-0004'||i=='126-0004'||i=='137-0002'||i=='137-0003'||i=='142-0001'||i=='142-0002'||i=='142-0007'||i=='142-0011'||i=='145-0003'||i=='145-0005'||i=='149-0001'||i=='161-0004'||i=='170-0002')
    {
        blackcount<-blackcount+1
        if(i=='001-0003') {blackdata<-data.frame(name,date,size)
        blackdata$name=blackcount}
        else{blackdata<-rbind(blackdata,tmpdata)}
    }


}


