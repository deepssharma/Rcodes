#!/usr/bin/env Rscript
#library(ggplot2)
library(tumgr)

venicedata <- read.table("/Users/deepalisharma/Rtest/datasets/venice/data/txt/adlb.txt", header=T, sep="",stringsAsFactors=FALSE)

venicedata$PSANEW=venicedata$LBSTRESN

attach(venicedata)
categories <- unique(venicedata$SUBJID)
CountVenice=0;
CountBlackVenice=0;

for(i in categories)
{

    plotdata <-subset(venicedata, venicedata$SUBJID==i & venicedata$LBTEST=='PSA' & venicedata$VISIT!='Screening' & venicedata$VISIT!='Follow-up 0 (30 days)' & venicedata$VISIT!='Follow-up 1' & venicedata$VISIT!='Follow-up 2' & venicedata$VISIT!='Follow-up 3' & venicedata$VISIT!='Follow-up 4' & venicedata$VISIT!='Follow-up 5' & venicedata$VISIT!='Follow-up 6' & venicedata$VISIT!='Follow-up 7' & venicedata$VISIT!='Follow-up 8' & venicedata$VISIT!='Follow-up 9' & venicedata$VISIT!='Follow-up 10' & venicedata$VISIT!='Follow-up 11' & venicedata$VISIT!='Follow-up 12' & venicedata$VISIT!='Follow-up 13' & venicedata$VISIT!='Follow-up 14' & venicedata$VISIT!='Follow-up 15' & venicedata$VISIT!='Follow-up 18' & venicedata$VISIT!='Follow-up 19' & venicedata$VISIT!='Follow-up 20' & venicedata$RACE=='CAUCASIAN/WHITE')
    #plotdata <-subset(venicedata, venicedata$SUBJID==i & venicedata$LBTEST=='PSA' )

    plotdata$LBDTC <- as.POSIXct( plotdata$LBDTC , format = "%Y-%m-%d %S:%I:%M %p" , tz = "GMT")
    class(plotdata$LBDTC)
    plotdata <- plotdata[do.call(order, plotdata), ]

    if(nrow(plotdata)==1){
        print("Skipping")
        next
    }
    else if(nrow(plotdata)>1)
    {
        #plotdata[,c(3,45,92,97,98)]
        CountVenice<-CountVenice+1

        for(ii in 1:nrow(plotdata))
        {
            plotdata$Days[ii] <- ii*21
        }

        name<-c(plotdata$SUBJID)
        date<-c(plotdata$Days)
        size<-c(plotdata$PSANEW)

        if(CountVenice==1) {
            fitdataVenice<-data.frame(name,date,size)
            #fitdataVenice$name=CountVenice
        }
        else if(CountVenice>1)
        {
            tmpdata<-data.frame(name,date,size)
            #tmpdata$name=CountVenice
            fitdataVenice<-rbind(fitdataVenice,tmpdata)
        }
    }
        #if(i==140007401||i==420208601|i==430208501||i==440408401||i==450008501||i==470808401||i==480308611||i==490308501||i==500102401||i==530002201||i==576000401||i==716100202||i==716100701||i==766000202||i==766000501||i==786000202||i==796000501)
    plotdata <-subset(venicedata, venicedata$SUBJID==i & venicedata$LBTEST=='PSA' & venicedata$VISIT!='Screening' & venicedata$VISIT!='Follow-up 0 (30 days)' & venicedata$VISIT!='Follow-up 1' & venicedata$VISIT!='Follow-up 2' & venicedata$VISIT!='Follow-up 3' & venicedata$VISIT!='Follow-up 4' & venicedata$VISIT!='Follow-up 5' & venicedata$VISIT!='Follow-up 6' & venicedata$VISIT!='Follow-up 7' & venicedata$VISIT!='Follow-up 8' & venicedata$VISIT!='Follow-up 9' & venicedata$VISIT!='Follow-up 10' & venicedata$VISIT!='Follow-up 11' & venicedata$VISIT!='Follow-up 12' & venicedata$VISIT!='Follow-up 13' & venicedata$VISIT!='Follow-up 14' & venicedata$VISIT!='Follow-up 15' & venicedata$VISIT!='Follow-up 18' & venicedata$VISIT!='Follow-up 19' & venicedata$VISIT!='Follow-up 20' & venicedata$RACE=='BLACK')
    {
        plotdata$LBDTC <- as.POSIXct( plotdata$LBDTC , format = "%Y-%m-%d %S:%I:%M %p" , tz = "GMT")
        class(plotdata$LBDTC)
        plotdata <- plotdata[do.call(order, plotdata), ]

        if(nrow(plotdata)==1){
            print("Skipping")
            next
        }
        else if(nrow(plotdata)>1)
        {
            for(ii in 1:nrow(plotdata))
            {
                plotdata$Days[ii] <- ii*21
            }

            name<-c(plotdata$SUBJID)
            date<-c(plotdata$Days)
            size<-c(plotdata$PSANEW)

            CountBlackVenice<-CountBlackVenice+1
            if(CountBlackVenice==1) {BlackVenicedata<-data.frame(name,date,size)}
            else
            {
                tmpdata<-data.frame(name,date,size)
                BlackVenicedata<-rbind(BlackVenicedata,tmpdata)
            }

            print(i)
        }
    }
}

