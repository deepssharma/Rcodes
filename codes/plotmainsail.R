#!/usr/bin/env Rscript
#library(ggplot2)
library(tumgr)

mainsaildata <- read.table("/Users/deepalisharma/Rtest/datasets/mainsail/data/txt/lab.txt", header=T, sep="",stringsAsFactors=FALSE)

mainsaildata$PSAMainsail=as.numeric(as.character(mainsaildata$US_RES))

attach(mainsaildata)
categories <- unique(mainsaildata$RPT)
CountMainsail=0;
CountBlackMainsail=0;

for(i in categories)
{
    plotdata <-subset(mainsaildata, mainsaildata$RPT==i & mainsaildata$TST_NAME=='PSA' & mainsaildata$VISNAME!='Screening' & mainsaildata$VISNAME!='Unscheduled' & mainsaildata$VISNAME!='TX Phase Discontinuation' & (i!=6 & i!=11 & i!=61 & i!=72 & i!=88 & i!=114 & i!=163 & i!=164 & i!=170 & i!=219 & i!=227 & i!=234 & i!=257 & i!=270 & i!=296 & i!=322 & i!=362 & i!=420 & i!=424 & i!=428 & i!=431 & i!=450 & i!=453 & i!=494 & i!=504 & i!=14 & i!=38 & i!=78 & i!=136 & i!=214 & i!=218 & i!=223 & i!=223 & i!=343 & i!=376 & i!=444 & i!=447 & i!=478))
    {
        plotdata$RPT<-plotdata$RPT +1000
        if(nrow(plotdata)==1){
            print("Skipping")
            next
        }
        else if(nrow(plotdata)>1)
        {
            CountMainsail<-CountMainsail+1

            for(ii in 1:nrow(plotdata))
            {
                plotdata$Days[ii] <- ii*21
            }

            name<-c(plotdata$RPT)
            date<-c(plotdata$Days)
            size<-c(plotdata$PSAMainsail)

            if(CountMainsail==1) {
                fitdataMainsail<-data.frame(name,date,size)
                                        #fitdataMainsail$name=CountMainsail
            }
            else if(CountMainsail>1)
            {
                tmpdata<-data.frame(name,date,size)
            #tmpdata$name=CountMainsail
                fitdataMainsail<-rbind(fitdataMainsail,tmpdata)
            }
        }
    }

    plotdata <-subset(mainsaildata, mainsaildata$RPT==i & mainsaildata$TST_NAME=='PSA' & mainsaildata$VISNAME!='Screening' & mainsaildata$VISNAME!='Unscheduled' & mainsaildata$VISNAME!='TX Phase Discontinuation' & (i==6||i==11||i==61||i==72||i==88||i==114||i==163||i==164||i==170||i==219||i==227||i==234||i==257||i==270||i==296||i==322||i==362||i==420||i==424||i==428||i==431||i==450||i==453||i==494||i==504 ))
    {
        plotdata$RPT<-plotdata$RPT +1000

        if(nrow(plotdata)==1){
            print("Skipping")
            #next
        }
        else if(nrow(plotdata)>1)
        {
            for(ii in 1:nrow(plotdata))
            {
                plotdata$Days[ii] <- ii*21
            }
            name<-c(plotdata$RPT)
            date<-c(plotdata$Days)
            size<-c(plotdata$PSAMainsail)

            CountBlackMainsail<-CountBlackMainsail+1
            if(CountBlackMainsail==1) {BlackMainsaildata<-data.frame(name,date,size)}
            else
            {
                tmpdata<-data.frame(name,date,size)
                BlackMainsaildata<-rbind(BlackMainsaildata,tmpdata)}
        }
    }

}

