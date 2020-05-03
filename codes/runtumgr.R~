#!/usr/bin/env Rscript
library(ggplot2)

my_data <- read.table("/Users/deepalisharma/Rtest/datasets/ascent/data/psa.txt", header=T, sep="",stringsAsFactors=FALSE)

#my_data is a data frame structure
#summary will list the names of all the columns and row numbers
#names(my_data) will print the column names only.
#my_data$SUBID will print all the values for the column SUBID
# to print first 10 rows my_data[1:10,]

my_data$PSANEW=as.numeric(my_data$PSA)

attach(my_data)
categories <- unique(my_data$SUBID)

for(i in categories)
{
    plot_data <-subset(my_data, my_data$SUBID==i)
    plot_data$tum_qu <- plot_data$PSANEW/plot_data$PSANEW[1]

    for(ii in 1:nrow(plot_data))
    {
        #plot_data$tum_qu[ii] <- plot_data$PSANEW[ii]/data$PSANEW[1]
        plot_data$Days[ii] <- ii*21
    }
    #plot(plot_data$tum_qu,pch=5,ylim=c(0,1.2), xlim=c(0.5,)
    #plot(plot_data$days,plot_data$tum_qu, pch=5, ylim=c(0,1.2),xlim=c(0,180), ylab="Tumor Fraction", xlab="Treatment Days")

    plotname <-file.path("", "Users", "deepalisharma", "Rtest","codes", "figs", paste("subid_", i, ".pdf", sep=""))
    p1 <- ggplot(plot_data, aes(y=tum_qu, x=Days))
    p1 +geom_point() +xlim(1,170)
    #p1 +geom_point() +geom_smooth() + ylim(0,10) +xlim(1,170)
    #p1+ geom_smooth(method='nls',formula=y~exp(-d*x)+exp(g*x)-1, method.args = list(start = c(d=0.01, g=0.01)), se=FALSE) +xlim(1,170)
    #ggsave(filename=plotname, plot=last_plot(), device=NULL)
}

fitgd<-function(x,d,g)
{
    n1<-exp(-d*x)+exp(g*x)-1
    return (d, g)
}

fitg<-function(x,g)
{
    n1<-exp(g*x)
    return (g)
}

fitd<-function(x,d)
{
    n1<-exp(-d*x)
    return (d)
}
