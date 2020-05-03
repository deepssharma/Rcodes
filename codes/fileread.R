#!/usr/bin/env Rscript
library(haven)

extension1 <-"sas7bdat"

fileNames <- Sys.glob(paste("/Users/deepalisharma/Rtest/datasets/Sanofi_2000_80/sanofi_data_80/*.",extension1,sep=""))

fileNumbers <- seq(fileNames)

extension2 <-"txt"

for (fileNumber in fileNumbers) {

#gsub("ascent/data/","ascent/data/txt/",fileNames[fileNumber])

fileNames[fileNumber]

newFileName <- paste("",sub(paste("\\.",extension1,sep=""),"",fileNames[fileNumber]),".",extension2,sep="")

samplefile <- read_sas(fileNames[fileNumber])

write.table(samplefile, newFileName, sep="\t")
}
