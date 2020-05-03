library(haven)
input_file <- "cm.sas7bdat"
output_file <- "cm_data.txt"
outfile <- read_sas(input_file)
write.table(outfile, output_file, sep="\t")
