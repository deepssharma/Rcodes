# Data input, preprocessing and imputation
# by Teemu Daniel Laajala (teelaa@utu.fi)

	#########
	######### PHASE A: Read data (Core tables)
	#########

# Read from raw data folder
#setwd("D:\\Work\\PhD_2015\\DREAM\\final\\RawData\\")

# Core tables
ct.vl <- read.csv("CoreTable_validation.csv", stringsAsFactors=F)
ct.tr <- read.csv("CoreTable_training.csv", stringsAsFactors=F)
ct.lb <- read.csv("CoreTable_leaderboard.csv", stringsAsFactors=F)

# Individual names
rownames(ct.vl) <- ct.vl[,"RPT"]
rownames(ct.lb) <- ct.lb[,"RPT"]
rownames(ct.tr) <- ct.tr[,"RPT"]
names.vl <- ct.vl[,"RPT"]
names.lb <- ct.lb[,"RPT"]
names.tr <- ct.tr[,"RPT"]

# Transform the categories of weights in AZ to averaged numerical values to conserve information in ASC, CELGENE and EFC better
ct.lb[,"WEIGHTBL"] = seq(from=45, to=145, by=10)[match(ct.lb[,"WGTBLCAT"], c(">=40-50", ">=50-60", ">=60-70", ">=70-80", ">=80-90", ">=90-100", ">=100-110", ">=110-120", ">=120-130", ">=130-140"))]
ct.vl[,"WEIGHTBL"] = seq(from=45, to=145, by=10)[match(ct.vl[,"WGTBLCAT"], c(">=40-50", ">=50-60", ">=60-70", ">=70-80", ">=80-90", ">=90-100", ">=100-110", ">=110-120", ">=120-130", ">=130-140"))]
# Same for heights
ct.lb[,"HEIGHTBL"] = seq(from=150, to=210, by=20)[match(ct.lb[,"HGTBLCAT"], c(">=140-160", ">=160-180", ">=180-200", ">=200-220"))]
ct.vl[,"HEIGHTBL"] = seq(from=150, to=210, by=20)[match(ct.vl[,"HGTBLCAT"], c(">=140-160", ">=160-180", ">=180-200", ">=200-220"))]
# Will not do the same for age, as there the ordinal categories were extremely deviating in ranges
ct <- rbind(ct.tr, ct.lb, ct.vl)
# Ordinal variable age
ct[ct[,"AGEGRP2"] == "18-64","AGEGRP2"] = 0
ct[ct[,"AGEGRP2"] == "65-74","AGEGRP2"] = 1
ct[ct[,"AGEGRP2"] == ">=75","AGEGRP2"] = 2

# Fields that should be removed in any case, due to e.g. no presence in the validation set
remove <- c("DOMAIN", "SMOKE", "SMOKFREQ", "SMOKSTAT", "TRT1_ID", "TRT2_ID")

# Potential numeric fields
nums <- c("BMI","HEIGHTBL","WEIGHTBL","ALP","ALT","AST","CA","CREAT","HB","LDH","NEU","PLT","PSA","TBILI","TESTO","WBC","CREACL","NA.","MG","PHOS","ALB","TPRO","RBC","LYM","BUN","CCRC","GLU")
# Potential binary fields
bins <- c("NON_TARGET","TARGET","BONE","RECTAL","LYMPH_NODES","KIDNEYS","LUNGS","LIVER","PLEURA","OTHER","PROSTATE","ADRENAL","BLADDER","PERITONEUM","COLON","HEAD_AND_NECK","SOFT_TISSUE","STOMACH","PANCREAS","THYROID","ABDOMINAL","ORCHIDECTOMY","PROSTATECTOMY","TURP","LYMPHADENECTOMY","SPINAL_CORD_SURGERY","BILATERAL_ORCHIDECTOMY","PRIOR_RADIOTHERAPY","ANALGESICS","ANTI_ANDROGENS","GLUCOCORTICOID","GONADOTROPIN","BISPHOSPHONATE","CORTICOSTEROID","IMIDAZOLE","ACE_INHIBITORS","BETA_BLOCKING","HMG_COA_REDUCT","ESTROGENS","ANTI_ESTROGENS","ARTTHROM","CEREBACC","CHF","DVT","DIAB","GASTREFL","GIBLEED","MI","PUD","PULMEMB","PATHFRAC","SPINCOMP","COPD","MHBLOOD","MHCARD","MHCONGEN","MHEAR","MHENDO","MHEYE","MHGASTRO","MHGEN","MHHEPATO","MHIMMUNE","MHINFECT","MHINJURY","MHINVEST","MHMETAB","MHMUSCLE","MHNEOPLA","MHNERV","MHPSYCH","MHRENAL","MHRESP","MHSKIN","MHSOCIAL","MHSURG","MHVASC")
# Potential ordinal fields
ords <- c("ECOG_C","AGEGRP2") # Height & weight instead as semi-numeric due to so many intervals while most studies provided exact values
# Potential categorical fields
cats <- c("RACE_C","REGION_C")
# Responses
resp <- c("DEATH","LKADT_P","DISCONT","ENDTRS_C")


# List of variable types
vartypes = list(nums = nums, bins = bins, ords = ords, cats = cats, resp = resp)

# Process numeric columns
for(column in nums){
	ct[ct[,column]=="." | is.na(ct[,column]) ,column] <- NA
	ct[,column] <- as.numeric(ct[,column])
}
#ct[sample(1:nrow(ct),7),nums]
nummat <- ct[,nums]

# Process binary columns
# Include binary characteristics only if they have at least 10 positive outcomes, since otherwise these are too redundant in respect to the whole data
bins <- bins[unlist(lapply(bins, FUN=function(z) sum(ct[,z] %in% c("Y", "YES"))))>10]
# Process binary fields into {0,1}
for(column in bins){
	ct[ct[,column]=="" | is.na(ct[,column]),column] <- 0
	ct[ct[,column]=="Y" | ct[,column]=="YES",column] <- 1
	ct[,column] <- as.numeric(ct[,column])
}
binmat <- ct[,bins]

# Create binary batch indicators per each study
binbatches <- matrix(0, nrow=nrow(ct), ncol=4)
colnames(binbatches) <- c("isASC", "isAZ", "isCELG", "isEFC")
binbatches[ct[,"STUDYID"]=="ASCENT2","isASC"] <- 1
binbatches[ct[,"STUDYID"]=="AZ","isAZ"] <- 1
binbatches[ct[,"STUDYID"]=="CELGENE","isCELG"] <- 1
binbatches[ct[,"STUDYID"]=="EFC6546","isEFC"] <- 1
rownames(binbatches) <- rownames(ct)

ordmat <- ct[,ords]

## Categorical variables
ct[ct[,"RACE_C"]=="Missing","RACE_C"] <- NA
ct[ct[,"REGION_C"]=="MISSING","REGION_C"] <- NA
     
catmat <- matrix(0, nrow=nrow(ct), ncol=sum(5+7))
colnames(catmat) <- 
	c("RaceAsian", "RaceBlack", "RaceHispanic", "RaceOther", "RaceWhite",
	"RegionAfrica", "RegionAsia", "RegionEastEuro", "RegionNorthAmer", "RegionOther", "RegionSouthAmer", "RegionWestEuro")

# Binary indicators; missing information at origin {0,0,0,0,0}	
catmat[ct[,"RACE_C"]=="Asian", "RaceAsian"] <- 1
catmat[ct[,"RACE_C"]=="Black", "RaceBlack"] <- 1
catmat[ct[,"RACE_C"]=="Hispanic", "RaceHispanic"] <- 1
catmat[ct[,"RACE_C"]=="Other", "RaceOther"] <- 1
catmat[ct[,"RACE_C"]=="White", "RaceWhite"] <- 1
# Similarly for regions
catmat[ct[,"REGION_C"]=="AFRICA", "RegionAfrica"] <- 1
catmat[ct[,"REGION_C"]=="ASIA/PACIFIC", "RegionAsia"] <- 1
catmat[ct[,"REGION_C"]=="EASTERN EUROPE", "RegionEastEuro"] <- 1
catmat[ct[,"REGION_C"]=="NORTH AMERICA", "RegionNorthAmer"] <- 1
catmat[ct[,"REGION_C"]=="OTHER", "RegionOther"] <- 1
catmat[ct[,"REGION_C"]=="SOUTH AMERICA", "RegionSouthAmer"] <- 1
catmat[ct[,"REGION_C"]=="WESTERN EUROPE", "RegionWestEuro"] <- 1
# Name of categorical (binary indicator) variables
cats <- colnames(catmat)

# Patient identification codes  
rownames(nummat) <- rownames(binmat) <- rownames(ordmat) <- rownames(catmat) <- rownames(binbatches) <- rownames(ct)


# Data matrices:
# nummat: Numeric variable matrix
# binmat: Binary variable matrix
# ordmat: Ordinary variable matrix
# catmat : Categorical variable matrix that have been binary coded
# binbatches: Binary indicators for the study batches

# Response matrix Y:
# All entries missing for AZ, it is the target for predictions

respmatrix <- matrix(NA, nrow=nrow(ct), ncol=3)
colnames(respmatrix) <- c("DEATH", "LKADT_P", "DISCONT")
respmatrix[ct[,"DEATH"]=="YES","DEATH"] <- 1
respmatrix[ct[,"DEATH"]=="","DEATH"] <- 0
respmatrix[,"LKADT_P"] <- as.numeric(ct[,"LKADT_P"])
respmatrix[,"DISCONT"] <- as.numeric(ct[,"DISCONT"])
rownames(respmatrix) <- rownames(ct)

	####
	#### PHASE B: Obtain few of vital sign values that could be useful (that are not readily available in core table) as well as some potentially interesting additional lab values
	####

# Read Vital Sign tables
vs.lb <- read.csv("VitalSign_leaderboard.csv", stringsAsFactors=F)
vs.tr <- read.csv("VitalSign_training.csv", stringsAsFactors=F)
vs.vl <- read.csv("VitalSign_validation.csv", stringsAsFactors=F)
tmp.tr = vs.tr[vs.tr[,"VSBLFL"]=="Y" & vs.tr[,"VSTEST"] %in% c("SYSTOLIC BLOOD PRESSURE", "DIASTOLIC BLOOD PRESSURE", "PULSE"),c("RPT", "VSTEST", "VSSTRESN")]
tmp.lb = vs.lb[vs.lb[,"VSBLFL"]=="Y" & vs.lb[,"VSTEST"] %in% c("SYSTOLIC BLOOD PRESSURE", "DIASTOLIC BLOOD PRESSURE", "PULSE"),c("RPT", "VSTEST", "VSSTRESN")]
tmp.vl = vs.vl[vs.vl[,"VSBLFL"]=="Y" & vs.vl[,"VSTEST"] %in% c("SYSTOLIC BLOOD PRESSURE", "DIASTOLIC BLOOD PRESSURE", "PULSE"),c("RPT", "VSTEST", "VSSTRESN")]
vs <- rbind(tmp.tr, tmp.lb, tmp.vl)
# Construct data-matrix like structure of baseline measurements
vsvars <- do.call("rbind", lapply(rownames(ct), FUN=function(z){
	w <- which(vs[,"RPT"]==z)
	mat <- vs[w,]
	w1 <- which(mat[,"VSTEST"]=="SYSTOLIC BLOOD PRESSURE")
	w2 <- which(mat[,"VSTEST"]=="DIASTOLIC BLOOD PRESSURE")
	w3 <- which(mat[,"VSTEST"]=="PULSE")
	vec <- rep(NA, times=3)
	if(length(w1)>0) vec[1] <- mat[w1,"VSSTRESN"]
	if(length(w2)>0) vec[2] <- mat[w2,"VSSTRESN"]
	if(length(w3)>0) vec[3] <- mat[w3,"VSSTRESN"]
	vec <- as.numeric(vec)
	names(vec) <- c("SYSTOLICBP", "DIASTOLICBP", "PULSE")
	vec
}))
rownames(vsvars) <- rownames(ct)
####
#### Obtain few of new lab values that could be useful (that are not readily available in core table)
####
lv.lb <- read.csv("LabValue_leaderboard.csv", stringsAsFactors=F)
lv.tr <- read.csv("LabValue_training.csv", stringsAsFactors=F)
lv.vl <- read.csv("LabValue_validation.csv", stringsAsFactors=F)
# Some additional lab values beyond the core table were identified that might still be useful
lvmark <- c("HEMATOCRIT", "SPECIFIC GRAVITY", "LYMPHOCYTES/LEUKOCYTES", "MONOCYTES", "MONOCYTES/LEUKOCYTES", "NEUTROPHILS/LEUKOCYTES", "POTASIUM", "BASOPHILS/LEUKOCYTES", "EOSINOPHILS", "EOSINOPHILS/LEUKOCYTES")
# NOT USING STANDARDIZED LAB MEASUREMENT VALUES!
# Using raw lab values instead, they seemed like an over-simplified discretization of a continuous/ordinal variable and information would be lost
# "LBSTRESN" -> "LBORRES"
tmp.tr = lv.tr[lv.tr[,"LBBLFL"]=="Y" & lv.tr[,"LBTEST"] %in% lvmark,c("RPT", "LBTEST", "LBORRES")]
tmp.lb = lv.lb[lv.lb[,"LBBLFL"]=="Y" & lv.lb[,"LBTEST"] %in% lvmark,c("RPT", "LBTEST", "LBORRES")]
tmp.vl = lv.vl[lv.vl[,"LBBLFL"]=="Y" & lv.vl[,"LBTEST"] %in% lvmark,c("RPT", "LBTEST", "LBORRES")]
lv <- rbind(tmp.tr, tmp.lb, tmp.vl)
#lv[sample(1:2070, 7),]
# Construct data-matrix like structure of baseline measurements
lvvars <- do.call("rbind", lapply(rownames(ct), FUN=function(z){
	w <- which(lv[,"RPT"]==z)
	mat <- lv[w,]
	vec <- rep(NA, times=length(lvmark))
	for(i in 1:length(lvmark)){
		w <- which(mat[,"LBTEST"]==lvmark[i])
		if(length(w)>0) vec[i] <- mat[w,"LBORRES"]
	}
	vec <- as.numeric(vec)
	names(vec) <- lvmark
	vec
}))
rownames(lvvars) <- rownames(ct)
# Rename for convenience
lvmark[lvmark=="HEMATOCRIT"] <- "HEMAT"
lvmark[lvmark=="SPECIFIC GRAVITY"] <- "SPEGRA"
lvmark[lvmark=="LYMPHOCYTES/LEUKOCYTES"] <- "LYMperLEU"
lvmark[lvmark=="MONOCYTES"] <- "MONO"
lvmark[lvmark=="MONOCYTES/LEUKOCYTES"] <- "MONOperLEU"
lvmark[lvmark=="NEUTROPHILS/LEUKOCYTES"] <- "NEUperLEU"
lvmark[lvmark=="POTASIUM"] <- "POT"
lvmark[lvmark=="BASOPHILS/LEUKOCYTES"] <- "BASOperLEU"
lvmark[lvmark=="EOSINOPHILS"] <- "EOS"
lvmark[lvmark=="EOSINOPHILS/LEUKOCYTES"] <- "EOSperLEU"
colnames(lvvars) <- lvmark
# Only exception is hematocrit: it is XX.X% in CELG, while, 0.XX form in AZ; divide CELG by 100
lvvars[grep("CELG", rownames(lvvars)),"HEMAT"] <- lvvars[grep("CELG", rownames(lvvars)),"HEMAT"]/100
# New variables
newLV <- c("HEMAT","SPEGRA","LYMperLEU","MONO","MONOperLEU","NEUperLEU","POT","BASOperLEU","EOS","EOSperLEU")
newVS <- c("SYSTOLICBP","DIASTOLICBP","PULSE")
  
  

	#########
	######### Phase C: Imputation and constructing of a feasible data matrix
	#########

# In following steps the 'glmnet'-package is required
library(glmnet)

x <- data.frame(binbatches, nummat, vsvars, lvvars, binmat, ordmat, catmat)
y <- respmatrix

# Different types of missingness was present, some appear MAR while others are structured (mainly batch-based block of missingness)

print("Missing %s")
missingness <- apply(x, MARGIN=2, FUN=function(z) sum(is.na(z))/length(z))
missingness[missingness>0]
#> missingness[missingness>0]
#        BMI    HEIGHTBL    WEIGHTBL         ALP         ALT         AST          CA 
#0.005314010 0.003381643 0.005797101 0.003381643 0.003864734 0.007729469 0.006280193 
#      CREAT          HB         LDH         NEU         PLT         PSA       TBILI 
#0.002415459 0.008212560 0.297101449 0.012077295 0.014975845 0.011111111 0.012077295 
#      TESTO         WBC      CREACL         NA.          MG        PHOS         ALB 
#0.414975845 0.008212560 0.729468599 0.233333333 0.247342995 0.244444444 0.239130435 
#       TPRO         RBC         LYM         BUN        CCRC         GLU  SYSTOLICBP 
#0.244444444 0.525603865 0.526086957 0.675845411 0.554106280 0.464251208 0.239130435 
#DIASTOLICBP       PULSE       HEMAT      SPEGRA   LYMperLEU        MONO  MONOperLEU 
#0.239130435 0.526086957 0.526570048 0.614975845 0.526086957 0.526086957 0.526086957 
#  NEUperLEU         POT  BASOperLEU         EOS   EOSperLEU 
#0.526086957 0.234299517 0.526086957 0.526086957 0.526086957

# Which are MAR missing, which are structurally imputed now

# Practically all numeric variables are failing Shapiro-Wilk normality test
# Based on visualizations, the following variables will be log transformed before further processing
numsToLog <- c("ALP", "ALT", "AST", "CREAT", "LDH", "NEU", "PSA", "TBILI", "TESTO", "WBC", "CREACL", "MG", "PHOS", "LYM", "BUN", "CCRC", "GLU")

# Equal to zero measurements in PSA and TESTO; adding smallest non-zero so these won't go to -Inf in log-trans
#[1] "PSA"
#[1] 1
#[1] "TESTO"
#[1] 8
# Replace zero measurements of PSA and TESTO with the smallest non-zero in data matrix x
x[which(x[,"PSA"]==0),"PSA"] <- min(x[which(!x[,"PSA"]==0),"PSA"])
x[which(x[,"TESTO"]==0),"TESTO"] <- min(x[which(!x[,"TESTO"]==0),"TESTO"])

# Transform numsToLog to have better distributional characteristics in many of the variables
for(i in numsToLog){
	x[,i] = log(x[,i])
}

# Make all variable types are correct
#> which(!unlist(lapply(x, FUN=is.numeric)))
# ECOG_C AGEGRP2 
#    102     103
x[,"ECOG_C"] <- as.numeric(x[,"ECOG_C"])
x[,"AGEGRP2"] <- as.numeric(x[,"AGEGRP2"])

# MAR-imputations
# more than 0% missing values but less than 2%
#> missingness[missingness<0.02 & missingness>0]
#        BMI    HEIGHTBL    WEIGHTBL         ALP         ALT         AST          CA 
#0.005314010 0.003381643 0.005797101 0.003381643 0.003864734 0.007729469 0.006280193 
#      CREAT          HB         NEU         PLT         PSA       TBILI         WBC 
#0.002415459 0.008212560 0.012077295 0.014975845 0.011111111 0.012077295 0.008212560


mar.glmnet.imputation = function(
	# Whole data matrix x
	x,
	# The fields to impute, presumably MAR
	# All of these ought to be numeric fields (due to type="gaussian" glmnet)
	impute = c("BMI", "HEIGHTBL", "WEIGHTBL", "ALP", "ALT", "AST", "CA",
		"CREAT", "HB", "NEU", "PLT", "PSA", "TBILI", "WBC", "ECOG_C"),
	# Fields to use as explaining variables
	vars,
	# The sequence of alpha values to test
	#alpha = seq(from=0, to=1, by=0.025),
	alpha = seq(from=0, to=1, by=0.25),
	# Number of lambda sequences in each run
	nlamb = 100,
	# Additional parameters
	...
){
	xtemp <- x
	library(glmnet)	
	for(i in impute){
		varstemp <- vars
		varstemp <- varstemp[!varstemp==i]
		# Missing values in column in x to impute
		ymissing <- is.na(x[,i])
		# Non-complete rows in data matrix x
		xmissing <- apply(x[,varstemp], MARGIN=1, FUN=function(z) any(is.na(z)))
		
		# Construct the teaching data
		xteach <- x[!(ymissing | xmissing),varstemp]
		#xteach <- xteach[,!colnames(xteach) %in% i]
		yteach <- x[!(ymissing | xmissing),i]
		# Predicting matrix for the missing instances
		xpred <- x[ymissing,varstemp]
		
		# Lambda sequence is provided by glmnet itself, it determined a suitable range
		# Previously I tried to provide my own lambda sequence but it was rather inefficient
		tmp <- lapply(alpha, FUN=function(a){
			# Fit model and perform conventional 10-fold CV
			fit <- glmnet(y=yteach, x=as.matrix(xteach), family="gaussian", alpha=a, nlambda=nlamb)
			cvfit <- cv.glmnet(y=yteach, x=as.matrix(xteach), family="gaussian", alpha=a, lambda=fit$lambda)
			# Collect relevant results
			dat <- data.frame(lambda = cvfit$lambda, cvm = cvfit$cvm, cvup = cvfit$cvup, cvlo = cvfit$cvlo, alpha=a, lambda.min=cvfit$lambda.min, lambda.1se=cvfit$lambda.1se)
			list(fit, dat)
		})
		optimal <- which.min(unlist(lapply(tmp, FUN=function(z) min(z[[2]]$cvm))))
		print(paste("Optimal alpha for variable",i,"was",alpha[optimal], "and best lambda", tmp[[optimal]][[2]][1,"lambda.1se"]))
		prediction <- predict(object=tmp[[optimal]][[1]], newx = as.matrix(xpred), s=tmp[[optimal]][[2]][1,"lambda.1se"])
		xtemp[which(ymissing),i] <- prediction
	}
	xtemp
}

set.seed(1)
# Predict MAR missingess using a normal distributiona approximation in glmnet
# Alpha is varied between 0 and 1 with a grid sequence by 0.01, and 1000 length lambda sequence per tested alpha
# Predictions are then imputed to these MAR missing positions
x2 <- mar.glmnet.imputation(x=x, vars=c(colnames(binbatches), bins, ords, cats), alpha = seq(from=0, to=1, by=0.01), nlamb=1000)

# Generally, LASSO-like characteristics were favored
#[1] "Optimal alpha for variable BMI was 0.97 and best lambda 0.270112701126894"
#[1] "Optimal alpha for variable HEIGHTBL was 0.37 and best lambda 1.39300936833102"
#[1] "Optimal alpha for variable WEIGHTBL was 0.48 and best lambda 2.02373331760189"
#[1] "Optimal alpha for variable ALP was 0.77 and best lambda 0.0872085985855768"
#[1] "Optimal alpha for variable ALT was 0.28 and best lambda 0.304708601748881"
#[1] "Optimal alpha for variable AST was 0.45 and best lambda 0.136809264929211"
#[1] "Optimal alpha for variable CA was 0.42 and best lambda 0.047635041435525"
#[1] "Optimal alpha for variable CREAT was 0.19 and best lambda 0.0788304606354662"
#[1] "Optimal alpha for variable HB was 0.99 and best lambda 0.095303561939036"
#[1] "Optimal alpha for variable NEU was 0.81 and best lambda 0.032385333572538"
#[1] "Optimal alpha for variable PLT was 0.66 and best lambda 17.9029322368897"
#[1] "Optimal alpha for variable PSA was 0.77 and best lambda 0.0951952740494295"
#[1] "Optimal alpha for variable TBILI was 0.08 and best lambda 0.231248588661164"
#[1] "Optimal alpha for variable WBC was 0.93 and best lambda 0.0279285657856313"
#[1] "Optimal alpha for variable ECOG_C was 0.1 and best lambda 0.153752160333506"

# The only ordinal prediction was a single value for ECOG_C; round this to an integer
x2[,"ECOG_C"] <- round(x2[,"ECOG_C"],0)

# Structurally missing variables; most likely due to a whole study missing the measurement value
miss2 <- apply(x2, MARGIN=2, FUN=function(z) sum(is.na(z))/length(z))
names(miss2[miss2>0])
#> names(miss2[miss2>0])
# [1] "LDH"         "TESTO"       "CREACL"      "NA."         "MG"          "PHOS"       
# [7] "ALB"         "TPRO"        "RBC"         "LYM"         "BUN"         "CCRC"       
#[13] "GLU"         "SYSTOLICBP"  "DIASTOLICBP" "PULSE"       "HEMAT"       "SPEGRA"     
#[19] "LYMperLEU"   "MONO"        "MONOperLEU"  "NEUperLEU"   "POT"         "BASOperLEU" 
#[25] "EOS"         "EOSperLEU"

# Define a few handy helper functions for creating pair-wise variables to add some non-linearity to glmnet predictions

# Computes all pairwise interactions, includes raising a single column to its 2nd power (interaction with its self)
all.int <- function(input){
	output <- do.call("cbind", lapply(1:ncol(input), FUN=function(z){ 
		do.call("cbind", lapply(z:ncol(input), FUN=function(x){
			tmp <- data.frame(input[,z] * input[,x])
			colnames(tmp)[1] <- paste(colnames(input)[z], "x", colnames(input)[x], sep="")
			tmp
		}))
	}))
	output
}
# Compute only interactions chosen from two vectors
part.int <- function(input, first, second){
	output <- do.call("cbind", lapply(first, FUN=function(z){ 
		do.call("cbind", lapply(second, FUN=function(x){
			tmp <- data.frame(input[,z] * input[,x])
			colnames(tmp)[1] <- paste(z, "x", x, sep="")
			tmp
		}))
	}))
	output
}


# Imputation of structured missingness
# emphasis on more complex interactions and utilizing
struct.glmnet.imputation = function(
	# Whole data matrix x
	x,
	# Fields to impute; all of these ought to be numeric fields (due to type="gaussian" glmnet)
	impute,
	# Fields to use as explaining variables
	vars,
	# The sequence of alpha values to test
	alpha = seq(from=0, to=1, by=0.25),
	# Number of lambda sequences in each run
	nlamb = 100,
	# Additional parameters
	...
){
		
	xtemp <- x
	library(glmnet)	
	for(i in impute){
		varstemp <- vars
		varstemp <- varstemp[!varstemp==i]
		# Missing values in column in x to impute
		ymissing <- is.na(x[,i])
		# Non-complete rows in data matrix x
		xmissing <- apply(x[,varstemp], MARGIN=1, FUN=function(z) any(is.na(z)))
		
		# Construct the teaching data
		xteach <- x[!(ymissing | xmissing),varstemp]
		#xteach <- xteach[,!colnames(xteach) %in% i]
		yteach <- x[!(ymissing | xmissing),i]
		# Predicting matrix for the missing instances
		xpred <- x[ymissing,varstemp]
		
		# Lambda sequence is provided by glmnet itself, it determines a suitable range
		# Previously I tried to provide my own lambda sequence but it was rather inefficient
		tmp <- lapply(alpha, FUN=function(a){
			# Fit model and perform conventional 10-fold CV
			fit <- glmnet(y=yteach, x=as.matrix(xteach), family="gaussian", alpha=a, nlambda=nlamb)
			cvfit <- cv.glmnet(y=yteach, x=as.matrix(xteach), family="gaussian", alpha=a, lambda=fit$lambda, type.measure="mse")
			# Collect relevant results
			dat <- data.frame(lambda = cvfit$lambda, cvm = cvfit$cvm, cvup = cvfit$cvup, cvlo = cvfit$cvlo, alpha=a, lambda.min=cvfit$lambda.min, lambda.1se=cvfit$lambda.1se)
			list(fit, dat)
		})
		optimal <- which.min(unlist(lapply(tmp, FUN=function(z) min(z[[2]]$cvm))))
		print(Sys.time())
		print(paste("Optimal alpha for variable",i,"was",alpha[optimal], "and best lambda", tmp[[optimal]][[2]][1,"lambda.1se"]))
		prediction <- predict(object=tmp[[optimal]][[1]], newx = as.matrix(xpred), s=tmp[[optimal]][[2]][1,"lambda.1se"])
		xtemp[which(ymissing),i] <- prediction
	}
	xtemp
}

# Numerical fields for structural imputation due to missing values
numMiss <- names(miss2[miss2>0])
#> numMiss
# [1] "LDH"         "TESTO"       "CREACL"      "NA."         "MG"          "PHOS"        "ALB"         "TPRO"        "RBC"         "LYM"         "BUN"        
#[12] "CCRC"        "GLU"         "SYSTOLICBP"  "DIASTOLICBP" "PULSE"       "HEMAT"       "SPEGRA"      "LYMperLEU"   "MONO"        "MONOperLEU"  "NEUperLEU"  
#[23] "POT"         "BASOperLEU"  "EOS"         "EOSperLEU"

# Numerical fields common enough after MAR imputation using glmnet
numsImpute <- nums[!nums %in% numMiss]
#> nums[!nums %in% numMiss]
# [1] "BMI"      "HEIGHTBL" "WEIGHTBL" "ALP"      "ALT"      "AST"      "CA"       "CREAT"    "HB"       "NEU"      "PLT"      "PSA"      "TBILI"    "WBC"
 
# Create interactions for predicting the structured missingness
# All pair-wise interactions of numerical variables
x2a <- all.int(x2[,numsImpute])
# Pair-wise interactions where first is a numerical variable and second is a binary indicator variable
x2b <- part.int(x2, first=bins, second=numsImpute)
# Collect the original variables as well as the indicators
x3 <- data.frame(x2, x2a, x2b)

# Structured imputation; explaining variables are anything but those with missingness remaining
# The alpha-sequence is combed with a little less strict grid now, with sequence every 0.025 value ranging between [0,1] due to time constraints
set.seed(1)
x4 <- struct.glmnet.imputation(x=x3, impute=numMiss, vars=colnames(x3)[!colnames(x3) %in% numMiss], alpha = seq(from=0, to=1, by=0.025), nlamb=100)
# Above run is time consuming

# Ignore interactions etc for the time being and output a data matrix with only main variables present
x5 <- x4[,colnames(x)]

# [1] "Optimal alpha for variable LDH was 0.6 and best lambda 0.0506984559781992"
# [1] "Optimal alpha for variable TESTO was 1 and best lambda 0.336771626091876"
# [1] "Optimal alpha for variable CREACL was 0.75 and best lambda 0.0376921779399435"
# [1] "Optimal alpha for variable NA. was 0.35 and best lambda 0.973914049871393"
# [1] "Optimal alpha for variable MG was 0.075 and best lambda 0.343775436999462"
# [1] "Optimal alpha for variable PHOS was 0.975 and best lambda 0.0321812874111257"
# [1] "Optimal alpha for variable ALB was 0.775 and best lambda 0.278937558384935"
# [1] "Optimal alpha for variable TPRO was 0.65 and best lambda 0.663900086223724"
# [1] "Optimal alpha for variable RBC was 1 and best lambda 0.0533157136571461"
# [1] "Optimal alpha for variable LYM was 0.95 and best lambda 0.0196887721328639"
# [1] "Optimal alpha for variable BUN was 0.675 and best lambda 0.0706659996510768"
# [1] "Optimal alpha for variable CCRC was 0.8 and best lambda 0.0143849928580573"
# [1] "Optimal alpha for variable GLU was 0.575 and best lambda 0.0612715544863575"
# [1] "Optimal alpha for variable SYSTOLICBP was 0.85 and best lambda 2.62360193632496"
# [1] "Optimal alpha for variable DIASTOLICBP was 0.4 and best lambda 1.68846314809189"
# [1] "Optimal alpha for variable PULSE was 0.75 and best lambda 1.31954307481627"
# [1] "Optimal alpha for variable HEMAT was 0.925 and best lambda 0.00143375747983707"
# [1] "Optimal alpha for variable SPEGRA was 0.2 and best lambda 0.00919581004356266"
# [1] "Optimal alpha for variable LYMperLEU was 1 and best lambda 0.282559516482283"
# [1] "Optimal alpha for variable MONO was 0.925 and best lambda 0.0119929119339898"
# [1] "Optimal alpha for variable MONOperLEU was 1 and best lambda 0.121241772410709"
# [1] "Optimal alpha for variable NEUperLEU was 0.975 and best lambda 0.368662096490355"
# [1] "Optimal alpha for variable POT was 0.125 and best lambda 0.247176156530969"
# [1] "Optimal alpha for variable BASOperLEU was 0.825 and best lambda 0.0702786638840255"
# [1] "Optimal alpha for variable EOS was 0.4 and best lambda 0.0669634078475612"
# [1] "Optimal alpha for variable EOSperLEU was 0.975 and best lambda 0.205082230588696"

# Remove few variables from x:
toRemove <- c("isASC", "isAZ", "isCELG", "isEFC")
x5 <- x5[,-which(colnames(x5) %in% toRemove)]

	#########
	######### Phase D: Process Y a bit
	#########


# Add extra columns to 'y'
# Batches
z <- as.factor(substr(rownames(y), start=1, stop=2))
# Elaborate on the coding
levels(z)[which(levels(z)=="AS")] <- "ASC"
levels(z)[which(levels(z)=="AZ")] <- "AZ"
levels(z)[which(levels(z)=="CE")] <- "CEL"
levels(z)[which(levels(z)=="VE")] <- "EFC"
y <- data.frame(y)
y[,"Batch"] <- z

# Annotate training 'tr', validation 'vl', and leaderboards 'lb'
y[,"tr"] <- 0
y[,"lb"] <- 0
y[,"vl"] <- 0
y[ct.tr[,"RPT"],"tr"] <- 1
y[ct.vl[,"RPT"],"vl"] <- 1
y[ct.lb[,"RPT"],"lb"] <- 1

# Create a pre-defined color coding for the individuals per each study to highlight batch differences
y[,"col"] <- "black"
y[y[,"Batch"]=="ASC","col"] <- "red"
y[y[,"Batch"]=="CEL","col"] <- "blue"
y[y[,"Batch"]=="EFC","col"] <- "green"

	#########
	######### Phase E: Diagnostics and finishing touch on the data matrix x
	#########

pdf("NumVars_batchwise_distributions.pdf", width=6, height=6)
dump <- lapply(1:ncol(x5), FUN=function(z){
	if(!all(x5[,z] %in% c(0,1,2,3))){		
		boxplot(x5[,z] ~ y[,"Batch"], pch=16, range=0, main=colnames(x5)[z])
	}else{
		print(paste("Variable", colnames(x5)[z]))
		print(table(Batch = y[,"Batch"], Response = x5[,z]))
	}
})
dev.off()

# Some features, e.g. ECOG_C, were skewed ordinal; no presence of ECOG_C>=2 in AZ
#[1] "Variable ECOG_C"
#     Response
#Batch   0   1   2   3
#  ASC 220 234  22   0
#  AZ  247 223   0   0
#  CEL 258 247  20   1
#  EFC 280 291  27   0

# z-score normalization
x6 <- scale(x5)

pdf("PC1_y_plots.pdf", width=10, height=5)
par(mfrow=c(1,2), mar=c(4,4,1,1))
# PCA of all basic data matrix x variables
x6pca <- prcomp(x6)
plot(x=y[,"LKADT_P"], y=x6pca$x[,1], pch=ifelse(y[,"DEATH"], 16, 1), col=y[,"col"], xlim=c(-300,1600), xlab="Response q1", ylab="PC1", main="y vs PC1, all variables", cex=0.7)
set.seed(1)
points(x=jitter(rep(-200, time=sum(y[,"Batch"]=="AZ")), factor=30), y=x6pca$x[y[,"Batch"]=="AZ",1], col="black", pch=16, cex=0.7)
abline(v=0, col="grey", lwd=2)
abline(v=357, col="red"); abline(v=279, col="blue"); abline(v=642.5, col="green"); abline(v=463, col="black")
legend("bottomright", lwd=1, col="black", legend="Median batch follow-up time", cex=0.8)

# PCA of binary data matrix x variables
x7pca <- prcomp(x6[,names(which(apply(x6, MARGIN=2, FUN=function(z) length(unique(z))==2)))])
plot(x=y[,"LKADT_P"], y=x7pca$x[,1], pch=ifelse(y[,"DEATH"], 16, 1), col=y[,"col"], xlim=c(-300,1600), xlab="Response q1", ylab="PC1", main="y vs PC1, only binary variables", cex=0.7)
set.seed(1)
points(x=jitter(rep(-200, time=sum(y[,"Batch"]=="AZ")), factor=30), y=x7pca$x[y[,"Batch"]=="AZ",1], col="black", pch=16, cex=0.7)
abline(v=0, col="grey", lwd=2)
legend("bottomright", col=c("red","blue", "green", "black"), pch=16, legend=c("ASC", "CEL", "EFC", "AZ"), cex=0.8)
legend("topright", pch=c(16,1), col="black", legend=c("Observed death", "Right censored"), cex=0.8)
abline(v=357, col="red"); abline(v=279, col="blue"); abline(v=642.5, col="green"); abline(v=463, col="black")
dev.off()


# Values for determining weights
batch_pc1 <- by(x6pca$x[,1], INDICES=y[,"Batch"], FUN=mean)
batch_pc2 <- by(x6pca$x[,2], INDICES=y[,"Batch"], FUN=mean)
batch_flw <- by(y[,"LKADT_P"], INDICES=y[,"Batch"], FUN=median)
# PCA 1st & 2nd component
pdf("PCA_batches.pdf", width=6, height=6)
plot(x=x6pca$x[,1], y=x6pca$x[,2], xlab="PC1", ylab="PC2", col=y[,"col"], cex=0.8, pch=16, main="Batchwise principal components")
points(x=batch_pc1["ASC"], y=batch_pc2["ASC"], col="red", pch=15, cex=2)
points(x=batch_pc1["ASC"], y=batch_pc2["ASC"], col="grey", pch=22, cex=2)
points(x=batch_pc1["CEL"], y=batch_pc2["CEL"], col="blue", pch=15, cex=2)
points(x=batch_pc1["CEL"], y=batch_pc2["CEL"], col="grey", pch=22, cex=2)
points(x=batch_pc1["EFC"], y=batch_pc2["EFC"], col="green", pch=15, cex=2)
points(x=batch_pc1["EFC"], y=batch_pc2["EFC"], col="grey", pch=22, cex=2)
points(x=batch_pc1["AZ"], y=batch_pc2["AZ"], col="black", pch=15, cex=2)
points(x=batch_pc1["AZ"], y=batch_pc2["AZ"], col="grey", pch=22, cex=2)
legend("bottomleft", pch=c(15, 16), col="black", legend = c("Mean of batchwise-PC", "PC observation"), cex=0.8)
legend("topleft", pch=16, col=c("red", "blue", "green", "black"), legend=c("ASC", "CEL", "EFC", "AZ"), cex=0.8)
dev.off()



### FIRST PRINCIPAL COMPONENT
#> by(x6pca$x[,1], INDICES=y[,"Batch"], FUN=mean)
#y[, "Batch"]: ASC
#[1] -0.8839712
#------------------------------------------------------------------- 
#y[, "Batch"]: AZ
#[1] 0.8251201
#------------------------------------------------------------------- 
#y[, "Batch"]: CEL
#[1] 0.4805046
#------------------------------------------------------------------- 
#y[, "Batch"]: EFC
#[1] -0.3675277

### SECOND PRINCIPAL COMPONENT
#> by(x6pca$x[,2], INDICES=y[,"Batch"], FUN=mean)
#y[, "Batch"]: ASC
#[1] 0.2851116
#------------------------------------------------------------------- 
#y[, "Batch"]: AZ
#[1] -0.5841717
#------------------------------------------------------------------- 
#y[, "Batch"]: CEL
#[1] 0.5091164
#------------------------------------------------------------------- 
#y[, "Batch"]: EFC
#[1] -0.2156315

### MEDIAN FOLLOW-UP TIME
#> by(y[,"LKADT_P"], INDICES=y[,"Batch"], FUN=median)
#y[, "Batch"]: ASC
#[1] 357
#------------------------------------------------------------------- 
#y[, "Batch"]: AZ
#[1] NA <<<--- THIS MEDIAN KNOWN TO BE 463
#------------------------------------------------------------------- 
#y[, "Batch"]: CEL
#[1] 279
#------------------------------------------------------------------- 
#y[, "Batch"]: EFC
#[1] 642.5


	#########
	######### Phase F: Wrapping up preprocessing and imputation, outputting processed data matrices
	#########


# Set a suitable working folder
#setwd("D:\\Work\\PhD_2015\\DREAM\\final\\RData\\")

# Save the R workspace
save.image("step0_workspace.RData")

# Write the final data matrix X for which imputations and z-score transformation have been performed
# raw version
write.csv(x5, file="x_final.csv", quote=F, row.names=T)

# Write out the response vectors/matrix
write.csv(y, "y_final.csv", quote=F, row.names=T)

