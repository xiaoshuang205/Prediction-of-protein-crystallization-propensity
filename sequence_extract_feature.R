library(glmnet)
data <- read.csv("t_cf_Geary .csv")
data
X<-data[,2:-1]
X <- as.matrix()
library("protr")
CFTR1 <- readFASTA("CF_TE_Sequence.fasta")

CFTR1



#sequence extract feature
#install.packages("protr")
#install.packages("ftrCOOL")
#install.packages("ragp")
library("protr")
#library("ftrCOOL")
#library(ragp)
#library(stats)
# load FASTA files
#install.packages("remotes")
#remotes::install_github("missuse/ragp")
CFTRLabel <- readFASTA("CF_TE_Label")
write.csv(x = CFTRLabel,file = "cf_test_label .csv")
#dir = tempdir()
#ptmSeqsADR<-system.file("extdata/",package="ftrCOOL")
#filePr<-system.file("extdata/protein.fasta",package="ftrCOOL")
#filePrs<-system.file("extdata/proteins.fasta",package="ftrCOOL")

#ad<-paste0(dir,"/blosum62.txt")
#vect<-BLOSUM62(seqs = filePr,outFormat="mat")
#vext1<-BLOSUM62(seqs = CFTR,outFormat="txt",outputFileDist="result")

#BLOSUM62(seqs = filePrs,outFormat="txt",outputFileDist=ad)

#unlink("dir", recursive = TRUE)
#extracell
#mitonchon
CFTR <- CFTR[(sapply(CFTR, protcheck))]
#CFTRLabel <- readFASTA("CF_TE_Label")
#write.csv(x = CFTRLabel,file = "cf_test_label .csv")
#length(CFTR)
#length(CFTRLabel)
# calculate APseAAC descriptors
xn <- t(sapply(CFTR,extractMoreauBroto))
write.csv(x = xn,file = "t_cf_Normalized Moreau-Broto .csv")
xn1 <- t(sapply(CFTR,extractTC))
write.csv(x = xn1,file = "t_cf_Tripeptide composition .csv")
xn2 <- t(sapply(CFTR,extractMoran))
write.csv(x = xn2,file = "t_cf_Moran .csv")
xn3 <- t(sapply(CFTR,extractGeary))
write.csv(x = xn3,file = "t_cf_Geary .csv")





x1 <- t(sapply(CFTR, extractAAC))



#x1
x2 <- t(sapply(CFTR, extractDC))
#x2
x3 <- t(sapply(CFTR, extractPAAC))
#x3
x4 <- t(sapply(CFTR, extractPSSM(CFTR,database.path ="D:/Program Files/NCBI/blast-2.12.0+/db/swissprot" )))

write.csv(x = x1,file = "amino acid composition (AAC) .csv")
write.csv(x = x2,file = " dipeptide composition (DPC).csv")
write.csv(x = x3,file = "pseudo-amino acid composition (PseAAC) .csv")



x <- rbind(x1, x2)
x
k <- c(rep(0, length(extracell)), rep(1, length(mitonchon)))
k
# make class labels
labels <- as.factor(c(rep(0, length(extracell)), rep(1, length(mitonchon))))
labels
set.seed(1001)

# split training and test set
tr.idx <- c(sample(1:nrow(x1), round(nrow(x1) * 0.75)),sample(nrow(x1) + 1:nrow(x2), round(nrow(x2) * 0.75)))
tr.idx
te.idx <- setdiff(1:nrow(x), tr.idx)

x.tr <- x[tr.idx, ]
x.te <- x[te.idx, ]
y.tr <- labels[tr.idx]
y.te <- labels[te.idx]