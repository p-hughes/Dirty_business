##setwd("C:/Users/bmal4866/Desktop")
##s_text<-read.table("s_text.txt",header=T)
#s_text<-read.table("texture.txt",header=T)
#s_text<-read.table("text390k_2.txt",sep=",",header=T)
#setwd("C:/Users/bmal4866/Desktop/TXTBIN")
setwd("C:/Users/phug7649/Desktop/txtbin")
##text390k_2 ## another text file i was about to work on before
##s_text<-read.table("399772.txt",sep=",",header=T)
##s_text<-read.table("text_ndup.txt",sep=",",header=T)
#s_text<-read.table("text_nodup_175716.txt",sep=",",header=T)
s_text <- read.table("Subset of Textures_161193.txt", sep=",", header=T)

str(s_text)
head(s_text)
t_text<- as.character(s_text$S_text)
str(s_text)
##?grep
##Starting to figure out the grep command, not essential########################
#setInternet2(use=TRUE)
#grep("[a-z]", letters)
#grep("[a-z]", LETTERS)
#grep("[A-Z]", letters)
#grep("[A-Z]", LETTERS)
#grep("[A-Z]",t_text)
#grep("ASHY",t_text)
#subset(data, data[[2]] %in% c("Company Name 09", "Company Name"), drop = TRUE)
#subset(data, grepl("^Company Name", data[[2]]), drop = TRUE)
#getwd()
#headings<-read.table("Headings.txt",header=T)
#subset(headings, headings[[1]] %WC% c("headings", "Company Name"), drop = TRUE)
#subset(data, grepl("^Company Name", data[[2]]), drop = TRUE)

#w <- grep("NOM$", headings$HEADINGS)
#headings[w,]<-"NOM"
#x <- grep("WC$", headings$HEADINGS)
#headings[x,]<-"WC"

#y <- c("WC", "BG", "NOM", "HD")

#for(i in y){
# w <- grep(paste(i, "$", sep=""), headings$HEADINGS)
# headings[w,]<-i
#}

################################################################################


##ASHY

n<-grep("ASHY?-", s_text[,1])
min_ASH <- gsub("ASHY?-", "", s_text[,1])

##BR

n<-grep("-?BR", min_ASH)
min_ASH_BR <- gsub("-?BR", "", min_ASH)
table(as.character(min_ASH[n]))
table(as.character(min_ASH_BR[n]))
##Check to see if the expression removes what you want

##BYV

n<-grep("BYV-?", min_ASH_BR)
min_ASH_BR_BYV <- gsub("BYV-?", "", min_ASH_BR)
table(as.character(min_ASH_BR[n]))
table(as.character(min_ASH_BR_BYV[n]))

##BYX

n<-grep("BYX-?", min_ASH_BR)
min_N_BYX <- gsub("BYX-?", "", min_ASH_BR_BYV)
table(as.character(min_ASH_BR_BYV[n]))
table(as.character(min_N_BYX[n]))

##BY

n<-grep("BY-?", min_N_BYX)
min_N_BY <- gsub("BY-?", "", min_N_BYX)
table(as.character(min_ASH_BR_BYV[n]))
table(as.character(min_N_BY[n]))

##1st iteration made min_N_BY <- min_N_BYX
##CBV is next

n<-grep("CBV-?", min_N_BYX)
min_N_CBV <- gsub("CBV-?", "", min_N_BY)
table(as.character( min_N_BY[n]))
table(as.character(min_N_CBV[n]))

##CBX

n<-grep("CBX-?", min_N_CBV)
min_N_CBX <- gsub("CBX-?", "", min_N_CBV)
table(as.character( min_N_CBV[n]))
table(as.character(min_N_CBX[n]))

##CB

n<-grep("CB-?", min_N_CBX)
min_N_CB <- gsub("CB-?", "", min_N_CBX)
table(as.character( min_N_CB[n]))
table(as.character(min_N_CBX[n]))

##CEM

n<-grep("CEM-?", min_N_CB)
min_N_CEM <- gsub("CEM-?", "", min_N_CB)
table(as.character( min_N_CB[n]))
table(as.character(min_N_CEM[n]))

##CE          min_N_CE

n<-grep("CE-?", min_N_CEM)
min_N_CE <- gsub("CE-?", "", min_N_CEM)
table(as.character( min_N_CE[n]))
table(as.character(min_N_CEM[n]))

##CNX

n<-grep("CNX-?", min_N_CE)
min_N_CNX <- gsub("CNX-?", "", min_N_CE)
table(as.character( min_N_CE[n]))
table(as.character(min_N_CNX[n]))

##CNV

n<-grep("CNV-?", min_N_CNX)
min_N_CNV <- gsub("CNV-?", "", min_N_CNX)
table(as.character( min_N_CNV[n]))
table(as.character(min_N_CNX[n]))

##CN

n<-grep("CN-?", min_N_CNV)
min_N_CN <- gsub("CN-?", "", min_N_CNV)
table(as.character( min_N_CN[n]))
table(as.character(min_N_CNV[n]))

##COS-S

n<-grep("COS-?", min_N_CN)
min_N_COS_S <- gsub("COS-?", "S", min_N_CN)
table(as.character( min_N_COS_S[n]))
table(as.character(min_N_CN[n]))



##CRX

n<-grep("CRX-?", min_N_COS_S)
min_N_CRX <- gsub("CRX-?", "", min_N_COS_S)
table(as.character( min_N_COS_S[n]))
table(as.character(min_N_CRX[n]))

##CRV

n<-grep("CRX-?", min_N_COS_S)
min_N_CRX <- gsub("CRX-?", "", min_N_COS_S)
table(as.character( min_N_COS_S[n]))
table(as.character(min_N_CRX[n]))

##CR

n<-grep("CR-?", min_N_CRX)
min_N_CR <- gsub("CR-?", "", min_N_CRX)
table(as.character( min_N_CR[n]))
table(as.character(min_N_CRX[n]))

##CY

n<-grep("CY-?", min_N_CR)
min_N_CY <- gsub("CY-?", "C", min_N_CR)
table(as.character( min_N_CR[n]))
table(as.character(min_N_CY[n]))

n<-grep("CC", min_N_CY)
min_N_CC <- gsub("CC", "C", min_N_CY)
table(as.character( min_N_CC[n]))
table(as.character(min_N_CY[n]))

##DE

n<-grep("DE-?", min_N_CC)
min_N_DE <- gsub("DE-?", "", min_N_CC)
table(as.character( min_N_CC[n]))
table(as.character(min_N_DE[n]))

##DIA

n<-grep("DIA-?", min_N_DE)
min_N_DIA <- gsub("DIA-?", "", min_N_DE)
table(as.character( min_N_DIA[n]))
table(as.character(min_N_DE[n]))

##DUR

n<-grep("DUR-?", min_N_DIA)
min_N_DUR <- gsub("DUR-?", "", min_N_DIA)
table(as.character( min_N_DIA[n]))
table(as.character(min_N_DUR[n]))

##FB

n<-grep("FB-?", min_N_DUR)
min_N_FB <- gsub("FB-?", "1", min_N_DUR)
table(as.character( min_N_FB[n]))
table(as.character(min_N_DUR[n]))

##FLX

n<-grep("FLX-?", min_N_FB)
min_N_FLX <- gsub("FLX-?", "", min_N_FB)
table(as.character( min_N_FB[n]))
table(as.character(min_N_FLX[n]))

##FLV

n<-grep("FLV-?", min_N_FLX)
min_N_FLV <- gsub("FLV-?", "", min_N_FLX)
table(as.character( min_N_FLV[n]))
table(as.character(min_N_FLX[n]))

##FL

n<-grep("FL-?", min_N_FLV)
min_N_FL <- gsub("FL-?", "", min_N_FLV)
table(as.character( min_N_FLV[n]))
table(as.character(min_N_FL[n]))

##FM

n<-grep("FM-?", min_N_FL)
min_N_FM <- gsub("FM-?", "", min_N_FL)
table(as.character( min_N_FL[n]))
table(as.character(min_N_FM[n]))

##FRAG

n<-grep("FRAG-?", min_N_FM)
min_N_FRAG <- gsub("FRAG-?", "", min_N_FM)
table(as.character( min_N_FRAG[n]))
table(as.character(min_N_FM[n]))

##FS

n<-grep("FS-?", min_N_FRAG)
min_N_FS <- gsub("FS-?", "S", min_N_FRAG)
table(as.character( min_N_FRAG[n]))
table(as.character(min_N_FS[n]))

## min_N_FS  ???

##GRC

n<-grep("GRC-?", min_N_FS)
min_N_GRC <- gsub("GRC-?", "", min_N_FS)
table(as.character( min_N_GRC[n]))
table(as.character(min_N_FS[n]))

##GRV

n<-grep("GRV-?", min_N_GRC)
min_N_GRV <- gsub("GRV-?", "", min_N_GRC)
table(as.character( min_N_GRC[n]))
table(as.character(min_N_GRV[n]))

##GRX

n<-grep("GRX-?", min_N_GRV)
min_N_GRX <- gsub("GRX-?", "", min_N_GRV)
table(as.character( min_N_GRX[n]))
table(as.character(min_N_GRV[n]))

##GRF

n<-grep("GRF-?", min_N_GRX)
min_N_GRF <- gsub("GRF-?", "", min_N_GRX)
table(as.character( min_N_GRX[n]))
table(as.character(min_N_GRF[n]))

##GR ##CB

n<-grep("GR-?", min_N_GRF)
min_N_GR <- gsub("GR-?", "", min_N_GRF)
table(as.character( min_N_GR[n]))
table(as.character(min_N_GRF[n]))

n<-grep("CB-?", min_N_GR)
min_N_CB <- gsub("CB-?", "", min_N_GR)
table(as.character( min_N_GR[n]))
table(as.character(min_N_CB[n]))

##GR-

##GS-O

n<-grep("GS-?", min_N_CB)
min_N_GS <- gsub("GS-?", "1", min_N_CB)
table(as.character( min_N_GS[n]))
table(as.character(min_N_CB[n]))

##GYP

n<-grep("GYP-?", min_N_GS)
min_N_GYP <- gsub("GYP-?", "", min_N_GS)
table(as.character( min_N_GS[n]))
table(as.character(min_N_GYP[n]))

##GY

n<-grep("GY-?", min_N_GYP)
min_N_GY <- gsub("GY-?", "", min_N_GYP)
table(as.character( min_N_GY[n]))
table(as.character(min_N_GYP[n]))

##G

n<-grep("G-?", min_N_GY)
min_N_G <- gsub("G-?", "", min_N_GY)
table(as.character( min_N_GY[n]))
table(as.character(min_N_G[n]))

##HB

n<-grep("HB-?", min_N_G)
min_N_HB <- gsub("HB-?", "1", min_N_G)
table(as.character( min_N_HB[n]))
table(as.character(min_N_G[n]))

##HE

n<-grep("HE-?", min_N_HB)
min_N_HE <- gsub("HE-?", "1", min_N_HB)
table(as.character( min_N_HB[n]))
table(as.character(min_N_HE[n]))

##HO

n<-grep("HO-?", min_N_HE)
min_N_HO <- gsub("HO-?", "1", min_N_HE)
table(as.character( min_N_HO[n]))
table(as.character(min_N_HE[n]))

##HPM

n<-grep("HPM-?", min_N_HO)
min_N_HPM <- gsub("HPM-?", "1", min_N_HO)
table(as.character( min_N_HO[n]))
table(as.character(min_N_HPM[n]))

##HYDR

n<-grep("HYDR-?", min_N_HPM)
min_N_HYDR <- gsub("HYDR-?", "", min_N_HPM)
table(as.character( min_N_HPM[n]))
table(as.character(min_N_HYDR[n]))

##CIND

n<-grep("CIND-?", min_N_HYDR)
min_N_CIND <- gsub("CIND-?", "", min_N_HYDR)
table(as.character( min_N_CIND[n]))
table(as.character(min_N_HYDR[n]))

##IND

n<-grep("IND-?", min_N_CIND)
min_N_IND <- gsub("IND-?", "", min_N_CIND)
table(as.character( min_N_IND[n]))
table(as.character(min_N_CIND[n]))

################## Maybe I will manually replace these #########################
## I                                                                          ##
## L C-LC                                                                     ##
## L CL-L                                                                     ##
################################################################################

## CBV

n<-grep("CBV-?", min_N_IND)
min_N_CBV <- gsub("CBV-?", "", min_N_IND)
table(as.character( min_N_IND[n]))
table(as.character(min_N_CBV[n]))

##MARL

n<-grep("MARL-?", min_N_CBV)
min_N_MARL <- gsub("MARL-?", "", min_N_CBV)
table(as.character( min_N_MARL[n]))
table(as.character(min_N_CBV[n]))

##MAT

n<-grep("MAT-?", min_N_MARL)
min_N_MAT <- gsub("MAT-?", "", min_N_MARL)
table(as.character( min_N_MARL[n]))
table(as.character(min_N_MAT[n]))

##MEDL

n<-grep("MEDL-?", min_N_MAT)
min_N_MEDL <- gsub("MEDL-?", "", min_N_MAT)
table(as.character( min_N_MEDL[n]))
table(as.character(min_N_MAT[n]))

##MKY      (ORGANIC)

n<-grep("MKY-?", min_N_MEDL)
min_N_MKY <- gsub("MKY-?", "1", min_N_MEDL)
table(as.character( min_N_MEDL[n]))
table(as.character(min_N_MKY[n]))

##MK    (ORGANIC)

n<-grep("MK-?", min_N_MKY)
min_N_MK <- gsub("MK-?", "1", min_N_MKY)
table(as.character( min_N_MK[n]))
table(as.character(min_N_MKY[n]))

##MPT (ORGANIC)

n<-grep("MPT-?", min_N_MKY)
min_N_MPT <- gsub("MPT-?", "1", min_N_MKY)
table(as.character( min_N_MPT[n]))
table(as.character(min_N_MKY[n]))

##MR       (I can use this to link drainage to colour!!!)

n<-grep("MR-?", min_N_MPT)
min_N_MR <- gsub("MR-?", "", min_N_MPT)
table(as.character( min_N_MPT[n]))
table(as.character(min_N_MR[n]))

##MUCK

n<-grep("MUCK-?", min_N_MR)
min_N_MUCK <- gsub("MUCK-?", "1", min_N_MR)
table(as.character( min_N_MUCK[n]))
table(as.character(min_N_MR[n]))

##OR         (Organic?)

n<-grep("OR-?", min_N_MUCK)
min_N_OR <- gsub("OR-?", "1", min_N_MUCK)
table(as.character( min_N_MUCK[n]))
table(as.character(min_N_OR[n]))

##PDOM

n<-grep("PDOM-?", min_N_OR)
min_N_PDOM <- gsub("PDOM-?", "", min_N_OR)
table(as.character( min_N_PDOM[n]))
table(as.character(min_N_OR[n]))

##PEAT

n<-grep("PEAT-?", min_N_PDOM)
min_N_PEAT <- gsub("PEAT-?", "1", min_N_PDOM)
table(as.character( min_N_PDOM[n]))
table(as.character(min_N_PEAT[n]))

##SPM

n<-grep("SPM-?", min_N_PEAT)
min_N_SPM <- gsub("SPM-?", "1", min_N_PEAT)
table(as.character( min_N_SPM[n]))
table(as.character(min_N_PEAT[n]))

##OMPM

n<-grep("OMPM-?", min_N_SPM)
min_N_OMPM <- gsub("OMPM-?", "1", min_N_SPM)
table(as.character( min_N_OMPM[n]))
table(as.character(min_N_SPM[n]))

##MPM

n<-grep("MPM-?", min_N_OMPM)
min_N_MPM <- gsub("MPM-?", "1", min_N_OMPM)
table(as.character( min_N_OMPM[n]))
table(as.character(min_N_MPM[n]))

##PT

n<-grep("PT-?", min_N_MPM)
min_N_PT <- gsub("PT-?", "1", min_N_MPM)
table(as.character( min_N_PT[n]))
table(as.character(min_N_MPM[n]))

##PF

n<-grep("PF-?", min_N_PT)
min_N_PF <- gsub("PF-?", "", min_N_PT)
table(as.character( min_N_PT[n]))
table(as.character(min_N_PF[n]))

##COP

n<-grep("COP-?", min_N_PF)
min_N_COP <- gsub("COP-?", "", min_N_PF)
table(as.character( min_N_COP[n]))
table(as.character(min_N_PF[n]))

##PCX

n<-grep("PCX-?", min_N_COP)
min_N_PCX <- gsub("PCX-?", "", min_N_COP)
table(as.character( min_N_COP[n]))
table(as.character(min_N_PCX[n]))


##P

n<-grep("P-?", min_N_PCX)
min_N_P <- gsub("P-?", "", min_N_PCX)
table(as.character( min_N_P[n]))
table(as.character(min_N_PCX[n]))

################################################################################
## CNX                                                                        ##
## CNV                                                                        ##
## CN                                                                         ##
## CX                                                                         ##
################################################################################

##FLX

n<-grep("FLX-?", min_N_P)
min_N_FLX <- gsub("FLX-?", "", min_N_P)
table(as.character( min_N_P[n]))
table(as.character(min_N_FLX[n]))

##FL

n<-grep("FL-?", min_N_FLX)
min_N_FL <- gsub("FL-?", "", min_N_FLX)
table(as.character( min_N_FL[n]))
table(as.character(min_N_FLX[n]))

##F

n<-grep("F-?", min_N_FL)
min_N_F <- gsub("F-?", "", min_N_FL)
table(as.character( min_N_F[n]))
table(as.character(min_N_FL[n]))


##STV

n<-grep("STV-?", min_N_F)
min_N_STV <- gsub("STV-?", "", min_N_F)
table(as.character( min_N_F[n]))
table(as.character(min_N_STV[n]))

##ART

n<-grep("ART-?", min_N_STV)
min_N_ART <- gsub("ART-?", "", min_N_STV)
table(as.character( min_N_ART[n]))
table(as.character(min_N_STV[n]))

##STX

n<-grep("STX-?", min_N_ART)
min_N_STX <- gsub("STX-?", "", min_N_ART)
table(as.character( min_N_ART[n]))
table(as.character(min_N_STX[n]))

##ST

n<-grep("ST-?", min_N_STX)
min_N_ST <- gsub("ST-?", "", min_N_STX)
table(as.character( min_N_ST[n]))
table(as.character(min_N_STX[n]))

##MK

n<-grep("MK-?", min_N_ST)
min_N_MK <- gsub("MK-?", "1", min_N_ST)
table(as.character( min_N_ST[n]))
table(as.character(min_N_MK[n]))

##T

n<-grep("T-?", min_N_MK)
min_N_T <- gsub("T-?", "", min_N_MK)
table(as.character( min_N_T[n]))
table(as.character(min_N_MK[n]))

##SR

n<-grep("SR-?", min_N_T)
min_N_SR <- gsub("SR-?", "", min_N_T)
table(as.character( min_N_T[n]))
table(as.character(min_N_SR[n]))

##U

n<-grep("U-?", min_N_SR)
min_N_U <- gsub("U-?", "", min_N_SR)
table(as.character( min_N_U[n]))
table(as.character(min_N_SR[n]))

##U U

##DOM

n<-grep("DOM-?", min_N_U)
min_N_DOM <- gsub("DOM-?", "", min_N_U)
table(as.character( min_N_U[n]))
table(as.character(min_N_DOM[n]))

##VAR

n<-grep("VAR-?", min_N_DOM)
min_N_VAR <- gsub("VAR-?", "", min_N_DOM)
table(as.character( min_N_VAR[n]))
table(as.character(min_N_DOM[n]))

##WB

n<-grep("WB-?", min_N_VAR)
min_N_WB <- gsub("WB-?", "", min_N_VAR)
table(as.character( min_N_VAR[n]))
table(as.character(min_N_WB[n]))

##WD

n<-grep("WD-?", min_N_WB)
min_N_WD <- gsub("WD-?", "", min_N_WB)
table(as.character( min_N_WD[n]))
table(as.character(min_N_WB[n]))

##ASHY
n<-grep("ASHY-?", min_N_WD)
min_N_ASHY <- gsub("ASHY-?", "", min_N_WD)
table(as.character( min_N_WD[n]))
table(as.character(min_N_ASHY[n]))

##SH

n<-grep("SH-?", min_N_ASHY)
min_N_SH <- gsub("SH-?", "", min_N_ASHY)
table(as.character( min_N_ASHY[n]))
table(as.character(min_N_SH[n]))

##V

n<-grep("V-?", min_N_SH)
min_N_V <- gsub("V-?", "", min_N_SH)
table(as.character( min_N_SH[n]))
table(as.character(min_N_V[n]))


##SYX

n<-grep("SYX-?", min_N_V)
min_N_SYX <- gsub("SYX-?", "", min_N_V)
table(as.character( min_N_SYX[n]))
table(as.character(min_N_V[n]))

##X

n<-grep("X-?", min_N_SYX)
min_N_X <- gsub("X-?", "", min_N_SYX)
table(as.character( min_N_X[n]))
table(as.character(min_N_SYX[n]))

##min_N_X



table(as.character( min_N_X))

##A-

n<-grep("A-?", min_N_X)
min_N_A <- gsub("A-?", "", min_N_X)
table(as.character( min_N_X[n]))
table(as.character(min_N_A[n]))


##cl-CL

n<-grep("cl-?", min_N_A)
min_N_cl <- gsub("cl-?", "CL", min_N_A)
table(as.character( min_N_A[n]))
table(as.character(min_N_cl[n]))

##l-L

n<-grep("l-?", min_N_cl)
min_N_l <- gsub("l-?", "L", min_N_cl)
table(as.character( min_N_l[n]))
table(as.character(min_N_cl[n]))

##M-

n<-grep("M-?", min_N_l)
min_N_M <- gsub("M-?", "", min_N_l)
table(as.character( min_N_l[n]))
table(as.character(min_N_M[n]))

##O

n<-grep("O-?", min_N_M)
min_N_O <- gsub("O-?", "1", min_N_M)
table(as.character( min_N_O[n]))
table(as.character(min_N_M[n]))

##-

n<-grep("-", min_N_O)
min_N_dash <- gsub("-", "", min_N_O)
table(as.character( min_N_O[n]))
table(as.character(min_N_dash[n]))

##SY

n<-grep("SY-?", min_N_dash)
min_N_SY <- gsub("SY-?", "", min_N_dash)
table(as.character( min_N_SY[n]))
table(as.character(min_N_dash[n]))

table(as.character(min_N_SY))

##Change texture metrics

##CLS-SCL

n<-grep("CLS-?", min_N_SY)
min_N_CLS <- gsub("CLS-?", "SCL", min_N_SY)
table(as.character( min_N_SY[n]))
table(as.character(min_N_CLS[n]))

##CS-SC

n<-grep("CS-?", min_N_CLS)
min_N_CS <- gsub("CS-?", "SC", min_N_CLS)
table(as.character( min_N_CS[n]))
table(as.character(min_N_CLS[n]))

##CSIL-SICL

n<-grep("CSIL-?", min_N_CS)
min_N_CSIL <- gsub("CSIL-?", "SICL", min_N_CS)
table(as.character( min_N_CS[n]))
table(as.character(min_N_CSIL[n]))

table(as.character(min_N_CSIL))

##CSL-SCL

n<-grep("CSL-?", min_N_CSIL)
min_N_CSL <- gsub("CSL-?", "SCL", min_N_CSIL)
table(as.character( min_N_CSL[n]))
table(as.character(min_N_CSIL[n]))

table(as.character( min_N_CSL))

##remove non-sensible class SCIL

n<-grep("SCIL-?", min_N_CSL)
min_N_SCIL <- gsub("SCIL-?", "", min_N_CSL)
table(as.character( min_N_CSL[n]))
table(as.character(min_N_SCIL[n]))


## spm

n<-grep("spm-?", min_N_SCIL)
min_N_spm <- gsub("spm-?", "1", min_N_SCIL)
table(as.character( min_N_spm[n]))
table(as.character(min_N_SCIL[n]))

## mpm

n<-grep("mpm-?", min_N_spm)
min_N_mpm <- gsub("mpm-?", "1", min_N_spm)
table(as.character( min_N_spm[n]))
table(as.character(min_N_mpm[n]))

## SL SL SL



##The clay fractions
##SICL

n<-grep("SICL-?", min_N_mpm)
min_SICL <- gsub("SICL-?", "", min_N_mpm)
table(as.character( min_SICL[n]))
table(as.character(min_N_mpm[n]))

##SIC

n<-grep("SIC-?", min_SICL)
min_SIC <- gsub("SIC-?", "", min_SICL)
table(as.character( min_SICL[n]))
table(as.character(min_SIC[n]))

##SCL

n<-grep("SCL-?", min_SIC)
min_SCL <- gsub("SCL-?", "", min_SIC)
table(as.character( min_SIC[n]))
table(as.character(min_SCL[n]))

##CL

n<-grep("CL-?", min_SCL)
min_CL <- gsub("CL-?", "", min_SCL)
table(as.character( min_CL[n]))
table(as.character(min_SCL[n]))

##SC

n<-grep("SC-?", min_CL)
min_SC <- gsub("SC-?", "", min_CL)
table(as.character( min_CL[n]))
table(as.character(min_SC[n]))
                                
##C

n<-grep("C-?", min_SC)
min_C <- gsub("C-?", "", min_SC)
table(as.character( min_C[n]))
table(as.character(min_SC[n]))


##SIL   

n<-grep("SIL-?", min_C)
min_SIL <- gsub("SIL-?", "", min_SC)
table(as.character( min_C[n]))
table(as.character(min_SIL[n]))

   
##SL

n<-grep("SL-?", min_SIL)
min_SL <- gsub("SL-?", "", min_SIL)
table(as.character( min_SL[n]))
table(as.character(min_SIL[n]))

##SI

n<-grep("SI-?", min_SL)
min_SI <- gsub("SI-?", "", min_SL)
table(as.character( min_SL[n]))
table(as.character(min_SI[n]))

##LS

n<-grep("LS-?", min_SI)
min_LS <- gsub("LS-?", "", min_SI)
table(as.character( min_LS[n]))
table(as.character(min_SI[n]))

##L

n<-grep("L-?", min_LS)
min_L <- gsub("L-?", "", min_LS)
table(as.character( min_LS[n]))
table(as.character(min_L[n]))
                               
##S

n<-grep("S-?", min_L)
min_S <- gsub("S-?", "", min_L)
table(as.character( min_S[n]))
table(as.character(min_L[n]))

## (SPACE)

n<-grep(" -?", min_L)
min_Space <- gsub(" -?", "", min_S)
table(as.character( min_S[n]))
table(as.character(min_Space[n]))

##47.5

n<-grep("47.547.5-?", min_Space)
min_47.5 <- gsub("47.547.5-?", "47.5", min_Space)
table(as.character( min_47.5[n]))
table(as.character(min_Space[n]))

n<-grep("47.547.5-?", min_47.5)
min_47.5II <- gsub("47.547.5-?", "47.5", min_47.5)
table(as.character( min_47.5[n]))
table(as.character(min_47.5II[n]))


 table(as.character(min_47.5II))

##CC

n<-grep("CC", min_47.5II)
min_CC <- gsub("CC-?", "", min_47.5II)
table(as.character( min_CC[n]))
table(as.character(min_47.5II[n]))

##C

n<-grep("C", min_47.5II)
min_C <- gsub("C-?", "", min_CC)
table(as.character( min_CC[n]))
table(as.character(min_C[n]))

##757575

n<-grep("757575", min_C)
min_757575 <- gsub("757575-?", "75", min_C)
table(as.character( min_757575[n]))
table(as.character(min_C[n]))

##7575C

n<-grep("7575", min_757575)
min_7575 <- gsub("7575-?", "75", min_757575)
table(as.character( min_757575[n]))
table(as.character(min_7575[n]))

##2525

n<-grep("2525", min_7575)
min_2525 <- gsub("2525-?", "25", min_7575)
table(as.character( min_2525[n]))
table(as.character(min_7575[n]))



##N/

n<-grep("N/", min_2525)
min_N <- gsub("N/-?", "", min_2525)
table(as.character( min_2525[n]))
table(as.character(min_N[n]))

## make 0  Don't bother.. it breaks

table(as.character(min_N))

n<-grep("SL?", min_N)
min_SLII <- gsub("SL?", "", min_N)
table(as.character( min_SLII[n]))
table(as.character(min_N[n]))



table(as.character(min_7575))
str(min_N_mpm)




write.table(min_7575, "Organic_161193.txt")





##Convert Textures into sand/silt/clay fractions

##Create a data frame which contains the 12 classes, then assign values for the 
##fractions

#text<-data.frame(tex=c("C","CL","L","LS","S","SC","SCL","SI","SIC","SICL","SIL","SL"),
#S=c(.15,

