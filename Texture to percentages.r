## This script is to convert data in the US database to the 12 main textures in the US texture triangle.Any duplicate textures 
## that are present after an initial texture class are deleted. This provides a rough base line of the sand/silt/clay fraction
## which can be used to replace any missing data.

setwd("C:/Users/phug7649/Desktop/txtbin")

##text390k_2 (another text file i was about to work on before)
s_text <- read.table("Subset of Textures_161193.txt", sep=",", header=T)

################################################################################

##the name of the item to be replaced preceeds the command so any mistakes can easily be corrected.
##The code is in 4 lines.Only 1 line is actually neccesary but the other three are check steps. Heres how it works:

#n<-grep("-?BR", min_ASH)                ## Creates a vector of all the instances of optional "-" followed by "BR"
#min_ASH_BR <- gsub("-?BR", "", min_ASH) ## Creates an object in which "BR" is substituted (in this case with nothing)
#table(as.character(min_ASH[n]))         ## Displays the table with reference to the items you wish to modify ("before")
#table(as.character(min_ASH_BR[n]))      ## Displays the "after table". This only works if something has gone wrong with 
                                         ## the substitution process.  


##ASHY

text<-s_text[,1]
n<-grep("ASHY?-", s_text[,1])
min_ASH <- gsub("ASHY?-", "", s_text[,1])
table(as.character(min_ASH[n]))
table(as.character(text[n]))

##BR

n<-grep("-?BR", min_ASH)
min_ASH_BR <- gsub("-?BR", "", min_ASH)
table(as.character(min_ASH[n]))
table(as.character(min_ASH_BR[n]))
##Check to see if the expression removes what you want (will not mention this in later iterations)

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
min_N_FB <- gsub("FB-?", "", min_N_DUR)
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
min_N_GS <- gsub("GS-?", "O", min_N_CB)
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
min_N_HB <- gsub("HB-?", "O", min_N_G)
table(as.character( min_N_HB[n]))
table(as.character(min_N_G[n]))

##HE

n<-grep("HE-?", min_N_HB)
min_N_HE <- gsub("HE-?", "O", min_N_HB)
table(as.character( min_N_HB[n]))
table(as.character(min_N_HE[n]))

##HO

n<-grep("HO-?", min_N_HE)
min_N_HO <- gsub("HO-?", "O", min_N_HE)
table(as.character( min_N_HO[n]))
table(as.character(min_N_HE[n]))

##HPM

n<-grep("HPM-?", min_N_HO)
min_N_HPM <- gsub("HPM-?", "O", min_N_HO)
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
min_N_MKY <- gsub("MKY-?", "", min_N_MEDL)
table(as.character( min_N_MEDL[n]))
table(as.character(min_N_MKY[n]))

##MK    (ORGANIC)

n<-grep("MK-?", min_N_MKY)
min_N_MK <- gsub("MK-?", "", min_N_MKY)
table(as.character( min_N_MK[n]))
table(as.character(min_N_MKY[n]))

##MPT (ORGANIC)

n<-grep("MPT-?", min_N_MKY)
min_N_MPT <- gsub("MPT-?", "", min_N_MKY)
table(as.character( min_N_MPT[n]))
table(as.character(min_N_MKY[n]))

##MR       (I can use this to link drainage to colour!!!)

n<-grep("MR-?", min_N_MPT)
min_N_MR <- gsub("MR-?", "", min_N_MPT)
table(as.character( min_N_MPT[n]))
table(as.character(min_N_MR[n]))

##MUCK

n<-grep("MUCK-?", min_N_MR)
min_N_MUCK <- gsub("MUCK-?", "", min_N_MR)
table(as.character( min_N_MUCK[n]))
table(as.character(min_N_MR[n]))

##OR         (Organic?)

n<-grep("OR-?", min_N_MUCK)
min_N_OR <- gsub("OR-?", "", min_N_MUCK)
table(as.character( min_N_MUCK[n]))
table(as.character(min_N_OR[n]))

##PDOM

n<-grep("PDOM-?", min_N_OR)
min_N_PDOM <- gsub("PDOM-?", "", min_N_OR)
table(as.character( min_N_PDOM[n]))
table(as.character(min_N_OR[n]))

##PEAT

n<-grep("PEAT-?", min_N_PDOM)
min_N_PEAT <- gsub("PEAT-?", "", min_N_PDOM)
table(as.character( min_N_PDOM[n]))
table(as.character(min_N_PEAT[n]))

##SPM

n<-grep("SPM-?", min_N_PEAT)
min_N_SPM <- gsub("SPM-?", "", min_N_PEAT)
table(as.character( min_N_SPM[n]))
table(as.character(min_N_PEAT[n]))

##OMPM

n<-grep("OMPM-?", min_N_SPM)
min_N_OMPM <- gsub("OMPM-?", "", min_N_SPM)
table(as.character( min_N_OMPM[n]))
table(as.character(min_N_SPM[n]))

##MPM

n<-grep("MPM-?", min_N_OMPM)
min_N_MPM <- gsub("MPM-?", "", min_N_OMPM)
table(as.character( min_N_OMPM[n]))
table(as.character(min_N_MPM[n]))

##PT

n<-grep("PT-?", min_N_MPM)
min_N_PT <- gsub("PT-?", "", min_N_MPM)
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
min_N_MK <- gsub("MK-?", "", min_N_ST)
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
min_N_O <- gsub("O-?", "", min_N_M)
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
min_N_spm <- gsub("spm-?", "", min_N_SCIL)
table(as.character( min_N_spm[n]))
table(as.character(min_N_SCIL[n]))

## mpm

n<-grep("mpm-?", min_N_spm)
min_N_mpm <- gsub("mpm-?", "", min_N_spm)
table(as.character( min_N_spm[n]))
table(as.character(min_N_mpm[n]))


## Creating the 12 texture classes
## Caution: the following commands remove everything after the letter. This means that eg CL, a legitimate texture, 
## will be replaced when using "C" on its own.Special codes from cheatography used but if anything has gone wrong, 
## THIS IS THE PLACE TO LOOK

##Spaces

n<-grep("^\\s+|\\s+$", min_N_mpm,perl=T)
min_N_space_final <- gsub("^\\s+|\\s+$","", min_N_mpm,perl=T)
table(as.character( min_N_space_final[n]))
table(as.character(min_N_mpm[n]))

## Create SL

n<-grep("SL.+", min_N_space_final,perl=T)
min_N_sl_final <- gsub("SL.+","SL", min_N_space_final,perl=T)
table(as.character( min_N_sl_final[n]))
table(as.character(min_N_space_final[n]))

## create LS

n<-grep("LS.+", min_N_sl_final,perl=T)
min_N_ls_final <- gsub("LS.+","LS", min_N_sl_final,perl=T)
table(as.character( min_N_ls_final[n]))
table(as.character(min_N_sl_final[n]))

#S

n<-grep("^S\\s+.+|^\\s+S\\s+.+", min_N_ls_final,perl=T)
min_N_s_final <- gsub("^S\\s+.+|^\\s+S\\s+.+","S", min_N_ls_final,perl=T)
table(as.character( min_N_ls_final[n]))
table(as.character(min_N_s_final[n]))

##spaces

n<-grep("^\\s+|\\s+$", min_N_s_final,perl=T)
min_N_space_final <- gsub("^\\s+|\\s+$","", min_N_s_final,perl=T)
table(as.character( min_N_space_final[n]))
table(as.character(min_N_s_final[n]))


#C

n<-grep("^C\\s+.+", min_N_space_final,perl=T)
min_N_c_final <- gsub("^C\\s+.+","C", min_N_space_final,perl=T)
table(as.character( min_N_c_final[n]))
table(as.character(min_N_space_final[n]))

#L 

n<-grep("^L\\s+.+", min_N_c_final,perl=T)
min_N_l_final <- gsub("^L\\s+.+","L", min_N_c_final,perl=T)
table(as.character( min_N_c_final[n]))
table(as.character(min_N_l_final[n]))

#SIC

n<-grep("SIC\\s.+", min_N_l_final,perl=T)
min_N_sic_final <- gsub("SIC\\s.+","SIC", min_N_l_final,perl=T)
table(as.character( min_N_l_final[n]))
table(as.character(min_N_sic_final[n]))

#SI

n<-grep("SI\\s.+", min_N_sic_final,perl=T)
min_N_si_final <- gsub("SI\\s.+","SI", min_N_sic_final,perl=T)
table(as.character( min_N_si_final[n]))
table(as.character(min_N_sic_final[n]))

#SC

n<-grep("SC\\s.+", min_N_si_final,perl=T)
min_N_sc_final <- gsub("SC\\s.+","SC", min_N_si_final,perl=T)
table(as.character( min_N_si_final[n]))
table(as.character(min_N_sc_final[n]))

#CL

n<-grep("CL\\s.+", min_N_sc_final,perl=T)
min_N_cl_final <- gsub("CL\\s.+","CL", min_N_sc_final,perl=T)
table(as.character( min_N_sc_final[n]))
table(as.character(min_N_cl_final[n]))

#SICL

n<-grep("SICL\\s.+", min_N_cl_final,perl=T)
min_N_sicl_final <- gsub("SICL\\s.+","SICL", min_N_cl_final,perl=T)
table(as.character( min_N_sicl_final[n]))
table(as.character(min_N_cl_final[n]))

#SCL

n<-grep("SCL\\s.+", min_N_sicl_final,perl=T)
min_N_scl_final <- gsub("SCL\\s.+","SCL", min_N_sicl_final,perl=T)
table(as.character( min_N_sicl_final[n]))
table(as.character(min_N_scl_final[n]))

#SIL

n<-grep("SIL\\s.+", min_N_scl_final,perl=T)
min_N_sil_final <- gsub("SIL\\s.+","SIL", min_N_scl_final,perl=T)
table(as.character( min_N_sil_final[n]))
table(as.character(min_N_scl_final[n]))

#SI

n<-grep("SI\\s.+", min_N_sil_final,perl=T)
min_N_si_final <- gsub("SI\\s.+","SI", min_N_sil_final,perl=T)
table(as.character( min_N_sil_final[n]))
table(as.character(min_N_si_final[n]))

## Check to see if the final list conforms to the 12 texture classes and 1 NA class

table(as.character(min_N_si_final))
str(min_N_mpm)

## If it works, write this to a text file.

write.table(min_N_l_final, "text_161913.txt")

##Yay! instead of manually replacing data for weeks on end, this script should do it in seconds! There are some things
## that could be done and the next few lines are a wish list. I will do this as my knowledge and confidence in R increases.


###############################################   WISH LIST   ############################################################

##Convert Textures into sand/silt/clay fractions

##Sand

#SICL 1

n<-grep("SICL", min_N_si_final,perl=T)
sand1 <- gsub("SICL","15", min_N_si_final,perl=T)
table(as.character( sand1[n]))
table(as.character(min_N_si_final[n]))

#SIL 2

n<-grep("SIL", sand1,perl=T)
sand2 <- gsub("SIL","20", sand1,perl=T)
table(as.character( sand1[n]))
table(as.character( sand2[n]))

#SIC 3

n<-grep("SIC", sand2,perl=T)
sand3 <- gsub("SIC","5", sand2,perl=T)
table(as.character( sand3[n]))
table(as.character( sand2[n]))

#SL 4

n<-grep("SL", sand3,perl=T)
sand4 <- gsub("SL","65", sand3,perl=T)
table(as.character( sand3[n]))
table(as.character( sand4[n]))


#SI 5

n<-grep("SI", sand4,perl=T)
sand5 <- gsub("SI","5", sand4,perl=T)
table(as.character( sand4[n]))
table(as.character( sand5[n]))

#SCL 6

n<-grep("SCL", sand5,perl=T)
sand6 <- gsub("SCL","62.5", sand5,perl=T)
table(as.character( sand5[n]))
table(as.character( sand6[n]))

#SC 7

n<-grep("SC", sand6,perl=T)
sand7 <- gsub("SC","52.5", sand6,perl=T)
table(as.character( sand6[n]))
table(as.character( sand7[n]))

#LS 8

n<-grep("LS", sand7,perl=T)
sand8 <- gsub("LS","80", sand7,perl=T)
table(as.character( sand7[n]))
table(as.character( sand8[n]))


#CL 9

n<-grep("CL", sand8,perl=T)
sand9 <- gsub("CL","30", sand8,perl=T)
table(as.character( sand8[n]))
table(as.character( sand9[n]))

#L 10

n<-grep("L", sand9,perl=T)
sand10 <- gsub("L","40", sand9,perl=T)
table(as.character( sand9[n]))
table(as.character( sand10[n]))

#C 11

n<-grep("C", sand10,perl=T)
sand11 <- gsub("C","15", sand10,perl=T)
table(as.character( sand10[n]))
table(as.character( sand11[n]))

#S 12

n<-grep("S", sand11,perl=T)
sand12 <- gsub("S","90", sand11,perl=T)
table(as.character( sand11[n]))
table(as.character( sand12[n]))

write.table(sand12, "sand_161913.txt")

##Silt
#SICL

n<-grep("SICL", min_N_si_final,perl=T)
silt1 <- gsub("SICL","55", min_N_si_final,perl=T)
table(as.character( silt1[n]))
table(as.character(min_N_si_final[n]))

#SIL 2

n<-grep("SIL", silt1,perl=T)
silt2 <- gsub("SIL","65", silt1,perl=T)
table(as.character( silt1[n]))
table(as.character( silt2[n]))

#SIC 3

n<-grep("SIC", silt2,perl=T)
silt3 <- gsub("SIC","50", silt2,perl=T)
table(as.character( silt3[n]))
table(as.character( silt2[n]))

#SL 4

n<-grep("SL", silt3,perl=T)
silt4 <- gsub("SL","25", silt3,perl=T)
table(as.character( silt3[n]))
table(as.character( silt4[n]))


#SI 5

n<-grep("SI", silt4,perl=T)
silt5 <- gsub("SI","90", silt4,perl=T)
table(as.character( silt4[n]))
table(as.character( silt5[n]))

#SCL 6

n<-grep("SCL", silt5,perl=T)
silt6 <- gsub("SCL","12.5", silt5,perl=T)
table(as.character( silt5[n]))
table(as.character( silt6[n]))

#SC 7

n<-grep("SC", silt6,perl=T)
silt7 <- gsub("SC","7.5", silt6,perl=T)
table(as.character( silt6[n]))
table(as.character( silt7[n]))

#LS 8

n<-grep("LS", silt7,perl=T)
silt8 <- gsub("LS","15", silt7,perl=T)
table(as.character( silt7[n]))
table(as.character( silt8[n]))


#CL 9

n<-grep("CL", silt8,perl=T)
silt9 <- gsub("CL","35", silt8,perl=T)
table(as.character( silt8[n]))
table(as.character( silt9[n]))

#L 10

n<-grep("L", silt9,perl=T)
silt10 <- gsub("L","40", silt9,perl=T)
table(as.character( silt9[n]))
table(as.character( silt10[n]))

#C 11

n<-grep("C", silt10,perl=T)
silt11 <- gsub("C","15", silt10,perl=T)
table(as.character( silt10[n]))
table(as.character( silt11[n]))

#S 12

n<-grep("S", silt11,perl=T)
silt12 <- gsub("S","5", silt11,perl=T)
table(as.character( silt11[n]))
table(as.character( silt12[n]))

write.table(silt12, "silt_161913.txt")

##Clay
#SICL

n<-grep("SICL", min_N_si_final,perl=T)
clay1 <- gsub("SICL","35", min_N_si_final,perl=T)
table(as.character( clay1[n]))
table(as.character(min_N_si_final[n]))

#SIL 2

n<-grep("SIL", clay1,perl=T)
clay2 <- gsub("SIL","15", clay1,perl=T)
table(as.character( clay1[n]))
table(as.character( clay2[n]))

#SIC 3

n<-grep("SIC", clay2,perl=T)
clay3 <- gsub("SIC","45", clay2,perl=T)
table(as.character( clay3[n]))
table(as.character( clay2[n]))

#SL 4

n<-grep("SL", clay3,perl=T)
clay4 <- gsub("SL","10", clay3,perl=T)
table(as.character( clay3[n]))
table(as.character( clay4[n]))


#SI 5

n<-grep("SI", clay4,perl=T)
clay5 <- gsub("SI","5", clay4,perl=T)
table(as.character( clay4[n]))
table(as.character( clay5[n]))

#SCL 6

n<-grep("SCL", clay5,perl=T)
clay6 <- gsub("SCL","35", clay5,perl=T)
table(as.character( clay5[n]))
table(as.character( clay6[n]))

#SC 7

n<-grep("SC", clay6,perl=T)
clay7 <- gsub("SC","40", clay6,perl=T)
table(as.character( clay6[n]))
table(as.character( clay7[n]))

#LS 8

n<-grep("LS", clay7,perl=T)
clay8 <- gsub("LS","5", clay7,perl=T)
table(as.character( clay7[n]))
table(as.character( clay8[n]))


#CL 9

n<-grep("CL", clay8,perl=T)
clay9 <- gsub("CL","35", clay8,perl=T)
table(as.character( clay8[n]))
table(as.character( clay9[n]))

#L 10

n<-grep("L", clay9,perl=T)
clay10 <- gsub("L","20", clay9,perl=T)
table(as.character( clay9[n]))
table(as.character( clay10[n]))

#C 11

n<-grep("C", clay10,perl=T)
clay11 <- gsub("C","70", clay10,perl=T)
table(as.character( clay10[n]))
table(as.character( clay11[n]))

#S 12

n<-grep("S", clay11,perl=T)
clay12 <- gsub("S","5", clay11,perl=T)
table(as.character( clay11[n]))
table(as.character( clay12[n]))

write.table(clay12, "clay_161913.txt")

####The converasion must be done in this order! (the other commands aren't functioning)
#SICL 1
#SIL 2
#SIC 3
#SL 4
#SI 5
#SCL 6
#SCL 7
#LS 8
#CL 9
#L 10
#C 11
#S 12

##Create a data frame which contains the 12 classes, then assign values for the 
##fractions

#text<-data.frame(tex=c("C","CL","L","LS","S","SC","SCL","SI","SIC","SICL","SIL","SL"),
#S=c(.15,

