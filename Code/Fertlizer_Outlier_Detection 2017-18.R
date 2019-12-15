# Copywrite - Asif Al Faisal - faisal.iit.du@gmail.com
setwd("D:\\CIMMYT\\Shuvo vai Data")
getwd()

library(readxl)

Data_Source <- "D:\\CIMMYT\\Shuvo vai Data\\Fertilizer Data.xlsx"


Master_Data <- read_excel(Data_Source)

Data.2017_18 <- read_excel(Data_Source, sheet = "2017-18")


head(Data.2017_18[,1:10])
str(Data.2017_18[,1:10])

## Subset Farm Yard Manure (FYM)



FYM_Cols <- c("D:q2014_fym", "D:q2015_fymTp", "D:q2016_fymAt", "D:q2017_fym_Utconv")

subset_FYM <- Data.2017_18[ , FYM_Cols]

colnames(subset_FYM) <- c("FYM.YesNo","FYM.Type","FYM.Amount","Maund.Standard.KG")


summary(subset_FYM)

library(ggplot2)

FYM_Plot <- ggplot(data = na.omit(subset_FYM), aes(x=FYM.Type, y=FYM.Amount, fill = FYM.Type))
FYM_Plot

FYM_Plot <- FYM_Plot + geom_boxplot() +
  #geom_jitter(alpha=0.5, size=0.4)+
  facet_wrap(~FYM.Type, scale="free") + 
  coord_cartesian(ylim = c(0,100))
FYM_Plot

FYM_Plot <- FYM_Plot + xlab("FYM Type") + ylab("FYM Amount in KG") + 
  ggtitle("FYM Fertilizer Box Plot (2017-18)")
FYM_Plot


### BASAL Fertilizer



Basal_Fert_Cols <- c("E:q2101_Fertuse",	"E:q2102_FertApp",	"G:q2201_bslUrea",	"G:q2202_bslDAP",	
                     "G:q2203_bslMoP",	"G:q2204_bslSSP",	"G:q2205_bslTSP",	
                     "G:q2206_bslZn",	"G:q2207_bslGPSM",	"G:q2208_bslSGNPK",	
                      "G:q2209_bslB",	"G:q2210_bslTh",	"G:q2211_bsl_othrFert",	
                     "G:q2212_bsl_othrFertAt")


Subset_BASAL <- Data.2017_18[ ,Basal_Fert_Cols]

colnames(Subset_BASAL) <- c("Ferti.YesNo", "Ferti.Type","Urea.Amount","DAP.Amount","MOP.Amount",
                            "SSP.Amount","TSP.Amount","Zinc.Amount","Gypsum.Amount","SGNPK.Amount",
                            "Boron.Amount","Thiovit.Amount","Other.Type","Other.Amount")



Subset_BASAL<- Subset_BASAL[Subset_BASAL$Ferti.YesNo == "Yes",]

TempCols <- c("Ferti.YesNo", "Ferti.Type", "Other.Type")

Subset_BASAL[TempCols] <- lapply(Subset_BASAL[TempCols], factor)

library(reshape2)
Molten_BASAL <- melt(Subset_BASAL)
str(Molten_BASAL)


## Alternative to melt

# amount.colums <- colnames(Subset_BASAL)[
#   endsWith(colnames(Subset_BASAL), ".Amount")
#   ]
# 
# 
# rm(df.boxplot); 
# for(i in seq_along(amount.colums)) {
#   i.vals <- Subset_BASAL[[amount.colums[i]]]
#   i.vals <- i.vals[!is.na(i.vals)]
#   
#   if(length(i.vals) == 0) next
#   i.df <- data.frame(
#     type = amount.colums[i],
#     amount = i.vals
#   )
#   
#   if(
#     !exists("df.boxplot")
#   ) { df.boxplot <- i.df } else { df.boxplot <- rbind(df.boxplot, i.df) }
# }; rm(i, i.df, i.vals)



BASAL_Plot <- ggplot(data = Molten_BASAL, aes(x=variable, y=value, fill = variable))
BASAL_Plot

BASAL_Plot <- BASAL_Plot + 
  geom_boxplot() + 
  #geom_jitter(alpha=0.5, size=0.4)+
  facet_wrap(~variable, scale="free") + 
  coord_cartesian(ylim = c(0,40))
BASAL_Plot


BASAL_Plot <- BASAL_Plot + xlab("BASAL Fertilizer Type") + 
  ylab("BASAL Fertilizer Amount (in KG)") + ggtitle("BASAL Fertilizer Box Plot (2017-18)")
BASAL_Plot






# Top Dressing 1

TD1_Cols <- c("I:q2211_td1Urea", "I:q2213_td1DAP","I:q2215_td1MoP","I:q2217_td1SSP",
              "I:q2219_td1TSP","I:q2221_td1Zn","I:q2223_td1GPSM","I:q22241_td1SGNPK")

Subset_TD1 <- Data.2017_18[,TD1_Cols]

colnames(Subset_TD1) <- c("Urea.Amount","DAP.Amount","MoP.Amount","SSP.Amount",
                          "TSP.Amount","Zn.Amount","GPSM.Amount","SGNPK.Amount")


Molten_TD1 <- melt(Subset_TD1)

TD1_Plot <- ggplot(data = Molten_TD1, aes(x=variable, y=value, fill = variable))
TD1_Plot

TD1_Plot <- TD1_Plot + geom_boxplot() + 
  #geom_jitter(alpha=0.5, size=0.4)+
  facet_wrap(~variable, scale="free") + 
  coord_cartesian(ylim = c(0,50))
TD1_Plot


TD1_Plot <- TD1_Plot + xlab("Fertilizer Type") + 
  ylab("Fertilizer Amount (in KG)") + ggtitle("First Top Dressing Fertilizer Box Plot(2017-18)")
TD1_Plot

# Top Dressing 2

TD2_Cols <- c("q2228_td2Fert",	"K:q2229_td2Urea", "K:q2231_td2DAP", "K:q2233_td2MoP",	
              "K:q2235_td2SSP", "K:q2237_td2TSP", "K:q2239_td2Zn", "K:q2241_td2GPSM", 
              "K:q22421_td2SGNPK")

Subset_TD2 <- Data.2017_18[,TD2_Cols]

colnames(Subset_TD2) <- c("TD2.YesNo","Urea.Amount","DAP.Amount","MoP.Amount","SSP.Amount",
                          "TSP.Amount","Zn.Amount","GPSM.Amount","SGNPK.Amount")

Subset_TD2 <- Subset_TD2[Subset_TD2$TD2.YesNo=="Yes",]

Subset_TD2$TD2.YesNo <- as.factor(Subset_TD2$TD2.YesNo)

Subset_TD2 <- Subset_TD2[!is.na(Subset_TD2$TD2.YesNo),]


Molten_TD2 <- melt(Subset_TD2)

TD2_Plot <- ggplot(data = Molten_TD2, aes(x=variable, y=value, fill = variable))
TD2_Plot

TD2_Plot <- TD2_Plot + geom_boxplot() + 
  #geom_jitter(alpha=0.5, size=0.4)+
  facet_wrap(~variable, scale="free") + 
  coord_cartesian(ylim = c(0,30))
TD2_Plot


TD2_Plot <- TD2_Plot + xlab("Fertilizer Type") + 
  ylab("Fertilizer Amount (in KG)") + ggtitle("Second Top Dressing Fertilizer Box Plot")
TD2_Plot





# Top Dressing 3

TD3_Cols <- c("q2247_td3Fert",	"M:q2248_td3Urea", "M:q2250_td3DAP", "M:q2252_td3MoP",	
              "M:q2254_td3SSP",	"M:q2256_td3TSP", "M:q2258_td3Zn",
              "M:q2260_td3GPSM", "M:q22611_td3SGNPK")

Subset_TD3 <- Data.2017_18[,TD3_Cols]

colnames(Subset_TD3) <- c("TD3.YesNo","Urea.Amount","DAP.Amount","MoP.Amount","SSP.Amount",
                          "TSP.Amount","Zn.Amount","GPSM.Amount","SGNPK.Amount")

Subset_TD3 <- Subset_TD3[Subset_TD3$TD3.YesNo=="Yes",]

Subset_TD3$TD3.YesNo <- as.factor(Subset_TD3$TD3.YesNo)

Subset_TD3 <- Subset_TD3[!is.na(Subset_TD3$TD3.YesNo),]

#rm(Basal_Fert_Cols,FYM_Cols,TD1_Cols,TD2_Cols,TempCols)

Molten_TD3 <- melt(Subset_TD3)

TD3_Plot <- ggplot(data = Molten_TD3, aes(x=variable, y=value, fill = variable))
TD3_Plot

TD3_Plot <- TD3_Plot + geom_boxplot() + 
  #geom_jitter(alpha=0.5, size=0.4)+
  facet_wrap(~variable, scale="free") + 
  coord_cartesian(ylim = c(0,25))
TD3_Plot




TD3_Plot <- TD3_Plot + xlab("Fertilizer Type") + 
  ylab("Fertilizer Amount (in KG)") + ggtitle("Third Top Dressing Fertilizer Box Plot (2017-18)")
TD3_Plot


FYM_Plot
BASAL_Plot
TD1_Plot
TD2_Plot
TD3_Plot





