# Copywrite - Asif Al Faisal - faisal.iit.du@gmail.com
setwd("D:\\CIMMYT\\Shuvo vai Data")
getwd()

library(readxl)

Data_Source <- "D:\\CIMMYT\\Shuvo vai Data\\Fertilizer Data.xlsx"


Master_Data <- read_excel(Data_Source)

Data.2018_19 <- read_excel(Data_Source, sheet = "2018-19")


head(Data.2018_19[,1:10])
str(Data.2018_19[,1:10])


outlierFunc <- function(arr) {
  outlier_extent <- as.vector(c(
    quantile(arr, 0.25,na.rm = T) - 1.5 * IQR(arr,na.rm = T),
    quantile(arr, 0.75,na.rm = T) + 1.5 * IQR(arr,na.rm = T)
  ))
  
  sapply(arr, function(x) {
    if(x < outlier_extent[1] | x > outlier_extent[2]) mean(arr) else x
  })
}





## Subset Farm Yard Manure (FYM)




FYM_Cols <- c("D:q2015_fym", "D:q2016_fymTp", "D:q2017_fymAt", "D:q2018_fym_Utconv")

subset_FYM <- Data.2018_19[ , FYM_Cols]

colnames(subset_FYM) <- c("FYM.YesNo", "FYM.Type","FYM.Amount.KG", "Maund.Standard.KG")

subset_FYM<- subset_FYM[subset_FYM$FYM.YesNo == "Yes",]

subset_FYM$FYM.YesNo <- as.factor(subset_FYM$FYM.YesNo)


wet <- subset_FYM[subset_FYM$FYM.Type=="Wet",]
dry <- subset_FYM[subset_FYM$FYM.Type=="Dry",]

wet$FYM.Amount.KG <- outlierFunc(wet$FYM.Amount.KG)

dry$FYM.Amount.KG <- outlierFunc(dry$FYM.Amount.KG)

subset_FYM <- rbind(wet,dry)



summary(subset_FYM)




library(ggplot2)

FYM_Plot <- ggplot(data = subset_FYM, aes(x=FYM.Type, y=FYM.Amount.KG, fill = FYM.Type))
FYM_Plot

FYM_Plot <- FYM_Plot + geom_boxplot() + 
  facet_wrap(~FYM.Type, scales = "free")+
  coord_cartesian(ylim = c(0,100))
FYM_Plot



FYM_Plot <- FYM_Plot + xlab("FYM Type") + ylab("FYM Amount in KG") + 
  ggtitle("FYM Fertilizer Box Plot (2018-19)")
FYM_Plot


### BASAL Fertilizer



Basal_Fert_Cols <- c("E:q2101_Fertuse",	"G:bslFert",	"G:q2201_bslUrea",	"G:q2202_bslDAP",	
                     "G:q2203_bslMoP",	"G:q2204_bslSSP",	"G:q2205_bslTSP",	
                     "G:q2206_bslZn",	"G:q2207_bslGPSM",	"G:q2208_bslSGNPK",	
                     "G:q2209_bslB",	"G:q2210_bslTh",	"G:q2211_bsl_othrFert",	
                     "G:q2212_bsl_othrFertAt")


Subset_BASAL <- Data.2018_19[ ,Basal_Fert_Cols]

colnames(Subset_BASAL) <- c("Ferti.YesNo", "Ferti.Type","Urea.Amount","DAP.Amount","MOP.Amount",
                            "SSP.Amount","TSP.Amount","Zinc.Amount","Gypsum.Amount","SGNPK.Amount",
                            "Boron.Amount","Thiovit.Amount","Other.Type","Other.Amount")



Subset_BASAL<- Subset_BASAL[Subset_BASAL$Ferti.YesNo == "Yes",]

TempCols <- c("Ferti.YesNo", "Ferti.Type", "Other.Type")

Subset_BASAL[TempCols] <- lapply(Subset_BASAL[TempCols], factor)

#Subset_BASAL$Urea.Amount[!is.na(Subset_BASAL$Urea.Amount)] <- outlierFunc(Subset_BASAL$Urea.Amount[!is.na(Subset_BASAL$Urea.Amount)])


library(reshape2)
Molten_BASAL <- melt(Subset_BASAL)
summary(Molten_BASAL)


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
  geom_boxplot(outlier.shape = NA) + 
  #geom_jitter(alpha=0.5, size=0.4)+
  facet_wrap(~variable, scale="free") + 
  coord_cartesian(ylim = c(0,40))
BASAL_Plot



BASAL_Plot <- BASAL_Plot + xlab("BASAL Fertilizer Type") + 
  ylab("BASAL Fertilizer Amount (in KG)") + ggtitle("BASAL Fertilizer Box Plot (2018-19)")
BASAL_Plot


# Top Dressing 1

TD1_Cols <- c("I:q2213_td1Urea", "I:q2215_td1DAP","I:q2217_td1MoP","I:q2219_td1SSP",
              "I:q2221_td1TSP","I:q2223_td1Zn","I:q2225_td1GPSM","I:q2227_td1SGNPK",
              "I:q2229_td1B","I:q2231_td1Th","I:q2233_td1othrFert")

Subset_TD1 <- Data.2018_19[,TD1_Cols]

colnames(Subset_TD1) <- c("Urea.Amount","DAP.Amount","MoP.Amount","SSP.Amount",
                          "TSP.Amount","Zn.Amount","GPSM.Amount","SGNPK.Amount",
                          "Boron.Amount","Thiovit.Amount","Other.Amount")


Subset_TD1$Urea.Amount[is.na(Subset_TD1$Urea.Amount)] <- mean(Subset_TD1$Urea.Amount, na.rm = T)


Molten_TD1 <- melt(Subset_TD1)

TD1_Plot <- ggplot(data = Molten_TD1, aes(x=variable, y=value, fill = variable))
TD1_Plot

TD1_Plot <- TD1_Plot + geom_boxplot(outlier.color = NA) + coord_cartesian(ylim = c(0,45))
TD1_Plot


TD1_Plot <- TD1_Plot + xlab("Fertilizer Type") + 
  ylab("Fertilizer Amount (in KG)") + ggtitle("First Top Dressing Fertilizer Box Plot (2018-19)")
TD1_Plot

# Top Dressing 2

TD2_Cols <- c("K:q2235_td2Urea", "K:q2237_td2DAP", "K:q2239_td2MoP",	
              "K:q2241_td2SSP", "K:q2243_td2TSP", "K:q2245_td2Zn", "K:q2247_td2GPSM", 
              "K:q2249_td2SGNPK","K:q2251_td2B","K:q2253_td2Th","K:q2255_td2othrFert")

Subset_TD2 <- Data.2018_19[,TD2_Cols]

colnames(Subset_TD2) <- c("Urea.Amount","DAP.Amount","MoP.Amount","SSP.Amount",
                          "TSP.Amount","Zn.Amount","GPSM.Amount","SGNPK.Amount",
                          "Boron.Amount","Thiovit.Amount","Other.Amount")

Molten_TD2 <- melt(Subset_TD2)

TD2_Plot <- ggplot(data = Molten_TD2, aes(x=variable, y=value, fill = variable))
TD2_Plot

TD2_Plot <- TD2_Plot + geom_boxplot() + coord_cartesian(ylim = c(0,30))
TD2_Plot


TD2_Plot <- TD2_Plot + xlab("Fertilizer Type") + 
  ylab("Fertilizer Amount (in KG)") + ggtitle("Second Top Dressing Fertilizer Box Plot (2018-19)")
TD2_Plot





# Top Dressing 3

TD3_Cols <- c("M:q2257_td3Urea", "M:q2259_td3DAP", "M:q2261_td3MoP",	
              "M:q2263_td3SSP",	"M:q2265_td3TSP", "M:q2267_td3Zn",
              "M:q2269_td3GPSM", "M:q2271_td3SGNPK","M:q2273_td3B",
              "M:q2275_td3Th","M:q2277_td3othrFert")

Subset_TD3 <- Data.2018_19[,TD3_Cols]

colnames(Subset_TD3) <- c("Urea.Amount","DAP.Amount","MoP.Amount","SSP.Amount",
                          "TSP.Amount","Zn.Amount","GPSM.Amount","SGNPK.Amount",
                          "Boron.Amount","Thiovit.Amount","Other.Amount")


Molten_TD3 <- melt(Subset_TD3)

TD3_Plot <- ggplot(data = Molten_TD3, aes(x=variable, y=value, fill = variable))
TD3_Plot

TD3_Plot <- TD3_Plot + geom_boxplot() + coord_cartesian(ylim = c(0,25))
TD3_Plot




TD3_Plot <- TD3_Plot + xlab("Fertilizer Type") + 
  ylab("Fertilizer Amount (in KG)") + ggtitle("Third Top Dressing Fertilizer Box Plot(2018-19)")
TD3_Plot









