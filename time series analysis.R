library(dplyr)
library(haven)
library(tidydr)
library(readxl)
library(gamlr)
library(glmnet)
library(mice)
library(caTools)
library(aqp)
library(soilDB)
library(ggplot2)
library(ggpubr)
library(tidyr)
library(Boruta)
library(randomForest)

#load all the data 

DF_UnemploymentR <- read.csv("UNRATE.csv")
DF_MortageR30 <- read.csv("MORTGAGE30US.csv")
DF_MortageR15 <- read.csv("MORTGAGE15US.csv")
DF_HPI <- read.csv("CSUSHPINSA.csv")
DF_CPI <- read.csv("CPALTT01USM657N.csv")
DF_PopGrowR <- read.csv("SPPOPGROWUSA.csv")               #annual
DF_Pop <- read.csv("POPTHM.csv")
DF_GDP <- read.csv("USALORSGPNOSTSAM.csv")
DF_RealDisposableInc <- read.csv("DSPIC96.csv")
DF_DelinqecyR <- read.csv("DRSFRMACBS.csv")
DF_buildingpermits <- read.csv("M0255AUSM398NNBR.csv")
DF_constructionspening <- read.csv("TTLCONS.csv")
DF_realInc <- read.csv("RPI.csv")
DF_cost <- read.csv("WPUSI012011.csv")
DF_familys <- read.csv("TTLFHH.csv")
DF_workingage <- read.csv("LFWA25TTUSM647N.csv")
DF_publicinfra <- read.csv("TLPBLCONS.csv")
DF_maintenance <- read.csv("DHHMRC1A027NBEA.csv")
DF_sales <- read.csv("NHSDPTS.csv")

#change variable names

names(DF_cost)[names(DF_cost) == 'WPUSI012011'] <- 
  'ConstructionCost' 

names(DF_familys)[names(DF_familys) == 'TTLFHH'] <- 
  'familyhouseholds' 

names(DF_workingage)[names(DF_workingage) == 'LFWA25TTUSM647N'] <- 
  'WorkingAgePop' 

names(DF_publicinfra)[names(DF_publicinfra) == 'TLPBLCONS'] <- 
  'PublicInfra' 

names(DF_maintenance)[names(DF_maintenance) == 'DHHMRC1A027NBEA'] <- 
  'MaintenanceCost' 

names(DF_sales)[names(DF_sales) == 'NHSDPTS'] <- 
  'HomeSales' 

names(DF_UnemploymentR)[names(DF_UnemploymentR) == 'UNRATE'] <- 
  'UnemploymentR' 

names(DF_MortageR30)[names(DF_MortageR30) == 'MORTGAGE30US'] <- 
  'MortageR30' 

names(DF_MortageR15)[names(DF_MortageR15) == 'MORTGAGE15US'] <- 
  'MortageR15'

names(DF_HPI)[names(DF_HPI) == 'CSUSHPINSA'] <- 
  'HPI'

names(DF_CPI)[names(DF_CPI) == 'CPALTT01USM657N'] <- 
  'CPI'

names(DF_PopGrowR)[names(DF_PopGrowR) == 'SPPOPGROWUSA'] <- 
  'PopGrowR'

names(DF_Pop)[names(DF_Pop) == 'POPTHM'] <- 
  'Pop'

names(DF_GDP)[names(DF_GDP) == 'USALORSGPNOSTSAM'] <- 
  'GDP'

names(DF_RealDisposableInc)[names(DF_RealDisposableInc) == 'DSPIC96'] <- 
  'RealDisposableInc'

names(DF_DelinqecyR)[names(DF_DelinqecyR) == 'DRSFRMACBS'] <- 
  'DelinqecyR'

names(DF_buildingpermits)[names(DF_buildingpermits) == 'M0255AUSM398NNBR'] <- 
  'buildingpermits'

names(DF_constructionspening)[names(DF_constructionspening) == 'TTLCONS'] <- 
  'constructionspening' 

names(DF_realInc)[names(DF_realInc) == 'RPI'] <- 
  'realInc'

#make a cohesive data frame 

DF_Final <- merge(DF_buildingpermits, DF_constructionspening, by = "DATE", 
                  all = TRUE)
DF_Final <- merge(DF_Final, DF_cost, by = "DATE", all = TRUE)
DF_Final <- merge(DF_Final, DF_CPI, by = "DATE", all = TRUE)
DF_Final <- merge(DF_Final, DF_DelinqecyR, by = "DATE", all = TRUE)
DF_Final <- merge(DF_Final, DF_familys, by = "DATE", all = TRUE)
DF_Final <- merge(DF_Final, DF_GDP, by = "DATE", all = TRUE)
DF_Final <- merge(DF_Final, DF_HPI, by = "DATE", all = TRUE)
DF_Final <- merge(DF_Final, DF_maintenance, by = "DATE", all = TRUE)
DF_Final <- merge(DF_Final, DF_MortageR15, by = "DATE", all = TRUE)
DF_Final <- merge(DF_Final, DF_MortageR30, by = "DATE", all = TRUE)
DF_Final <- merge(DF_Final, DF_Pop, by = "DATE", all = TRUE)
DF_Final <- merge(DF_Final, DF_PopGrowR, by = "DATE", all = TRUE)
DF_Final <- merge(DF_Final, DF_publicinfra, by = "DATE", all = TRUE)
DF_Final <- merge(DF_Final, DF_RealDisposableInc, by = "DATE", all = TRUE)
DF_Final <- merge(DF_Final, DF_realInc, by = "DATE", all = TRUE)
DF_Final <- merge(DF_Final, DF_sales, by = "DATE", all = TRUE)
DF_Final <- merge(DF_Final, DF_UnemploymentR, by = "DATE", all = TRUE)
DF_Final <- merge(DF_Final, DF_workingage, by = "DATE", all = TRUE)

#check for NAs

summary(DF_Final)
length(is.na(DF_Final)) #every row has at least one NA

str(DF_Final)
DF_Final$familyhouseholds <- as.numeric(DF_Final$familyhouseholds)
DF_Final$PublicInfra <- as.numeric(DF_Final$PublicInfra)
DF_Final$constructionspening <- as.numeric(DF_Final$constructionspening)

#split DFs based on last 20 years v. all years data

DF_Final20 <- DF_Final[(2166:3595), ]

length(is.na(DF_Final$buildingpermits))
length(is.na(DF_Final20$buildingpermits))

DF_Final20 <- DF_Final20[-c(2)]

# EDA: Central Tendancy 

summary(DF_Final20)

Mean <- c()
Median <- c()
Min <- c()
Max <-  c()
Var <-  c()
SD <-  c()
CV <-  c()
IQR <-  c()
Range <- c()
FirstQuartile <- c()
ThridQuartile <- c()
NAs <- c()


for (i in 1:20) {
  Mean <- append(Mean, mean(DF_Final20[, i], na.rm = TRUE))
}

for (i in 1:20) {
  Median <- append(Median, median(DF_Final20[, i], na.rm=TRUE))
}

for (i in 1:20) {
  Min <- append(Min, min(DF_Final20[, i], na.rm=TRUE))
}

Min <- as.numeric(Min)

for (i in 1:20) {
  Max <- append(Max, max(DF_Final20[, i], na.rm=TRUE))
}

Max <- as.numeric(Max)

for (i in 1:20) {
  Var <- append(Var, var(DF_Final20[, i], na.rm=TRUE))
}

for (i in 1:20) {
  SD <- append(SD, sd(DF_Final20[, i], na.rm=TRUE))
}

for (i in 1:20) {
  IQR <- append(IQR, IQR(DF_Final20[, i], na.rm=TRUE))
}

for (i in 1:20) {
  x = Max[i] 
  y = Min[i]
  z = x - y 
  Range <- append(Range, z)
}

for (i in 1:20) {
  x = SD[i] 
  y = Mean[i]
  z = (x / y) * 100
  CV <- append(CV, z)
}

for (i in 1:20) {
  FirstQuartile <- append(FirstQuartile, quantile(as.numeric(DF_Final20[, i]), 
                                                  probs = c(0.25), na.rm = TRUE))
}

FirstQuartile <- as.numeric(FirstQuartile)

for (i in 1:20) {
  ThridQuartile <- append(ThridQuartile, quantile(as.numeric(DF_Final20[, i]), 
                                                  probs = c(0.75), na.rm = TRUE))
}

ThridQuartile <- as.numeric(ThridQuartile)

for (i in 1:20) {
  NAs <- append(NAs, sum(is.na(DF_Final20[, i])))
}

rm(i)
rm(x)
rm(y)
rm(z)

# add all the summary variables in a data frame

summary <- data.frame(colnames(DF_Final20), Mean, Median, CV, IQR, Max,
                      Min, Range, SD, Var, FirstQuartile, ThridQuartile, NAs)

# correlation, covariance, variance matrix

str(DF_Final20)
cor_data20 <- as.data.frame(cor(DF_Final20[, -1], use = "pairwise.complete.obs"))
cov_data20 <- as.data.frame(cov(DF_Final20[, -1], use = "pairwise.complete.obs"))

# Density Plot 

DC_1 <- ggplot(DF_Final20, aes(x = constructionspening)) + 
  geom_density()

DC_2 <- ggplot(DF_Final20, aes(x = ConstructionCost)) + 
  geom_density()

DC_3 <- ggplot(DF_Final20, aes(x = CPI)) + 
  geom_density()

DC_4 <- ggplot(DF_Final20, aes(x = DelinqecyR)) + 
  geom_density()

DC_5 <- ggplot(DF_Final20, aes(x = familyhouseholds)) + 
  geom_density()

DC_6 <- ggplot(DF_Final20, aes(x = MaintenanceCost)) + 
  geom_density()

DC_7 <- ggplot(DF_Final20, aes(x = MortageR15)) + 
  geom_density()

DC_8 <- ggplot(DF_Final20, aes(x = MortageR30)) + 
  geom_density()

DC_9 <- ggplot(DF_Final20, aes(x = Pop)) + 
  geom_density()

DC_10 <- ggplot(DF_Final20, aes(x = PopGrowR)) + 
  geom_density()

DC_11 <- ggplot(DF_Final20, aes(x = PublicInfra)) + 
  geom_density()

DC_12 <- ggplot(DF_Final20, aes(x = RealDisposableInc)) + 
  geom_density()

DC_13 <- ggplot(DF_Final20, aes(x = realInc)) + 
  geom_density()

DC_14 <- ggplot(DF_Final20, aes(x = HomeSales)) + 
  geom_density()

DC_15 <- ggplot(DF_Final20, aes(x = UnemploymentR)) + 
  geom_density()

DC_16 <- ggplot(DF_Final20, aes(x = WorkingAgePop)) + 
  geom_density()

DC_17 <- ggplot(DF_Final20, aes(x = GDP)) + 
  geom_density()

DC_18 <- ggplot(DF_Final20, aes(x = HPI)) + 
  geom_density()

Density_Curve <- ggarrange(DC_1, DC_2, DC_3, DC_4, DC_5, DC_6, DC_7, DC_8, 
                           DC_9, DC_10, DC_11, DC_12, DC_13, DC_14, DC_15, DC_16, 
                           DC_17, DC_18, labels = c( "constructionspening", "ConstructionCost",
                                       "CPI", "DelinqecyR", "familyhouseholds",
                                       "MaintenanceCost", "MortageR15", 
                                       "MortageR30", "Pop", "PopGrowR", 
                                       "PublicInfra", "RealDisposableInc",
                                       "realInc", "HomeSales", "UnemploymentR",
                                       "WorkingAgePop", "GDP", "HPI"), 
                           ncol = 6, nrow = 3)

Density_Curve

# QQ Plot 

QQ_1 <- ggplot(DF_Final20, aes(sample = constructionspening)) + 
  geom_qq() +
  geom_qq_line()

QQ_2 <- ggplot(DF_Final20, aes(sample = ConstructionCost)) + 
  geom_qq() +
  geom_qq_line()

QQ_3 <- ggplot(DF_Final20, aes(sample = CPI)) + 
  geom_qq() +
  geom_qq_line()

QQ_4 <- ggplot(DF_Final20, aes(sample = DelinqecyR)) + 
  geom_qq() +
  geom_qq_line()

QQ_5 <- ggplot(DF_Final20, aes(sample = familyhouseholds)) + 
  geom_qq() +
  geom_qq_line()

QQ_6 <- ggplot(DF_Final20, aes(sample = MaintenanceCost)) + 
  geom_qq() +
  geom_qq_line()

QQ_7 <- ggplot(DF_Final20, aes(sample = MortageR15)) + 
  geom_qq() +
  geom_qq_line()

QQ_8 <- ggplot(DF_Final20, aes(sample = MortageR30)) + 
  geom_qq() +
  geom_qq_line()

QQ_9 <- ggplot(DF_Final20, aes(sample = Pop)) + 
  geom_qq() +
  geom_qq_line()

QQ_10 <- ggplot(DF_Final20, aes(sample = PopGrowR)) + 
  geom_qq() +
  geom_qq_line()

QQ_11 <- ggplot(DF_Final20, aes(sample = PublicInfra)) + 
  geom_qq() +
  geom_qq_line()

QQ_12 <- ggplot(DF_Final20, aes(sample = RealDisposableInc)) + 
  geom_qq() +
  geom_qq_line()

QQ_13 <- ggplot(DF_Final20, aes(sample = realInc)) + 
  geom_qq() +
  geom_qq_line()

QQ_14 <- ggplot(DF_Final20, aes(sample = HomeSales)) + 
  geom_qq() +
  geom_qq_line()

QQ_15 <- ggplot(DF_Final20, aes(sample = UnemploymentR)) + 
  geom_qq() +
  geom_qq_line()

QQ_16 <- ggplot(DF_Final20, aes(sample = WorkingAgePop)) + 
  geom_qq() +
  geom_qq_line()

QQ_17 <- ggplot(DF_Final20, aes(sample = GDP)) + 
  geom_qq() +
  geom_qq_line()

QQ_18 <- ggplot(DF_Final20, aes(sample = HPI)) + 
  geom_qq() +
  geom_qq_line()

QQ_Curve <- ggarrange(QQ_1, QQ_2, QQ_3, QQ_4, QQ_5, QQ_6, QQ_7, QQ_8, 
                           QQ_9, QQ_10, QQ_11, QQ_12, QQ_13, QQ_14, QQ_15, QQ_16,
                      QQ_17, QQ_18, 
                           labels = c( "constructionspening", "ConstructionCost",
                                       "CPI", "DelinqecyR", "familyhouseholds",
                                       "MaintenanceCost", "MortageR15", 
                                       "MortageR30", "Pop", "PopGrowR", 
                                       "PublicInfra", "RealDisposableInc",
                                       "realInc", "HomeSales", "UnemploymentR",
                                       "WorkingAgePop", "GDP", "HPI"), 
                      ncol = 6, nrow = 3)


QQ_Curve

# fill all NA values with previous value

fillTheBlanks <- function(x){
  rle <- rle(as.numeric(x))
  empty <- which(is.na(rle$value))
  rle$values[empty] <- rle$value[empty+1]
  inverse.rle(rle)
}

fillTheBlanks2 <- function(x){
  rle <- rle(as.numeric(x))
  empty <- which(!is.na(rle$value))
  rle$values[empty+5] <- rle$value[empty]
  inverse.rle(rle)
}

fillTheBlanks3 <- function(x){
  rle <- rle(as.numeric(x))
  empty <- which(!is.na(rle$value))
  rle$values[empty+6] <- rle$value[empty]
  inverse.rle(rle)
}

fillTheBlanks4 <- function(x){
  rle <- rle(as.numeric(x))
  empty <- which(is.na(rle$value))
  rle$values[empty] <- rle$value[empty+5]
  inverse.rle(rle)
}

DF_Final20$MortageR15New <- fillTheBlanks(DF_Final20$MortageR15)
DF_Final20$MortageR30New <- fillTheBlanks(DF_Final20$MortageR30)

DF_Final20 <- DF_Final20[, -c(22, 23)]

DF_Final20$DelinqecyRNew <- fillTheBlanks2(DF_Final20$DelinqecyR)
DF_Final20$DelinqecyRNew <- fillTheBlanks2(DF_Final20$DelinqecyRNew)

DF_Final20$familyhouseholdsNew <- fillTheBlanks2(DF_Final20$familyhouseholds)
DF_Final20$familyhouseholdsNew <- fillTheBlanks2(DF_Final20$familyhouseholdsNew)
DF_Final20$familyhouseholdsNew <- fillTheBlanks3(DF_Final20$familyhouseholdsNew)
DF_Final20$familyhouseholdsNew <- fillTheBlanks2(DF_Final20$familyhouseholdsNew)
DF_Final20$familyhouseholdsNew <- fillTheBlanks2(DF_Final20$familyhouseholdsNew)
DF_Final20$familyhouseholdsNew <- fillTheBlanks3(DF_Final20$familyhouseholdsNew)
DF_Final20$familyhouseholdsNew <- fillTheBlanks2(DF_Final20$familyhouseholdsNew)
DF_Final20$familyhouseholdsNew <- fillTheBlanks2(DF_Final20$familyhouseholdsNew)
DF_Final20$familyhouseholdsNew <- fillTheBlanks2(DF_Final20$familyhouseholdsNew)
DF_Final20$familyhouseholdsNew <- fillTheBlanks2(DF_Final20$familyhouseholdsNew)
DF_Final20$familyhouseholdsNew <- fillTheBlanks4(DF_Final20$familyhouseholdsNew)

DF_Final20$MaintenanceCostNew <- fillTheBlanks2(DF_Final20$MaintenanceCost)
DF_Final20$MaintenanceCostNew <- fillTheBlanks2(DF_Final20$MaintenanceCostNew)
DF_Final20$MaintenanceCostNew <- fillTheBlanks3(DF_Final20$MaintenanceCostNew)
DF_Final20$MaintenanceCostNew <- fillTheBlanks2(DF_Final20$MaintenanceCostNew)
DF_Final20$MaintenanceCostNew <- fillTheBlanks2(DF_Final20$MaintenanceCostNew)
DF_Final20$MaintenanceCostNew <- fillTheBlanks3(DF_Final20$MaintenanceCostNew)
DF_Final20$MaintenanceCostNew <- fillTheBlanks2(DF_Final20$MaintenanceCostNew)
DF_Final20$MaintenanceCostNew <- fillTheBlanks2(DF_Final20$MaintenanceCostNew)
DF_Final20$MaintenanceCostNew <- fillTheBlanks2(DF_Final20$MaintenanceCostNew)
DF_Final20$MaintenanceCostNew <- fillTheBlanks2(DF_Final20$MaintenanceCostNew)
DF_Final20$MaintenanceCostNew <- fillTheBlanks4(DF_Final20$MaintenanceCostNew)

DF_Final20$PopGrowRNew <- fillTheBlanks2(DF_Final20$PopGrowR)
DF_Final20$PopGrowRNew <- fillTheBlanks2(DF_Final20$PopGrowRNew)
DF_Final20$PopGrowRNew <- fillTheBlanks3(DF_Final20$PopGrowRNew)
DF_Final20$PopGrowRNew <- fillTheBlanks2(DF_Final20$PopGrowRNew)
DF_Final20$PopGrowRNew <- fillTheBlanks2(DF_Final20$PopGrowRNew)
DF_Final20$PopGrowRNew <- fillTheBlanks3(DF_Final20$PopGrowRNew)
DF_Final20$PopGrowRNew <- fillTheBlanks2(DF_Final20$PopGrowRNew)
DF_Final20$PopGrowRNew <- fillTheBlanks2(DF_Final20$PopGrowRNew)
DF_Final20$PopGrowRNew <- fillTheBlanks2(DF_Final20$PopGrowRNew)
DF_Final20$PopGrowRNew <- fillTheBlanks2(DF_Final20$PopGrowRNew)
DF_Final20$PopGrowRNew <- fillTheBlanks4(DF_Final20$PopGrowRNew)

# split Date into "Date", "Year" and "Month

str(DF_Final20)

DF_Final20$Year <- as.numeric(substr(DF_Final20$DATE, 1, 4))
DF_Final20$Month <- as.numeric(substr(DF_Final20$DATE, 6, 7))

DF_Final20$Date <- as.numeric(paste(DF_Final20$Year, 
                                    DF_Final20$Month, sep = "."))
str(DF_Final20)

# box plot 

Box_1 <- ggplot(data = DF_Final20[, -c(1, 2, 11, 12)], 
                mapping = aes(x = Year, y = constructionspening)) +
  geom_boxplot()

Box_2 <- ggplot(data = DF_Final20[, -c(1, 2, 11, 12)], 
                mapping = aes(x = Year, y = ConstructionCost)) +
  geom_boxplot()

Box_3 <- ggplot(data = DF_Final20[, -c(1, 2, 11, 12)], 
                mapping = aes(x = Year, y = CPI)) +
  geom_boxplot()

Box_4 <- ggplot(data = DF_Final20[, -c(1, 2, 11, 12)], 
                mapping = aes(x = Year, y = DelinqecyRNew)) +
  geom_boxplot()

Box_5 <- ggplot(data = DF_Final20[, -c(1, 2, 11, 12)], 
                mapping = aes(x = Year, y = familyhouseholdsNew)) +
  geom_boxplot()

Box_6 <- ggplot(data = DF_Final20[, -c(1, 2, 11, 12)], 
                mapping = aes(x = Year, y = GDP)) +
  geom_boxplot()

Box_7 <- ggplot(data = DF_Final20[, -c(1, 2, 11, 12)], 
                mapping = aes(x = Year, y = HPI)) +
  geom_boxplot()

Box_8 <- ggplot(data = DF_Final20[, -c(1, 2, 11, 12)], 
                mapping = aes(x = Year, y = MaintenanceCostNew)) +
  geom_boxplot()

Box_9 <- ggplot(data = DF_Final20[, -c(1, 2, 11, 12)], 
                mapping = aes(x = Year, y = constructionspening)) +
  geom_boxplot()

Box_10 <- ggplot(data = DF_Final20[, -c(1, 2, 11, 12)], 
                mapping = aes(x = Year, y = MortageR15New)) +
  geom_boxplot()

Box_11 <- ggplot(data = DF_Final20[, -c(1, 2, 11, 12)], 
                mapping = aes(x = Year, y = WorkingAgePop)) +
  geom_boxplot()

Box_12 <- ggplot(data = DF_Final20[, -c(1, 2, 11, 12)], 
                mapping = aes(x = Year, y = Pop)) +
  geom_boxplot()

Box_13 <- ggplot(data = DF_Final20[, -c(1, 2, 11, 12)], 
                mapping = aes(x = Year, y = PopGrowRNew)) +
  geom_boxplot()

Box_14 <- ggplot(data = DF_Final20[, -c(1, 2, 11, 12)], 
                mapping = aes(x = Year, y = PublicInfra)) +
  geom_boxplot()

Box_15 <- ggplot(data = DF_Final20[, -c(1, 2, 11, 12)], 
                mapping = aes(x = Year, y = RealDisposableInc)) +
  geom_boxplot()

Box_16 <- ggplot(data = DF_Final20[, -c(1, 2, 11, 12)], 
                mapping = aes(x = Year, y = realInc)) +
  geom_boxplot()

Box_17 <- ggplot(data = DF_Final20[, -c(1, 2, 11, 12)], 
                mapping = aes(x = Year, y = HomeSales)) +
  geom_boxplot()

Box_18 <- ggplot(data = DF_Final20[, -c(1, 2, 11, 12)], 
                mapping = aes(x = Year, y = UnemploymentR)) +
  geom_boxplot()

Box_Plots <- ggarrange(Box_1,Box_2, Box_3, Box_4, Box_5, Box_6, Box_7, Box_8, 
                       Box_9, Box_10, Box_11, Box_12, Box_13, Box_14, Box_15, Box_16,
                       Box_17, Box_18, 
                      labels = c( "constructionspening", "ConstructionCost",
                                  "CPI", "DelinqecyR", "familyhouseholds",
                                  "GDP", "HPI", "MaintenanceCost", "constructionspening", 
                                  "MortageR15", 
                                  "WorkingAgePop", "Pop", "PopGrowR", 
                                  "PublicInfra", "RealDisposableInc",
                                  "realInc", "HomeSales", "UnemploymentR"), 
                      ncol = 6, nrow = 3)

Box_Plots

# time series plots - Recession Shading

Time_1 <- ggplot(data = DF_Final20[, -c(2, 11, 12)], 
                 aes(x = Date, y = constructionspening)) +
  geom_line(alpha = 0.3) +
  geom_smooth() +
  theme_bw() +
  geom_rect(xmin=2007.01, xmax=2009.01, 
            ymin=0, ymax=Inf, fill='grey', alpha = 0.01)

Time_2 <- ggplot(data = DF_Final20[, -c(1, 2, 11, 12)], 
                aes(x = Date, y = ConstructionCost)) +
  geom_line(alpha = 0.3) +
  geom_smooth() +
  theme_bw() +
  geom_rect(xmin=2007.01, xmax=2009.01, 
            ymin=0, ymax=Inf, fill='grey', alpha = 0.01)

Time_3 <- ggplot(data = DF_Final20[, -c(1, 2, 11, 12)], 
                aes(x = Date, y = CPI)) +
  geom_line(alpha = 0.3) +
  geom_smooth() +
  theme_bw() +
  geom_rect(xmin=2007.01, xmax=2009.01, 
            ymin=0, ymax=Inf, fill='grey', alpha = 0.01)

Time_4 <- ggplot(data = DF_Final20[, -c(1, 2, 11, 12)], 
                aes(x = Date, y = DelinqecyRNew)) +
  geom_line(alpha = 0.3) +
  geom_smooth() +
  theme_bw() +
  geom_rect(xmin=2007.01, xmax=2009.01, 
            ymin=0, ymax=Inf, fill='grey', alpha = 0.01)

Time_5 <- ggplot(data = DF_Final20[, -c(1, 2, 11, 12)], 
                aes(x = Date, y = familyhouseholdsNew)) +
  geom_line(alpha = 0.3) +
  geom_smooth() +
  theme_bw() +
  geom_rect(xmin=2007.01, xmax=2009.01, 
            ymin=0, ymax=Inf, fill='grey', alpha = 0.01)

Time_6 <- ggplot(data = DF_Final20[, -c(1, 2, 11, 12)], 
                aes(x = Date, y = GDP)) +
  geom_line(alpha = 0.3) +
  geom_smooth() +
  theme_bw() +
  geom_rect(xmin=2007.01, xmax=2009.01, 
            ymin=0, ymax=Inf, fill='grey', alpha = 0.01)

Time_7 <- ggplot(data = DF_Final20[, -c(1, 2, 11, 12)], 
                aes(x = Date, y = HPI)) +
  geom_line(alpha = 0.3) +
  geom_smooth() +
  theme_bw() +
  geom_rect(xmin=2007.01, xmax=2009.01, 
            ymin=0, ymax=Inf, fill='grey', alpha = 0.01)

Time_8 <- ggplot(data = DF_Final20[, -c(1, 2, 11, 12)], 
                aes(x = Date, y = MaintenanceCostNew)) +
  geom_line(alpha = 0.3) +
  geom_smooth() +
  theme_bw() +
  geom_rect(xmin=2007.01, xmax=2009.01, 
            ymin=0, ymax=Inf, fill='grey', alpha = 0.01)

Time_9 <- ggplot(data = DF_Final20[, -c(1, 2, 11, 12)], 
                aes(x = Date, y = constructionspening)) +
  geom_line(alpha = 0.3) +
  geom_smooth() +
  theme_bw() +
  geom_rect(xmin=2007.01, xmax=2009.01, 
            ymin=0, ymax=Inf, fill='grey', alpha = 0.01)

Time_10 <- ggplot(data = DF_Final20[, -c(1, 2, 11, 12)], 
                 aes(x = Date, y = MortageR15New)) +
  geom_line(alpha = 0.3) +
  geom_smooth() +
  theme_bw() +
  geom_rect(xmin=2007.01, xmax=2009.01, 
            ymin=0, ymax=Inf, fill='grey', alpha = 0.01)

Time_11 <- ggplot(data = DF_Final20[, -c(1, 2, 11, 12)], 
                 aes(x = Date, y = WorkingAgePop)) +
  geom_line(alpha = 0.3) +
  geom_smooth() +
  theme_bw() +
  geom_rect(xmin=2007.01, xmax=2009.01, 
            ymin=0, ymax=Inf, fill='grey', alpha = 0.01)

Time_12 <- ggplot(data = DF_Final20[, -c(1, 2, 11, 12)], 
                 aes(x = Date, y = Pop)) +
  geom_line(alpha = 0.3) +
  geom_smooth() +
  theme_bw() +
  geom_rect(xmin=2007.01, xmax=2009.01, 
            ymin=0, ymax=Inf, fill='grey', alpha = 0.01)

Time_13 <- ggplot(data = DF_Final20[, -c(1, 2, 11, 12)], 
                 aes(x = Date, y = PopGrowRNew)) +
  geom_line(alpha = 0.3) +
  geom_smooth() +
  theme_bw() +
  geom_rect(xmin=2007.01, xmax=2009.01, 
            ymin=0, ymax=Inf, fill='grey', alpha = 0.01)

Time_14 <- ggplot(data = DF_Final20[, -c(1, 2, 11, 12)], 
                 aes(x = Date, y = PublicInfra)) +
  geom_line(alpha = 0.3) +
  geom_smooth() +
  theme_bw() +
  geom_rect(xmin=2007.01, xmax=2009.01, 
            ymin=0, ymax=Inf, fill='grey', alpha = 0.01)

Time_15 <- ggplot(data = DF_Final20[, -c(1, 2, 11, 12)], 
                 aes(x = Date, y = RealDisposableInc)) +
  geom_line(alpha = 0.3) +
  geom_smooth() +
  theme_bw() +
  geom_rect(xmin=2007.01, xmax=2009.01, 
            ymin=0, ymax=Inf, fill='grey', alpha = 0.01)

Time_16 <- ggplot(data = DF_Final20[, -c(1, 2, 11, 12)], 
                 aes(x = Date, y = realInc)) +
  geom_line(alpha = 0.3) +
  geom_smooth() +
  theme_bw() +
  geom_rect(xmin=2007.01, xmax=2009.01, 
            ymin=0, ymax=Inf, fill='grey', alpha = 0.01)

Time_17 <- ggplot(data = DF_Final20[, -c(1, 2, 11, 12)], 
                 aes(x = Date, y = HomeSales)) +
  geom_line(alpha = 0.3) +
  geom_smooth() +
  theme_bw() +
  geom_rect(xmin=2007.01, xmax=2009.01, 
            ymin=0, ymax=Inf, fill='grey', alpha = 0.01)

Time_18 <- ggplot(data = DF_Final20[, -c(1, 2, 11, 12)], 
                 aes(x = Date, y = UnemploymentR)) +
  geom_line(alpha = 0.3) +
  geom_smooth() +
  theme_bw() +
  geom_rect(xmin=2007.01, xmax=2009.01, 
            ymin=0, ymax=Inf, fill='grey', alpha = 0.01)

Time_Plots <- ggarrange(Time_1, Time_2, Time_3, Time_4, Time_5, Time_6, Time_7, 
                        Time_8, 
                        Time_9, Time_10, Time_11, Time_12, Time_13, Time_14, 
                        Time_15, Time_16,
                        Time_17, Time_18, 
                       labels = c("constructionspening", "ConstructionCost",
                                   "CPI", "DelinqecyR", "familyhouseholds",
                                   "GDP", "HPI", "MaintenanceCost", 
                                  "constructionspening", 
                                   "MortageR15", 
                                   "WorkingAgePop", "Pop", "PopGrowR", 
                                   "PublicInfra", "RealDisposableInc",
                                   "realInc", "HomeSales", "UnemploymentR"), 
                       ncol = 6, nrow = 3)

Time_Plots

# all vars v. HPI

HPI_1 <- ggplot(DF_Final20, aes(y = HPI, x = constructionspening)) + 
  geom_point()

HPI_2 <- ggplot(DF_Final20, aes(y = HPI, x = ConstructionCost)) + 
  geom_point()

HPI_3 <- ggplot(DF_Final20, aes(y = HPI, x = CPI)) + 
  geom_point()

HPI_4 <- ggplot(DF_Final20, aes(y = HPI, x = DelinqecyR)) + 
  geom_point()

HPI_5 <- ggplot(DF_Final20, aes(y = HPI, x = familyhouseholds)) + 
  geom_point()

HPI_6 <- ggplot(DF_Final20, aes(y = HPI, x = MaintenanceCost)) + 
  geom_point()

HPI_7 <- ggplot(DF_Final20, aes(y = HPI, x = MortageR15New)) + 
  geom_point()

HPI_9 <- ggplot(DF_Final20, aes(y = HPI, x = Pop)) + 
  geom_point()

HPI_10 <- ggplot(DF_Final20, aes(y = HPI, x = PopGrowR)) + 
  geom_point()

HPI_11 <- ggplot(DF_Final20, aes(y = HPI, x = PublicInfra)) + 
  geom_point()

HPI_12 <- ggplot(DF_Final20, aes(y = HPI, x = RealDisposableInc)) + 
  geom_point()

HPI_13 <- ggplot(DF_Final20, aes(y = HPI, x = realInc)) + 
  geom_point()

HPI_14 <- ggplot(DF_Final20, aes(y = HPI, x = HomeSales)) + 
  geom_point()

HPI_15 <- ggplot(DF_Final20, aes(y = HPI, x = UnemploymentR)) + 
  geom_point()

HPI_16 <- ggplot(DF_Final20, aes(y = HPI, x = WorkingAgePop)) + 
  geom_point()

HPI_17 <- ggplot(DF_Final20, aes(y = HPI, x = GDP)) + 
  geom_point()

HPI_Curve <- ggarrange(HPI_1, HPI_2, HPI_3, HPI_4, HPI_5, HPI_6, HPI_7, 
                           HPI_9, HPI_10, HPI_11, HPI_12, HPI_13, HPI_14, HPI_15, 
                           HPI_16, HPI_17, labels = c( "constructionspening", 
                                        "ConstructionCost",
                                       "CPI", "DelinqecyR", "familyhouseholds",
                                       "MaintenanceCost", "MortageR15", 
                                       "Pop", "PopGrowR", 
                                       "PublicInfra", "RealDisposableInc",
                                       "realInc", "HomeSales", "UnemploymentR",
                                       "WorkingAgePop", "GDP"), 
                       ncol = 6, nrow = 3)

HPI_Curve
HPI_15
HPI_14
HPI_17
HPI_7

# Variable Selection via Boruta

DF_Final20_Boruta <- na.omit(DF_Final20[, -c(2, 6, 7, 10, 11, 12, 14, 27, 28, 29)])

boruta_output <- Boruta(HPI ~ ., data=DF_Final20_Boruta, doTrace=0)  
names(boruta_output)

boruta_signif <- getSelectedAttributes(boruta_output, withTentative = TRUE)
print(boruta_signif)

roughFixMod <- TentativeRoughFix(boruta_output)
boruta_signif <- getSelectedAttributes(roughFixMod)
print(boruta_signif)

imps <- attStats(roughFixMod)
imps2 = imps[imps$decision != 'Rejected', c('meanImp', 'decision')]
imps2[order(-imps2$meanImp), ]

plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance") 

# ML RF Algorithm

HPI.rf <- randomForest(HPI ~ ., data = DF_Final20_Boruta, mtry = 3,
                                   importance = TRUE)
print(HPI.rf)
plot(HPI.rf)

