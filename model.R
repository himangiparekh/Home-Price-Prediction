library(dplyr)
library(haven)
library(tidydr)
library(ggplot2)
library(skimr)
library(caret)
library(caretEnsemble)
library(caretForecast)
library(RANN)
library(earth)
library(xgboost)
library(kernlab)

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

DF_Final20 <- DF_Final[(2166:3595), ]

length(is.na(DF_Final$buildingpermits))
length(is.na(DF_Final20$buildingpermits))

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

# save the two files: 

write.csv(summary, "Summary Statistics.csv")
write.csv(cor_data20, "Correlation Table.csv")

# split Date into "Date", "Year" and "Month"

DF_Final20$Year <- as.numeric(substr(DF_Final20$DATE, 1, 4))
DF_Final20$Month <- as.numeric(substr(DF_Final20$DATE, 6, 7))

DF_Final20$Date <- as.numeric(paste(DF_Final20$Year, 
                                    DF_Final20$Month, sep = "."))
DF_Final20 <- DF_Final20[, -c(1, 2)]

# Mkae the Dataset such that No NA Values

DF_Final20$MortageR15 <- fillTheBlanks(DF_Final20$MortageR15)
DF_Final20$MortageR30 <- fillTheBlanks(DF_Final20$MortageR30)

# PLOT VALUES - EDA

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

# Save these plots 

HPI_Curve
Time_Plots
Box_Plots
QQ_Curve
Density_Curve

# split data into train and test

n_obs <- nrow(DF_Final20)
split <- round(n_obs * 0.7)
train20 <- DF_Final20[1:split, ]

test20 <- DF_Final20[(split + 1):nrow(DF_Final20), ]

dim(train20)
dim(test20)

# descriptive variables of data

skimmed <- skim_to_wide(DF_Final20)

# impute missing values using preProcess()

preProcess_missingdata_model <- preProcess(train20, method='knnImpute')
preProcess_missingdata_model 
# knn uses euclidean distance to find nearest neighbour

train20 <- predict(preProcess_missingdata_model, newdata = train20)
anyNA(train20)

# Transform the Data

preProcess_expo_model <- preProcess(train20, method='expoTrans')
# exponential transformation is like box-cox but works on negative values
# it transforms the data from a non-normal distribution to a normal one
# it does this using an exponent "lambda" 

train20 <- predict(preProcess_expo_model, newdata = train20)

# variable selection with recursive feature elimination

set.seed(100)
options(warn=-1) # suppressing warnings

subsets <- c(1:5, 10, 15, 18)

# rfe tries with all number of features: 1 feature, 2 features, 3 features...

ctrl <- rfeControl(functions = rfFuncs, # random forests
                   method = "repeatedcv", # repeated cross-validation
                   repeats = 5,
                   verbose = FALSE)

# could have used pickSizeTolerance in order to minimise RMSE

# Random forests consist of 4 –12 hundred decision trees, 
# each of them built over a random extraction of the observations from the 
# dataset and a random extraction of the features. Not every tree sees all 
# the features or all the observations, and this guarantees that the trees are 
# de-correlated and therefore less prone to over-fitting. Each tree is also a 
# sequence of yes-no questions based on a single or combination of features. 
# At each node (this is at each question), the tree divides the dataset 
# into 2 buckets, each of them hosting observations that are more similar 
# among themselves and different from the ones in the other bucket. 
# Therefore, the importance of each feature is derived from how “pure” each of 
# the buckets is.

lmProfile <- rfe(x=train20[, -c(7)], y=train20$HPI,
                 sizes = subsets,
                 rfeControl = ctrl)

lmProfile

# Variable Importance with Boruta

# boruta is effective when a data set comprised of several variables is 
# given for model building

boruta_output <- Boruta(HPI ~ ., data=train20, doTrace=0)  
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

# Train Data

set.seed(100)

# "train" good for cross-validating the model (measuring the performance of 
# given predicted model), and for tuning hyperparameters

model_HPI = train(HPI ~ ., data=train20, method='earth')
fitted <- predict(model_HPI)
summary(model_HPI)

# earth is MARS - Multivariate Adaptive Regression Splines 
# it splits the dataset into several "knots" to fit a linear model to each
# as these knots increase, MARS can be used to create a smooth model 
# for non-linear data

Model_Train_HPI <- plot(model_HPI, main="Model Accuracies with MARS")
Model_Train_HPI

# this shows how various iterations of hyperparameter tuning performed in terms 
# of RSME

# Linear Regression

try <- lm(HPI~., train20)
summary(try)

# Prepare Test Data

test20 <- predict(preProcess_missingdata_model, test20)  
test20 <- predict(preProcess_expo_model, test20)

# Predict Test Data

predicted <- as.data.frame(predict(model_HPI, round(test20)))
summary(predicted)

# Compare Predicted and Actual Values

actual_HPI <- as.data.frame(test20$HPI)

Comparison_Matrix <- data.frame(predicted, actual_HPI)

# Training Random Forest

set.seed(100)

model_rf = train(HPI ~ ., data=train20, method='rf', 
                       tuneLength=5)

# random forests creates trees where each node asks a yes/no question
# each observation is passed through the trees and nodes and put into a category
# which it is most similar to

# random forest: avoids over-fitting: is a classification and regression algo
# rf: bagging method where random data is organised and divided into sets, 
# for each set, a decision tree is made and then the majority voting / average
# is taken to calculate the ideal mapping of predictors on target value

summary(model_rf)

# Training SVM

set.seed(100)

model_svm = train(HPI ~ ., data=train20, method='svmRadial', 
                  tuneLength=15)
summary(model_svm)
model_svm[["ptype"]]

# support vector machine is ideally used for classification where the algo finds
# a hyperplane that differentiaties classes

# for regression it uses epsilon that is |y - ax| < E, where E is the acceptable
# error margin
# however, it can also use another variable "c" s.t. |y - ax| < E + |c|
# the model finds the ideal c s.t. the highest number of terms fall within E. 
# c, E are hyperparameters of the model 

# furthermore, xgboost / ada boost could have been used as a boosting algo

# compare models

models_compare <- resamples(list(RF=model_rf, MARS=model_HPI, SVM=model_svm))
summary(models_compare)

scales <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(models_compare, scales=scales)

# returns Mean Absolute Error (MAE), Residuals-squared, Root Mean Squared Errors

# caretEnsemble

trainControl <- trainControl(method="repeatedcv", 
                             number=10, 
                             repeats=3,
                             savePredictions=TRUE)

algorithmList <- c('rf', 'earth', 'svmRadial')

set.seed(100)

models <- caretList(HPI ~ ., data=train20, trControl=trainControl, 
                    methodList=algorithmList) 
summary(models)

results <- resamples(models)
summary(results)

# Plot results

scales <- list(x=list(relation="free"), y=list(relation="free"))

bwplot(results, scales=scales)

# Combine models for final prediction

set.seed(101)

stackControl <- trainControl(method="repeatedcv", 
                             number=10, 
                             repeats=3,
                             savePredictions=TRUE)

# Ensemble the predictions of `models` to form a new combined prediction based on glm

stack.glm <- caretStack(models, method="glm", 
                        trControl=stackControl)

summary(stack.glm)

# Predict on Test Data

stack_predicteds <- predict(stack.glm, newdata=test20)
stack_predicteds

# predicted data and actual data

Comparison_Matrix_2 <- data.frame(stack_predicteds, actual_HPI)

RSME <- sqrt(mean((abs(stack_predicteds)-abs(actual_HPI$`test20$HPI`))**2)) 
SSE <- sum((abs(stack_predicteds)-abs(actual_HPI$`test20$HPI`))**2) 

plot(stack_predicteds ~ actual_HPI$`test20$HPI`)

x <- stack_predicteds-actual_HPI$`test20$HPI`

plot(x ~ actual_HPI$`test20$HPI`)

# Extract Tables and Plots 

write.csv(Comparison_Matrix_2, "Comparison_Matrix_Actual_Predicted.csv")













