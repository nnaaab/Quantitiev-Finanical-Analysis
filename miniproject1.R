library(ggplot2)
mkt1925df <- read.csv("special.csv")

mkt1925df$mktcap <-mkt1925df$PRC*mkt1925df$SHROUT*1000/1000000
mkt1925df$year <-as.numeric(substring(as.character(mkt1925df$date), 7, 10))

ggplot(data = mkt1925df, aes(x = year, y = mktcap,group = year)) +
  geom_boxplot() + stat_summary(fun.y=mean, colour="blue", geom="point", shape=7,size=2) +
  ylim(0, 800) + xlab("Year") +  ylab("Market Capitalization ($M)") +
  labs(title = "Boxplots of Market Capitalization from 1925 (By Year)") +
  theme(axis.title.x = element_text(size=15), axis.title.y =element_text(size=15)) +
  theme_bw()





countYear <- 2019-1925+1
over50 <- numeric(countYear)
overMean <- numeric(countYear)
overMedian <- numeric(countYear)
propOver50 <- numeric(countYear)
mktcapMeans <- numeric(countYear)
mktcapMedians <- numeric(countYear)
propMeans <- numeric(countYear)
propMedians <- numeric(countYear)

k <- 1
for(val in seq(1925, 2019, 1)){
  temp <- mkt1925df[which(mkt1925df$year == val),]
  mktcapMeans[k] <- mean(temp$mktcap, na.rm = T)
  propMeans[k] <-nrow(temp[temp$mktcap>mktcapMeans[k],])/nrow(temp)
  mktcapMedians[k] <- median(temp$mktcap, na.rm =TRUE)
  propMedians[k] <-nrow(temp[temp$mktcap>mktcapMedians[k],])/nrow(temp)
  over50[k] <- nrow(temp[temp$mktcap>50,])
  overMean[k] <-nrow(temp[temp$mktcap>mktcapMeans[k],])
  overMedian[k] <-nrow(temp[temp$mktcap>mktcapMedians[k],])
  propOver50[k] <-nrow(temp[temp$mktcap>50,])/nrow(temp)
  k <- k+1
}

par(mfrow=c(3,2))
plot(seq(1925, 2019, 1),
     propMeans, type = "l",
     col = "darkblue", xlab = "Year",
     ylab = "Proportion",
     main ="Porportion over Mean Market Capitalization (1925 - 2019)", ylim =c(.1,.3))

plot(seq(1925, 2019, 1),
     overMean/12, type = "l",
     col = "darkblue", xlab = "Year",
     ylab = "Proportion",
     main ="Count over Mean Market Capitalization (1925 - 2019)")

plot(seq(1925, 2019, 1), 
     rep(.5, 95), type = "l",
     col = "darkblue", xlab = "Year",
     ylab = "Proportion",
     main ="Porportion over Median Market Capitalization (1925 - 2019)", ylim = c(0,.75))

plot(seq(1925, 2019, 1), 
     overMedian/12, type = "l", 
     col = "darkblue", xlab = "Year",
     ylab = "Proportion",
     main ="Count over Median Market Capitalization (1925 - 2019))")

plot(seq(1925, 2019, 1),
     propOver50, type = "l",
     col = "darkblue", xlab = "Year",
     ylab = "Proportion",
     main ="Porportion over $50M Median Market Capitalization (1925 - 2019)", ylim = c(0,.75))

plot(seq(1925, 2019, 1),
     overMedian/12, type = "l",
     col = "darkblue", xlab = "Year", 
     ylab = "Proportion", main ="Count over $50M Market Capitalization (1925 - 2019)")
mtext("Proportion of Market Capitalizations above Yearly Mean and Median (1925 - 2019)", side = 1, line = -1, outer = TRUE)

origin_data <-read.csv("origin_data_mini1.csv")

nrow(origin_data)
empdf <- numeric(ncol(origin_data))
dfname <- character(ncol(origin_data))
for (i in seq(1, ncol(origin_data),1)){
  empdf[i]<-1-sum(is.na(origin_data[i]))/304959
  dfname[i] <- names(origin_data[i])
}

dfnew<-cbind(dfname,empdf)
write.csv(dfnew,"dfnew.csv")


#mktcap - Market Capitalization (csho*prcc_f)
origin_data$mktcap <- origin_data$csho*origin_data$prcc_f
#bookvalue - Book Value (csho*bkvlps)
origin_data$bv <- origin_data$bkvlps*origin_data$csho
#pb - Price to Book Ratio (mktcap/bookvalue)
origin_data$pb <- origin_data$mktcap/origin_data$bv
#pe - Price to Earnings Ratio (mktcap/ib) (O'Shaughnessy UsesIB)
origin_data$pe <- origin_data$mktcap/origin_data$ib
#cr - Current Ratio (act/lct)
origin_data$cr <- origin_data$act/origin_data$lct
#leverage - Leverage (dltt/at)
origin_data$leverage <- origin_data$dltt/origin_data$at
#turnover - Turnover (revt/at)
origin_data$turnover <- origin_data$sale/origin_data$at
#gross_margin - Gross Margin Ratio (gp/sale)
origin_data$grossMarginRatio <- origin_data$gp/origin_data$sale
#roa <- Return on Assets (ni/ta)
origin_data$ROA <- origin_data$ni/origin_data$at
#oshaughnessy_cfo (ib+db)
origin_data$oshaughnessy_cfo <- origin_data$ib+origin_data$dp
#cfo_assetsratio <- Cash Flow from Operations to Total AssetsRatio ()
origin_data$cfo_assetsratio <- origin_data$oshaughnessy_cfo/origin_data$at
#divyield <- Dividend Yield (dvpsp_c/prcc_c)
origin_data$divYield <- origin_data$dvpsp_c/origin_data$prcc_c

#Decade 
origin_data$Decade <- 0
origin_data$Decade[origin_data$fyear >= 1970 & origin_data$fyear < 1980] <- "1970s"
origin_data$Decade[origin_data$fyear >= 1980 & origin_data$fyear < 1990] <- "1980s"
origin_data$Decade[origin_data$fyear >= 1990 & origin_data$fyear < 2000] <- "1990s"
origin_data$Decade[origin_data$fyear >= 2000 & origin_data$fyear < 2010] <- "2000s"
origin_data$Decade[origin_data$fyear >= 2010] <- "2010s"

#Sector
origin_data$Sector[origin_data$gsector == 10] <- "Energy"
origin_data$Sector[origin_data$gsector == 15] <- "Materials"
origin_data$Sector[origin_data$gsector == 20] <- "Industrials"
origin_data$Sector[origin_data$gsector == 25] <- "Consumer Discretionary"
origin_data$Sector[origin_data$gsector == 30] <- "Consumer Staples"
origin_data$Sector[origin_data$gsector == 35] <- "Health Care"
origin_data$Sector[origin_data$gsector == 40] <- "Financials"
origin_data$Sector[origin_data$gsector == 45] <- "Information Technology"
origin_data$Sector[origin_data$gsector == 50] <- "Telecommunications Services"
origin_data$Sector[origin_data$gsector == 55] <- "Utilities"
origin_data$Sector[origin_data$gsector == 60] <- "Real Estate"

data <- origin_data[ origin_data$fyear >= 1970 &  origin_data$exchg != 0 & origin_data$exchg != 1 &  origin_data$exchg != 3,]

#market cap
df <- data[!is.na(data$mktcap) & data$mktcap<= quantile(data$mktcap, .95, na.rm=TRUE),]

ggplot(data = df, aes(x = mktcap)) +
  geom_histogram(aes(x = mktcap, y = ..density..), bins = 100,col = "black", fill = "lightblue") + 
  geom_density(aes(x = mktcap), col = "darkblue") +  xlab("Market Capitalization") + 
  ylab("Density") +  labs(title = "Market Capitalization Distribution (1970 to 2019)") +
  theme(axis.title.x = element_text(size=12), axis.title.y =element_text(size=12)) + 
  theme_bw()

#bookvalue
df <- data[!is.na(data$bookvalue) &!is.na(data$Sector),]
#Statistics Summary Table
Mean <- numeric(11)
Minimum <- numeric(11)
Q1 <- numeric(11)
Median <- numeric(11)
Q3 <- numeric(11)
Maximum <- numeric(11)
StandardDeviation <- numeric(11)

i <- 1
for(val in seq(10,60,5)) {
  Mean[i] <- mean(df$bv[df$gsector == val], na.rm= TRUE)
  Minimum[i] <- min(df$bv[df$gsector == val],na.rm = TRUE)
  Q1[i] <- quantile(df$bv[df$gsector == val],.25, na.rm = TRUE)
  Median[i] <- median(df$bv[df$gsector == val],na.rm = TRUE)
  Q3[i] <- quantile(df$bv[df$gsector == val],.75, na.rm = TRUE)
  Maximum[i] <- max(df$bv[df$gsector == val],na.rm = TRUE)
  StandardDeviation[i] <- sd(df$bv[df$gsector == val],na.rm = TRUE) 
  i <- i + 1}

sectors <- data.frame(Sector = c("Energy","Materials", "Industrials", "Consumer Discretionary", "ConsumerStaples", "Health Care", "Financials", "Information Technology","Telecommunications Services", "Utilities", "Real Estate"),Mean, Minimum, Q1, Median, Q3, Maximum, StandardDeviation)
write.csv(sectors,"book_table")

df <- data[!is.na(data$bv) &data$bv <= quantile(origin_data$bv, .925,na.rm=TRUE) & data$bv >= quantile(origin_data$bookvalue,.005, na.rm=TRUE),]
ggplot(data = df, aes(x = bv)) +  geom_histogram(aes(x = bv, y = ..density..), bins =100, col = "black", fill = "lightblue") +
  geom_density(aes(x = bv), col = "darkblue") +  xlab("Book Value ($M)") +  ylab("Density") +
  labs(title = "Book Value Distribution (1970 to 2019)") +
  theme(axis.title.x = element_text(size=12), axis.title.y =element_text(size=12)) +
  theme_bw()
#decade
ggplot(data = df, aes(x = bv, fill = Decade)) +  geom_density(aes(x = bv), alpha = .25) +  xlab("Book Value ($M)") +  ylab("Density") +  labs(title = "Book Value Distribution from 1970 to 2019 (By Decade)") +
  theme(axis.title.x = element_text(size=12), axis.title.y =element_text(size=12)) +
  theme_bw()

#Boxplot for Each Year
ggplot(data = df, aes(x = fyear, y = bv, group =fyear)) +  geom_boxplot() +
  stat_summary(fun.y=mean, colour="blue", geom="point", shape=7,size=2) +
  ylim(-50, 225) +  xlab("Year") +  ylab("Book Value ($M)") +
  labs(title = "Boxplots of Book Value from 1970 (By Year)") +
  theme(axis.title.x = element_text(size=15), axis.title.y =element_text(size=15))+
  theme_bw()

#bv by sector
data_i <- data[!is.na(data$bv) &!is.na(data$Sector) & data$bv <=quantile(origin_data$bv, .925, na.rm=TRUE) & data$bv>= quantile(origin_data$bv, .005, na.rm=TRUE),]
ggplot(data = df, aes(x = bv, fill = Sector)) +  geom_histogram(aes(x = bv), bins = 20) +
  facet_wrap(~ Sector) +  xlab("Book Value ($M)") +  ylab("Density") +
  labs(title = "Book Value Histograms By Sector from 1970 to 2019") +
  theme(axis.title.x = element_text(size=12), axis.title.y =element_text(size=12)) + 
  theme_bw() 

ggplot(data = df, aes(x = bv, color = Sector)) +  geom_density() +  xlab("Book Value ($M)") +
  ylab("Density") +  labs(title = "Book Value Distributions By Sector from 1970 to 2019") +  theme(axis.title.x = element_text(size=12), axis.title.y =element_text(size=12)) +
  theme_bw() 


###revt
df <- data[!is.na(data$revt) &!is.na(data$Sector),]
#Statistics Summary Table
Mean <- numeric(11)
Minimum <- numeric(11)
Q1 <- numeric(11)
Median <- numeric(11)
Q3 <- numeric(11)
Maximum <- numeric(11)
StandardDeviation <- numeric(11)

i <- 1
for(val in seq(10,60,5)) {
  Mean[i] <- mean(df$revt[df$gsector == val], na.rm =TRUE)
  Minimum[i] <- min(df$revt[df$gsector == val], na.rm =TRUE) 
  Q1[i] <- quantile(df$revt[df$gsector == val], .25,na.rm = TRUE) 
  Median[i] <- median(df$revt[df$gsector == val], na.rm= TRUE) 
  Q3[i] <- quantile(df$revt[df$gsector == val], .75,na.rm = TRUE) 
  Maximum[i] <- max(df$revt[df$gsector == val], na.rm =TRUE) 
  StandardDeviation[i] <- sd(df$revt[df$gsector == val], na.rm =TRUE)
  i <- i + 1}

sectors <- data.frame(Sector = c("Energy","Materials", "Industrials", "Consumer Discretionary", "ConsumerStaples", "Health Care", "Financials", "Information Technology","Telecommunications Services", "Utilities", "Real Estate"),Mean, Minimum, Q1, Median, Q3, Maximum, StandardDeviation)
write.csv(sectors,"revt_table")

df <- data[!is.na(data$revt) & data$revt <=quantile(origin_data$revt, .925, na.rm=TRUE) & data$revt >=quantile(origin_data$revt, .005, na.rm=TRUE),]

ggplot(data = df, aes(x = revt)) +  geom_histogram(aes(x = revt, y = ..density..), bins = 100, col= "black", fill = "lightblue") +  geom_density(aes(x = revt), col = "darkblue") +  xlab("Revenue ($M)") +  ylab("Density") +  labs(title = "Revenue Distribution (1970 to 2019)") +
  theme(axis.title.x = element_text(size=12), axis.title.y =element_text(size=12)) +
  theme_bw() 

ggplot(data = df, aes(x = revt, fill = Decade)) +  geom_density(aes(x = revt), alpha = .25) +  xlab("Revenue ($M)") +  ylab("Density") +  labs(title = "Revenue Distribution from 1970 to 2019 (By Decade)") + 
  theme(axis.title.x = element_text(size=12), axis.title.y =element_text(size=12)) +
  theme_bw()

ggplot(data = df, aes(x = fyear, y = revt, group = fyear)) +  geom_boxplot() +  stat_summary(fun.y=mean, colour="blue", geom="point", shape=7,size=2) +  ylim(0, 300) +  xlab("Year") +  ylab("Revenue ($M)") +  labs(title = "Boxplots of Revenue from 1970 (By Year)") +
  theme(axis.title.x = element_text(size=12), axis.title.y =element_text(size=12))+
  theme_bw()

df <- data[!is.na(data$revt) &!is.na(data$Sector) & data$revt <= quantile(origin_data$revt,.925, na.rm=TRUE) & data$revt >= quantile(origin_data$revt, .005,na.rm=TRUE),]

ggplot(data = df, aes(x = revt, fill = Sector)) +  
  geom_histogram(aes(x = revt), bins = 20) + 
  facet_wrap(~ Sector) +  xlab("Revenue ($M)") + 
  ylab("Density") +  labs(title = "Revenue Histograms By Sector from 1970 to 2019") +  theme(axis.title.x = element_text(size=12), axis.title.y =element_text(size=12)) + theme_bw()

ggplot(data = df, aes(x = revt, color = Sector)) +  geom_density() +  xlab("Revenue ($M)") +  ylab("Density") +  labs(title = "Revenue Distributions By Sector from 1970 to 2019") +  theme(axis.title.x = element_text(size=12), axis.title.y =element_text(size=12)) + theme_bw()


#ni
df <- data[!is.na(data$ni) &!is.na(data$Sector),]
Mean <- numeric(11)
Minimum <- numeric(11)
Q1 <- numeric(11)
Median <- numeric(11)
Q3 <- numeric(11)
Maximum <- numeric(11)
StandardDeviation <- numeric(11)

i <- 1
for(val in seq(10,60,5)) {
  Mean[i] <- mean(df$ni[df$gsector == val], na.rm =TRUE)  
  Minimum[i] <- min(df$ni[df$gsector == val], na.rm =TRUE)
  Q1[i] <- quantile(df$ni[df$gsector == val], .25, na.rm= TRUE)
  Median[i] <- median(df$ni[df$gsector == val], na.rm =TRUE)  
  Q3[i] <- quantile(df$ni[df$gsector == val], .75, na.rm= TRUE)
  Maximum[i] <- max(df$ni[df$gsector == val], na.rm =TRUE)
  StandardDeviation[i] <- sd(df$ni[df$gsector == val], na.rm =TRUE)  
  i <- i + 1}

sectors <- data.frame(Sector = c("Energy","Materials", "Industrials", "Consumer Discretionary", "ConsumerStaples", "Health Care", "Financials", "Information Technology","Telecommunications Services", "Utilities", "Real Estate"),Mean, Minimum, Q1, Median, Q3, Maximum, StandardDeviation)
write.csv(sectors,"ni_table")

df <- data[!is.na(data$ni) & data$ni <=quantile(origin_data$ni, .95, na.rm=TRUE) & data$ni >=quantile(origin_data$ni, .02, na.rm=TRUE),]

ggplot(data = df, aes(x = ni)) +  geom_histogram(aes(x = ni, y = ..density..), bins = 100, col ="black", fill = "lightblue") +  geom_density(aes(x = ni), col = "darkblue") +  xlab("Net Income ($M)") +  ylab("Density") +  labs(title = "Net Income Distribution (1970 to 2019)") + 
  theme(axis.title.x = element_text(size=12), axis.title.y =element_text(size=12)) + theme_bw()

ggplot(data = df, aes(x = ni, fill = Decade)) +  geom_density(aes(x = ni), alpha = .25) +  xlab("Net Income ($M)") +  ylab("Density") +  labs(title = "Net Income Distribution from 1970 to 2019 (By Decade)") +
  theme(axis.title.x = element_text(size=12), axis.title.y =element_text(size=12)) +
  theme_bw()

ggplot(data = df, aes(x = fyear, y = ni, group = fyear)) +  geom_boxplot() + 
  stat_summary(fun.y=mean, colour="blue", geom="point", shape=7,size=2) + 
  ylim(-50, 150) +  xlab("Year") +  ylab("Net Income ($M)") +  
  labs(title = "Boxplots of Net Income from 1970 (By Year)") +  
  theme(axis.title.x = element_text(size=15), axis.title.y =element_text(size=15)) +
  theme_bw()

df <- data[!is.na(data$ni) &!is.na(data$Sector) & data$ni <= quantile(origin_data$ni,.95, na.rm=TRUE) & data$ni >= quantile(origin_data$ni, .02,na.rm=TRUE),]

ggplot(data = df, aes(x = ni, fill = Sector)) +  geom_histogram(aes(x = ni), bins = 20) +  facet_wrap(~ Sector) +  xlab("Net Income ($M)") +  ylab("Density") +  labs(title = "Net Income Histograms By Sector from 1970 to 2019") + 
  theme(axis.title.x = element_text(size=12), axis.title.y =element_text(size=12)) +
  theme_bw()

ggplot(data = df, aes(x = ni, color = Sector)) +  geom_density() +  xlab("Net Income ($M)") +  ylab("Density") +  labs(title = "Net Income Distributions By Sector from 1970 to 2019") +
  theme(axis.title.x = element_text(size=12), axis.title.y =element_text(size=12)) +
  theme_bw()

#epspx
df <- data[!is.na(data$epspx) &!is.na(data$Sector) & data$epspx > 0,]
Mean <- numeric(11)
Minimum <- numeric(11)
Q1 <- numeric(11)
Median <- numeric(11)
Q3 <- numeric(11)
Maximum <- numeric(11)
StandardDeviation <- numeric(11)

i <- 1
for(val in seq(10,60,5)) {
  Mean[i] <- mean(df$epspx[df$gsector == val], na.rm =TRUE) 
  Minimum[i] <- min(df$epspx[df$gsector == val], na.rm =TRUE) 
  Q1[i] <- quantile(df$epspx[df$gsector == val], .25,na.rm = TRUE) 
  Median[i] <- median(df$epspx[df$gsector == val], na.rm= TRUE) 
  Q3[i] <- quantile(df$epspx[df$gsector == val], .75,na.rm = TRUE) 
  Maximum[i] <- max(df$epspx[df$gsector == val], na.rm =TRUE) 
  StandardDeviation[i] <- sd(df$epspx[df$gsector == val], na.rm =TRUE)  
  i <- i + 1}

sectors <- data.frame(Sector = c("Energy","Materials", "Industrials", "Consumer Discretionary", "ConsumerStaples", "Health Care", "Financials", "Information Technology","Telecommunications Services", "Utilities", "Real Estate"),Mean, Minimum, Q1, Median, Q3, Maximum,StandardDeviation)
write.csv(sectors,"epspx_table")

df <- data[!is.na(data$epspx) & data$epspx <=quantile(origin_data$epspx, .985, na.rm=TRUE) & data$epspx > 0,]

ggplot(data = df, aes(x = epspx)) +  geom_histogram(aes(x = epspx, y = ..density..), bins = 100,col = "black", fill = "lightblue") +  geom_density(aes(x = epspx), col = "darkblue") +  xlab("Earnings per Share ($)") +  ylab("Density") +  labs(title = "Earnings per Share Distribution (1970 to 2019)")+
  theme(axis.title.x = element_text(size=12), axis.title.y =element_text(size=12)) +
  theme_bw()


ggplot(data = df, aes(x = epspx, fill = Decade)) +  geom_density(aes(x = epspx), alpha = .25) +  xlab("Earnings per Share ($)") +  ylab("Density") +  labs(title = "Earnings per Share Distribution from 1970 to 2019 (By Decade)") + 
  theme(axis.title.x = element_text(size=12), axis.title.y =element_text(size=12)) +
  theme_bw()
 
ggplot(data = df, aes(x = fyear, y = epspx, group = fyear))+  geom_boxplot() + 
  stat_summary(fun.y=mean, colour="blue", geom="point", shape=7,size=2) +
  ylim(0, 6) +  xlab("Year") +  ylab("Earnings per Share ($)") + 
  labs(title = "Boxplots of Earnings per Share from 1970 (By Year)") + 
  theme(axis.title.x = element_text(size=12), axis.title.y =element_text(size=12)) +
  theme_bw()

df <- data[!is.na(data$epspx) &!is.na(data$Sector) & data$epspx <=quantile(origin_data$epspx, .985, na.rm=TRUE) & data$epspx > 0,]

ggplot(data = df, aes(x = epspx, fill = Sector)) + 
  geom_histogram(aes(x = epspx), bins = 20) +
  facet_wrap(~ Sector) +  xlab("Earnings per Share ($)") +
  ylab("Density") +
  labs(title = "Earnings per Share Histograms By Sector from 1970 to 2019") + 
  theme(axis.title.x = element_text(size=12), axis.title.y =element_text(size=12)) +
  theme_bw()

ggplot(data = df, aes(x = epspx, color = Sector)) +  geom_density() +  xlab("Earnings per Share ($)") +  ylab("Density") +  labs(title = "Earnings per Share Distributions By Sector from 1970 to 2019") + 
  theme(axis.title.x = element_text(size=12), axis.title.y =element_text(size=12)) +
  theme_bw()

#pb
df <- data[!is.na(data$pb) &!is.na(data$Sector) & data$pb > 0 & data$pb <10000,]
Mean <- numeric(11)
Minimum <- numeric(11)
Q1 <- numeric(11)
Median <- numeric(11)
Q3 <- numeric(11)
Maximum <- numeric(11)
StandardDeviation <- numeric(11)

i <- 1
for(val in seq(10,60,5)) {
  Mean[i] <- mean(df$pb[df$gsector == val], na.rm =TRUE) 
  Minimum[i] <- min(df$pb[df$gsector == val], na.rm =TRUE) 
  Q1[i] <- quantile(df$pb[df$gsector == val], .25,na.rm = TRUE) 
  Median[i] <- median(df$pb[df$gsector == val], na.rm= TRUE) 
  Q3[i] <- quantile(df$pb[df$gsector == val], .75,na.rm = TRUE) 
  Maximum[i] <- max(df$pb[df$gsector == val], na.rm =TRUE) 
  StandardDeviation[i] <- sd(df$pb[df$gsector == val], na.rm =TRUE)  
  i <- i + 1}

sectors <- data.frame(Sector = c("Energy","Materials", "Industrials", "Consumer Discretionary", "ConsumerStaples", "Health Care", "Financials", "Information Technology","Telecommunications Services", "Utilities", "Real Estate"),Mean, Minimum, Q1, Median, Q3, Maximum,StandardDeviation)
write.csv(sectors,"pb_table")

df <- data[!is.na(data$pb) & data$pb <=quantile(origin_data$pb, .97, na.rm=TRUE) & data$pb >=quantile(origin_data$pb, .02, na.rm=TRUE),]

ggplot(data = df, aes(x = pb)) + 
  geom_histogram(aes(x = pb, y = ..density..), bins = 100, col ="black", fill = "lightblue") +
  geom_density(aes(x = pb), col = "darkblue") +  xlab("Price to Book Ratio") + 
  ylab("Density") +  labs(title = "Price to Book Ratio Distribution from 1970 to 2019") + 
  theme(axis.title.x = element_text(size=12), axis.title.y =element_text(size=12))+
  theme_bw()

ggplot(data = df, aes(x = pb, fill = Decade)) + 
  geom_density(aes(x = pb), alpha = .25) +  xlab("Price to Book Ratio") + 
  ylab("Density") +  labs(title = "Price to Book Ration Distribution from 1970 to 2019 (By Decade)")+ 
  theme(axis.title.x = element_text(size=12), axis.title.y =element_text(size=12))


ggplot(data = df, aes(x = fyear, y = pb, group = fyear)) + 
  geom_boxplot() + 
  stat_summary(fun.y=mean, colour="blue", geom="point", shape=7,size=2) +
  ylim(-2, 8) +  xlab("Year") +  ylab("Price to Book Ratio") +
  labs(title = "Boxplots of Price to Book Ratio from 1970 (By Year)") + 
  theme(axis.title.x = element_text(size=12), axis.title.y =element_text(size=12)) +
  theme_bw()

df <- data[!is.na(data$pb) &!is.na(data$Sector) & data$pb <= quantile(origin_data$pb,.97, na.rm=TRUE) & data$pb >= quantile(origin_data$pb, .02,na.rm=TRUE),]

ggplot(data = df, aes(x = pb, fill = Sector)) + 
  geom_histogram(aes(x = pb), bins = 20) + 
  facet_wrap(~ Sector) +  xlab("Price to Book Ratio") + 
  ylab("Density") +  labs(title = "Price to Book Ratio Histograms By Sector from 1970 to 2019") +
  theme(axis.title.x = element_text(size=12), axis.title.y =element_text(size=12)) +
  theme_bw()

ggplot(data = df, aes(x = pb, color = Sector)) +  geom_density() +  xlab("Price to Book Ratio") +  ylab("Density") + 
  labs(title = "Price to Book Ratio Distributions By Sector from 1970 to 2019") + 
  theme(axis.title.x = element_text(size=12), axis.title.y =element_text(size=12)) +
  theme_bw()

#pe
df <- data[!is.na(data$pe) &!is.na(data$Sector) & data$pe > 0 & data$pe <10000,]
Mean <- numeric(11)
Minimum <- numeric(11)
Q1 <- numeric(11)
Median <- numeric(11)
Q3 <- numeric(11)
Maximum <- numeric(11)
StandardDeviation <- numeric(11)

i <- 1
for(val in seq(10,60,5)) {
  Mean[i] <- mean(df$pe[df$gsector == val], na.rm =TRUE) 
  Minimum[i] <- min(df$pe[df$gsector == val], na.rm =TRUE) 
  Q1[i] <- quantile(df$pe[df$gsector == val], .25,na.rm = TRUE) 
  Median[i] <- median(df$pe[df$gsector == val], na.rm= TRUE) 
  Q3[i] <- quantile(df$pe[df$gsector == val], .75,na.rm = TRUE) 
  Maximum[i] <- max(df$pe[df$gsector == val], na.rm =TRUE) 
  StandardDeviation[i] <- sd(df$pe[df$gsector == val], na.rm =TRUE)  
  i <- i + 1}

sectors <- data.frame(Sector = c("Energy","Materials", "Industrials", "Consumer Discretionary", "ConsumerStaples", "Health Care", "Financials", "Information Technology","Telecommunications Services", "Utilities", "Real Estate"),Mean, Minimum, Q1, Median, Q3, Maximum,StandardDeviation)
write.csv(sectors,"pe_table")

df <- data[!is.na(data$pe) & data$pe > 0 &data$pe <= quantile(origin_data$pe, .97, na.rm=TRUE),]
ggplot(data = df, aes(x = pe)) +
  geom_histogram(aes(x = pe, y = ..density..), bins = 100, col ="black", fill = "lightblue") + 
  geom_density(aes(x = pe), col = "darkblue") +  xlab("Price to Earnings Ratio") + 
  ylab("Density") +  labs(title = "Price to Earnings Ratio Distribution from 1970 to 2019") + 
  theme(axis.title.x = element_text(size=12), axis.title.y =element_text(size=12)) +
  theme_bw()


ggplot(data = df, aes(x = pe, fill = Decade)) + 
  geom_density(aes(x = pe), alpha = .25) +
  xlab("Price to Earnings Ratio") + 
  ylab("Density") +  
  labs(title = "Price to Earnings Ratio Distribution from 1970 to 2019 (By Decade)") + 
  theme(axis.title.x = element_text(size=12), axis.title.y =element_text(size=12)) +
  theme_bw()

ggplot(data = df, aes(x = fyear, y = pe, group = fyear)) + 
  geom_boxplot() +  stat_summary(fun.y=mean, colour="blue", geom="point", shape=7,size=2) +
  ylim(0, 75) +  xlab("Year") +  ylab("Price to Earnings Ratio") + 
  labs(title = "Boxplots of Price to Earnings Ratio from 1970 (By Year)")+ 
  theme(axis.title.x = element_text(size=15), axis.title.y =element_text(size=15)) +
  theme_bw()

df <- data[!is.na(data$pe) &!is.na(data$Sector) & data$pe <= quantile(origin_data$pe,.97, na.rm=TRUE) & data$pe > 0,]
ggplot(data = df, aes(x = pe, fill = Sector)) + 
  geom_histogram(aes(x = pe), bins = 20) +  facet_wrap(~ Sector) +
  xlab("Price to Earnings Ratio") +  ylab("Density") +  labs(title = "Price to Earnings Ratio Histograms By Sector from 1970 to 2019") + 
  theme(axis.title.x = element_text(size=12), axis.title.y =element_text(size=12))

ggplot(data = df, aes(x = pe, color = Sector)) +  geom_density() +  xlab("Price to Earnings Ratio") +  ylab("Density") +  labs(title = "Price to Earnings Ratio Distributions By Sector from 1970 to 2019") +
  theme(axis.title.x = element_text(size=12), axis.title.y =element_text(size=12)) +
  theme_bw()

#divYield
df <- data[!is.na(data$divYield) &!is.na(data$Sector) & data$dvpsp_c > 0,]

Mean <- numeric(11)
Minimum <- numeric(11)
Q1 <- numeric(11)
Median <- numeric(11)
Q3 <- numeric(11)
Maximum <- numeric(11)
StandardDeviation <- numeric(11)

i <- 1
for(val in seq(10,60,5)) {
  Mean[i] <- mean(df$divYield[df$gsector == val], na.rm =TRUE) 
  Minimum[i] <- min(df$divYield[df$gsector == val], na.rm =TRUE) 
  Q1[i] <- quantile(df$divYield[df$gsector == val], .25,na.rm = TRUE) 
  Median[i] <- median(df$divYield[df$gsector == val], na.rm= TRUE) 
  Q3[i] <- quantile(df$divYield[df$gsector == val], .75,na.rm = TRUE) 
  Maximum[i] <- max(df$divYield[df$gsector == val], na.rm =TRUE) 
  StandardDeviation[i] <- sd(df$divYield[df$gsector == val], na.rm =TRUE)  
  i <- i + 1}

sectors <- data.frame(Sector = c("Energy","Materials", "Industrials", "Consumer Discretionary", "ConsumerStaples", "Health Care", "Financials", "Information Technology","Telecommunications Services", "Utilities", "Real Estate"),Mean, Minimum, Q1, Median, Q3, Maximum,StandardDeviation)
write.csv(sectors,"divYield_table.csv")

df <- data[!is.na(data$divYield) &data$dvpsp_c > 0 & data$divYield <=quantile(origin_data$divYield, .99, na.rm=TRUE),]

ggplot(data = df, aes(x = divYield)) + 
  geom_histogram(aes(x = divYield, y = ..density..), bins = 100,col = "black", fill = "lightblue") + 
  geom_density(aes(x = divYield), col = "darkblue") +  xlab("Dividend Yield") +  ylab("Density") +  
  labs(title = "Dividend Yield Distribution from 1970 to 2019") + 
  theme(axis.title.x = element_text(size=12), axis.title.y =element_text(size=12)) +
  theme_bw()

ggplot(data = df, aes(x = divYield, fill = Decade)) +
  geom_density(aes(x = divYield), alpha = .25) +
  xlab("Divident Yield") +  ylab("Density") +  
  labs(title = "Dividend Yield Distribution from 1970 to 2019 (By Decade)") +
  theme(axis.title.x = element_text(size=12), axis.title.y =element_text(size=12)) +
  theme_bw()


ggplot(data = df, aes(x = fyear, y = divYield, group =fyear)) +
  geom_boxplot() +  stat_summary(fun.y=mean, colour="blue", geom="point", shape=7,size=2) +
  ylim(0, .15) +  xlab("Year") +  ylab("Divident Yield") +  
  labs(title = "Boxplots of Dividend Yield from 1970 (By Year)") + 
  theme(axis.title.x = element_text(size=15), axis.title.y =element_text(size=15))

df <- data[!is.na(data$divYield) &!is.na(data$Sector) & data$divYield <=quantile(origin_data$divYield, .99, na.rm=TRUE) & data$divYield >0,]

ggplot(data = df, aes(x = divYield, fill = Sector)) + 
  geom_histogram(aes(x = divYield), bins = 20) + 
  facet_wrap(~ Sector) + 
  xlab("Dividend Yield") + 
  ylab("Density") +
  labs(title = "Dividend Yield Histograms By Sector from 1970 to 2019") +
  theme(axis.title.x = element_text(size=12), axis.title.y =element_text(size=12))

ggplot(data = df, aes(x = divYield, color = Sector)) +
  geom_density() +  xlab("Dividend Yield") + 
  ylab("Density") +  
  labs(title = "Dividend Yield Distributions By Sector from 1970 to 2019") + 
  theme(axis.title.x = element_text(size=12), axis.title.y =element_text(size=12))

#cr

df <- data[!is.na(data$cr) &!is.na(data$Sector) & data$lct != 0,]

Mean <- numeric(11)
Minimum <- numeric(11)
Q1 <- numeric(11)
Median <- numeric(11)
Q3 <- numeric(11)
Maximum <- numeric(11)
StandardDeviation <- numeric(11)

i <- 1
for(val in seq(10,60,5)) {
  Mean[i] <- mean(df$cr[df$gsector == val], na.rm =TRUE) 
  Minimum[i] <- min(df$cr[df$gsector == val], na.rm =TRUE) 
  Q1[i] <- quantile(df$cr[df$gsector == val], .25,na.rm = TRUE) 
  Median[i] <- median(df$cr[df$gsector == val], na.rm= TRUE) 
  Q3[i] <- quantile(df$cr[df$gsector == val], .75,na.rm = TRUE) 
  Maximum[i] <- max(df$cr[df$gsector == val], na.rm =TRUE) 
  StandardDeviation[i] <- sd(df$cr[df$gsector == val], na.rm =TRUE)  
  i <- i + 1}

sectors <- data.frame(Sector = c("Energy","Materials", "Industrials", "Consumer Discretionary", "ConsumerStaples", "Health Care", "Financials", "Information Technology","Telecommunications Services", "Utilities", "Real Estate"),Mean, Minimum, Q1, Median, Q3, Maximum,StandardDeviation)
write.csv(sectors,"cr.csv")


df <- data[!is.na(data$cr) & data$cr <=quantile(origin_data$pb, .97, na.rm=TRUE) & data$cr >=quantile(origin_data$pb, .02, na.rm=TRUE),]

ggplot(data = df, aes(x = cr)) + 
  geom_histogram(aes(x = cr, y = ..density..), bins = 100,col = "black", fill = "lightblue") + 
  geom_density(aes(x = cr), col = "darkblue") +  xlab("Current Ratio") +  ylab("Density") +  
  labs(title = "Current Ratio Distribution from 1970 to 2019") + 
  theme(axis.title.x = element_text(size=12), axis.title.y =element_text(size=12)) +
  theme_bw()

ggplot(data = df, aes(x = cr, fill = Decade)) +
  geom_density(aes(x = cr), alpha = .25) +
  xlab("Current Ratio") +  ylab("Density") +  
  labs(title = "Current Ratio Distribution from 1970 to 2019 (By Decade)") +
  theme(axis.title.x = element_text(size=12), axis.title.y =element_text(size=12)) +
  theme_bw()


ggplot(data = df, aes(x = fyear, y = cr, group =fyear)) +
  geom_boxplot() +  stat_summary(fun.y=mean, colour="blue", geom="point", shape=7,size=2) +
  ylim(0, .15) +  xlab("Year") +  ylab("Current Ratio") +  
  labs(title = "Boxplots of Current Ratio from 1970 (By Year)") + 
  theme(axis.title.x = element_text(size=15), axis.title.y =element_text(size=15)) +
  theme_bw()

df <- data[!is.na(data$cr) &!is.na(data$Sector) & data$cr <= quantile(origin_data$cr,.97, na.rm=TRUE) & data$cr >= quantile(origin_data$cr, .02,na.rm=TRUE),]
ggplot(data = df, aes(x = cr, fill = Sector)) + 
  geom_histogram(aes(x = cr), bins = 20) + 
  facet_wrap(~ Sector) + 
  xlab("Current Ratio") + 
  ylab("Density") +
  labs(title = "Current Ratio Histograms By Sector from 1970 to 2019") +
  theme(axis.title.x = element_text(size=12), axis.title.y =element_text(size=12)) + theme_bw()

ggplot(data = df, aes(x = cr, color = Sector)) +
  geom_density() +  xlab("Current Ratio") + 
  ylab("Density") +  
  labs(title = "Current Ratio Distributions By Sector from 1970 to 2019") + 
  theme(axis.title.x = element_text(size=12), axis.title.y =element_text(size=12)) +
  theme_bw()

#leverage
df <- data[!is.na(data$leverage) &!is.na(data$Sector) & data$at != 0 & data$dltt >0,]

Mean <- numeric(11)
Minimum <- numeric(11)
Q1 <- numeric(11)
Median <- numeric(11)
Q3 <- numeric(11)
Maximum <- numeric(11)
StandardDeviation <- numeric(11)

i <- 1
for(val in seq(10,60,5)) {
  Mean[i] <- mean(df$leverage[df$gsector == val], na.rm =TRUE) 
  Minimum[i] <- min(df$leverage[df$gsector == val], na.rm =TRUE) 
  Q1[i] <- quantile(df$leverage[df$gsector == val], .25,na.rm = TRUE) 
  Median[i] <- median(df$leverage[df$gsector == val], na.rm= TRUE) 
  Q3[i] <- quantile(df$leverage[df$gsector == val], .75,na.rm = TRUE) 
  Maximum[i] <- max(df$leverage[df$gsector == val], na.rm =TRUE) 
  StandardDeviation[i] <- sd(df$leverage[df$gsector == val], na.rm =TRUE)  
  i <- i + 1}

sectors <- data.frame(Sector = c("Energy","Materials", "Industrials", "Consumer Discretionary", "ConsumerStaples", "Health Care", "Financials", "Information Technology","Telecommunications Services", "Utilities", "Real Estate"),Mean, Minimum, Q1, Median, Q3, Maximum,StandardDeviation)
write.csv(sectors,"leverage.csv")

df <- data[!is.na(data$leverage) &data$leverage <= quantile(origin_data$leverage, .95, na.rm=TRUE) &data$dltt > 0,]

ggplot(data = df, aes(x = leverage)) + 
  geom_histogram(aes(x = leverage, y = ..density..), bins = 100,col = "black", fill = "lightblue") + 
  geom_density(aes(x = leverage), col = "darkblue") +
  xlab("Leverage") +  ylab("Density") +  
  labs(title = "Leverage Distribution from 1970 to 2019") +  
  theme(axis.title.x = element_text(size=12), axis.title.y =element_text(size=12)) +
  theme_bw()

ggplot(data = df, aes(x = leverage, fill = Decade)) +
  geom_density(aes(x = leverage), alpha = .25) + 
  xlab("Leverage") +  ylab("Density") +  
  labs(title = "Leverage Distribution from 1970 to 2019 (By Decade)") + 
  theme(axis.title.x = element_text(size=12), axis.title.y =element_text(size=12)) +
  theme_bw()


ggplot(data = df, aes(x = fyear, y = leverage, group =fyear)) +
  geom_boxplot() +  stat_summary(fun.y=mean, colour="blue", geom="point", shape=7,size=2) + 
  ylim(0, .4) +  xlab("Year") +
  ylab("Leverage") +  labs(title = "Boxplots of Leverage from 1970 (By Year)") + 
  theme(axis.title.x = element_text(size=15), axis.title.y =element_text(size=15)) +
  theme_bw()

df <- data[!is.na(data$leverage) &!is.na(data$Sector) & data$leverage <=quantile(origin_data$cr, .85, na.rm=TRUE) & data$dltt > 0,]


ggplot(data = df, aes(x = leverage, fill = Sector)) +  
  geom_histogram(aes(x = leverage), bins = 20) + 
  facet_wrap(~ Sector) +  xlab("Leverage") + 
  ylab("Density") +  labs(title = "Leverage Histograms By Sector from 1970 to 2019")+  
  theme(axis.title.x = element_text(size=12), axis.title.y =element_text(size=12)) +
  theme_bw()

ggplot(data = df, aes(x = leverage, color = Sector)) +
  geom_density() +  xlab("Leverage") + 
  ylab("Density") + 
  labs(title = "Leverage Distributions By Sector from 1970 to 2019") + 
  theme(axis.title.x = element_text(size=12), axis.title.y =element_text(size=12)) +
  theme_bw()


#turnover
df <- data[!is.na(data$turnover) &!is.na(data$Sector) & data$at != 0 & data$sale >0,]


Mean <- numeric(11)
Minimum <- numeric(11)
Q1 <- numeric(11)
Median <- numeric(11)
Q3 <- numeric(11)
Maximum <- numeric(11)
StandardDeviation <- numeric(11)

i <- 1
for(val in seq(10,60,5)) {
  Mean[i] <- mean(df$turnover[df$gsector == val], na.rm =TRUE) 
  Minimum[i] <- min(df$turnover[df$gsector == val], na.rm =TRUE) 
  Q1[i] <- quantile(df$turnover[df$gsector == val], .25,na.rm = TRUE) 
  Median[i] <- median(df$turnover[df$gsector == val], na.rm= TRUE) 
  Q3[i] <- quantile(df$turnover[df$gsector == val], .75,na.rm = TRUE) 
  Maximum[i] <- max(df$turnover[df$gsector == val], na.rm =TRUE) 
  StandardDeviation[i] <- sd(df$turnover[df$gsector == val], na.rm =TRUE)  
  i <- i + 1}

sectors <- data.frame(Sector = c("Energy","Materials", "Industrials", "Consumer Discretionary", "ConsumerStaples", "Health Care", "Financials", "Information Technology","Telecommunications Services", "Utilities", "Real Estate"),Mean, Minimum, Q1, Median, Q3, Maximum,StandardDeviation)
write.csv(sectors,"turnover.csv")

df <- data[!is.na(data$turnover) &data$turnover <= quantile(origin_data$turnover, .95, na.rm=TRUE) &data$sale > 0,]


ggplot(data = df, aes(x = turnover)) +
  geom_histogram(aes(x = turnover, y = ..density..), bins = 100,col = "black", fill = "lightblue") + 
  geom_density(aes(x = turnover), col = "darkblue") +
  xlab("Turnover") +  ylab("Density") +  
  labs(title = "Turnover Distribution from 1970 to 2019") +
  theme(axis.title.x = element_text(size=12), axis.title.y =element_text(size=12)) +
  theme_bw()

ggplot(data = df, aes(x = turnover, fill = Decade)) +  
  geom_density(aes(x = turnover), alpha = .25) + 
  xlab("Turnover") +  ylab("Density") +  
  labs(title = "Turnover Distribution from 1970 to 2019 (By Decade)") +
  theme(axis.title.x = element_text(size=12), axis.title.y =element_text(size=12)) +
  theme_bw()

ggplot(data = df, aes(x = fyear, y = turnover, group =fyear)) + 
  geom_boxplot() +  stat_summary(fun.y=mean, colour="blue", geom="point", shape=7,size=2) + 
  ylim(0, 2.6) +  xlab("Year") +  ylab("Turnover") +  
  labs(title = "Boxplots of Turnover from 1970 (By Year)") +
  theme(axis.title.x = element_text(size=12), axis.title.y =element_text(size=12)) +
  theme_bw()

df <- data[!is.na(data$turnover) &!is.na(data$Sector) & data$turnover <=quantile(origin_data$cr, .90, na.rm=TRUE) & data$sale > 0,]
ggplot(data = df, aes(x = turnover, fill = Sector)) + 
  geom_histogram(aes(x = turnover), bins = 20) +  facet_wrap(~ Sector) + 
  xlab("Turnover") +  ylab("Density") + 
  labs(title = "Turnover Histograms By Sector from 1970 to 2019")+  
  theme(axis.title.x = element_text(size=12), axis.title.y =element_text(size=12)) +
  theme_bw()

ggplot(data = df, aes(x = turnover, color = Sector)) +
  geom_density() +  xlab("Turnover") +  ylab("Density") +
  labs(title = "Turnover Distributions By Sector from 1970 to 2019") + 
  theme(axis.title.x = element_text(size=12), axis.title.y =element_text(size=12)) +
   theme_bw()

#gross margin ratio
df <- data[!is.na(data$grossMarginRatio) &!is.na(data$Sector) & data$sale != 0,]


Mean <- numeric(11)
Minimum <- numeric(11)
Q1 <- numeric(11)
Median <- numeric(11)
Q3 <- numeric(11)
Maximum <- numeric(11)
StandardDeviation <- numeric(11)

i <- 1
for(val in seq(10,60,5)) {
  Mean[i] <- mean(df$grossMarginRatio[df$gsector == val], na.rm =TRUE) 
  Minimum[i] <- min(df$grossMarginRatio[df$gsector == val], na.rm =TRUE) 
  Q1[i] <- quantile(df$grossMarginRatio[df$gsector == val], .25,na.rm = TRUE) 
  Median[i] <- median(df$grossMarginRatio[df$gsector == val], na.rm= TRUE) 
  Q3[i] <- quantile(df$grossMarginRatio[df$gsector == val], .75,na.rm = TRUE) 
  Maximum[i] <- max(df$grossMarginRatio[df$gsector == val], na.rm =TRUE) 
  StandardDeviation[i] <- sd(df$grossMarginRatio[df$gsector == val], na.rm =TRUE)  
  i <- i + 1}

sectors <- data.frame(Sector = c("Energy","Materials", "Industrials", "Consumer Discretionary", "ConsumerStaples", "Health Care", "Financials", "Information Technology","Telecommunications Services", "Utilities", "Real Estate"),Mean, Minimum, Q1, Median, Q3, Maximum,StandardDeviation)
write.csv(sectors,"grossMargin.csv")


df <- data[!is.na(data$grossMarginRatio) &data$grossMarginRatio <= 1 & data$grossMarginRatio > 0,]

ggplot(data = df, aes(x = grossMarginRatio)) + 
  geom_histogram(aes(x = grossMarginRatio, y = ..density..), bins =100, col = "black", fill = "lightblue") + 
  geom_density(aes(x = grossMarginRatio), col = "darkblue") + 
  xlab("Gross Margin Ratio") +  ylab("Density") + 
  labs(title = "Gross Margin Ratio Distribution from 1970 to 2019")+ 
  theme(axis.title.x = element_text(size=12), axis.title.y =element_text(size=12)) +
  theme_bw()

ggplot(data = df, aes(x = grossMarginRatio, fill = Decade)) +
  geom_density(aes(x = grossMarginRatio), alpha = .25) + 
  xlab("Gross Margin Ratio") + 
  ylab("Density") +  
  labs(title = "Gross Margin Ratio Distribution from 1970 to 2019 (By Decade)") +  
  theme(axis.title.x = element_text(size=12), axis.title.y =element_text(size=12)) +
  theme_bw()

ggplot(data = df, aes(x = fyear, y = grossMarginRatio, group =fyear)) + 
  geom_boxplot() +
  stat_summary(fun.y=mean, colour="blue", geom="point", shape=7,size=2) +  
  ylim(0, 1) +  xlab("Year") +  ylab("Gross Margin Ratio") + 
  labs(title = "Boxplots of Gross Margin Ratios from 1970 (By Year)") +
  theme(axis.title.x = element_text(size=12), axis.title.y =element_text(size=12)) +
  theme_bw()

df <- data[!is.na(data$grossMarginRatio) &!is.na(data$Sector) & data$grossMarginRatio <= 1 &data$grossMarginRatio > 0,]

ggplot(data = df, aes(x = grossMarginRatio, fill = Sector)) + 
  geom_histogram(aes(x = grossMarginRatio), bins = 20) + 
  facet_wrap(~ Sector) +  
  xlab("Gross Margin Ratio") + 
  ylab("Density") + 
  labs(title = "Gross Margin Ratio Histograms By Sector from 1970 to 2019") + 
  theme(axis.title.x = element_text(size=12), axis.title.y =element_text(size=12)) +
  theme_bw()

ggplot(data = df, aes(x = grossMarginRatio, color = Sector)) + 
  geom_density() +  xlab("Gross Margin Ratio") + 
  ylab("Density") +
  labs(title = "Gross Margin Ratio Distributions By Sector from 1970 to 2019") + 
  theme(axis.title.x = element_text(size=12), axis.title.y =element_text(size=12)) +
  theme_bw()

#ROA
df <- data[!is.na(data$ROA) &!is.na(data$Sector) & data$at != 0,]


Mean <- numeric(11)
Minimum <- numeric(11)
Q1 <- numeric(11)
Median <- numeric(11)
Q3 <- numeric(11)
Maximum <- numeric(11)
StandardDeviation <- numeric(11)

i <- 1
for(val in seq(10,60,5)) {
  Mean[i] <- mean(df$ROA[df$gsector == val], na.rm =TRUE) 
  Minimum[i] <- min(df$ROA[df$gsector == val], na.rm =TRUE) 
  Q1[i] <- quantile(df$ROA[df$gsector == val], .25,na.rm = TRUE) 
  Median[i] <- median(df$ROA[df$gsector == val], na.rm= TRUE) 
  Q3[i] <- quantile(df$ROA[df$gsector == val], .75,na.rm = TRUE) 
  Maximum[i] <- max(df$ROA[df$gsector == val], na.rm =TRUE) 
  StandardDeviation[i] <- sd(df$ROA[df$gsector == val], na.rm =TRUE)  
  i <- i + 1}

sectors <- data.frame(Sector = c("Energy","Materials", "Industrials", "Consumer Discretionary", "ConsumerStaples", "Health Care", "Financials", "Information Technology","Telecommunications Services", "Utilities", "Real Estate"),Mean, Minimum, Q1, Median, Q3, Maximum,StandardDeviation)
write.csv(sectors,"ROA.csv")

df <- data[!is.na(data$ROA) & data$at != 0 &data$ROA <= 1.2 & data$ROA >= quantile(origin_data$turnover,.02, na.rm=TRUE),]

ggplot(data = df, aes(x = ROA)) + 
  geom_histogram(aes(x = ROA, y = ..density..), bins = 100, col= "black", fill = "lightblue") + 
  geom_density(aes(x = ROA), col = "darkblue") + 
  xlab("Return on Assets") +  ylab("Density") +  
  labs(title = "Return on Assets Distribution from 1970 to 2019") + 
  theme(axis.title.x = element_text(size=12), axis.title.y =element_text(size=12)) +
  theme_bw()

ggplot(data = df, aes(x = ROA, fill = Decade)) +  
  geom_density(aes(x = ROA), alpha = .25) + 
  xlab("Return on Assets") +  ylab("Density") +  
  labs(title = "Return on Assets Distribution from 1970 to 2019 (By Decade)") + 
  theme(axis.title.x = element_text(size=12), axis.title.y =element_text(size=12)) +
  theme_bw()


ggplot(data = df, aes(x = fyear, y = ROA, group = fyear)) +
  geom_boxplot() +  stat_summary(fun.y=mean, colour="blue", geom="point", shape=7,size=2) +
  ylim(0,.4) +  xlab("Year") +  ylab("Gross Margin Ratio") +  
  labs(title = "Boxplots of Returns on Assets from 1970 (By Year)") +  
  theme(axis.title.x = element_text(size=12), axis.title.y =element_text(size=12)) +
  theme_bw()

df <- data[!is.na(data$ROA) &!is.na(data$Sector) & data$ROA <= .5 & data$ROA>= 0,]

ggplot(data = df, aes(x = ROA, fill = Sector)) + 
  geom_histogram(aes(x = ROA), bins = 20) +  
  facet_wrap(~ Sector) +  xlab("Return on Assets") + 
  ylab("Density") +  
  labs(title = "Return on Assets Histograms By Sector from 1970 to 2019") + 
  theme(axis.title.x = element_text(size=12), axis.title.y =element_text(size=12)) +
  theme_bw()

ggplot(data = df, aes(x = ROA, color = Sector)) + 
  geom_density() +  xlab("Return on Assets") +  ylab("Density") +
  labs(title = "Return on Assets Distributions By Sector from 1970 to 2019") + 
  theme(axis.title.x = element_text(size=12), axis.title.y =element_text(size=12)) +
  theme_bw()

#cfo_assetsratio
df <- data[!is.na(data$cfo_assetsratio) &!is.na(data$Sector) & data$at != 0,]

Mean <- numeric(11)
Minimum <- numeric(11)
Q1 <- numeric(11)
Median <- numeric(11)
Q3 <- numeric(11)
Maximum <- numeric(11)
StandardDeviation <- numeric(11)

i <- 1
for(val in seq(10,60,5)) {
  Mean[i] <- mean(df$cfo_assetsratio[df$gsector == val], na.rm =TRUE) 
  Minimum[i] <- min(df$cfo_assetsratio[df$gsector == val], na.rm =TRUE) 
  Q1[i] <- quantile(df$cfo_assetsratio[df$gsector == val], .25,na.rm = TRUE) 
  Median[i] <- median(df$cfo_assetsratio[df$gsector == val], na.rm= TRUE) 
  Q3[i] <- quantile(df$cfo_assetsratio[df$gsector == val], .75,na.rm = TRUE) 
  Maximum[i] <- max(df$cfo_assetsratio[df$gsector == val], na.rm =TRUE) 
  StandardDeviation[i] <- sd(df$cfo_assetsratio[df$gsector == val], na.rm =TRUE)  
  i <- i + 1}

sectors <- data.frame(Sector = c("Energy","Materials", "Industrials", "Consumer Discretionary", "ConsumerStaples", "Health Care", "Financials", "Information Technology","Telecommunications Services", "Utilities", "Real Estate"),Mean, Minimum, Q1, Median, Q3, Maximum,StandardDeviation)
write.csv(sectors,"cfo_assetsratio.csv")

df <- data[!is.na(data$cfo_assetsratio) &data$cfo_assetsratio <= quantile(origin_data$cfo_assetsratio, .98,na.rm=TRUE) & data$cfo_assetsratio >quantile(origin_data$cfo_assetsratio, .05, na.rm=TRUE),]

ggplot(data = df, aes(x = cfo_assetsratio)) + 
  geom_histogram(aes(x = cfo_assetsratio, y = ..density..), bins= 100, col = "black", fill = "lightblue") +
  geom_density(aes(x = cfo_assetsratio), col = "darkblue") + 
  xlab("CFO/Assets Ratio") +  ylab("Density") +  
  labs(title = "CFO/Assets Ratio Distribution from 1970 to 2019") +  
  theme(axis.title.x = element_text(size=12), axis.title.y =element_text(size=12)) +
  theme_bw()

ggplot(data = df, aes(x = cfo_assetsratio, fill = Decade)) + 
  geom_density(aes(x = cfo_assetsratio), alpha = .25) +
  xlab("CFO/Assets Ratio") +  ylab("Density") +  
  labs(title = "CFO/Assets Ratio Distribution from 1970 to 2019 (By Decade)") + 
  theme(axis.title.x = element_text(size=12), axis.title.y =element_text(size=12)) +
  theme_bw()

ggplot(data = df, aes(x = fyear, y = cfo_assetsratio, group= fyear)) +  
  geom_boxplot() +  stat_summary(fun.y=mean, colour="blue", geom="point", shape=7,size=2) + 
  ylim(0, .25) +  xlab("Year") +  ylab("CFO/Assets Ratio") +  labs(title = "Boxplots of CFO/Assets Ratios from 1970 (By Year)") + 
  theme(axis.title.x = element_text(size=12), axis.title.y =element_text(size=12)) +
  theme_bw()

df <- data[!is.na(data$cfo_assetsratio) &!is.na(data$Sector) & data$cfo_assetsratio <=quantile(origin_data$cfo_assetsratio, .98, na.rm=TRUE) &data$cfo_assetsratio > quantile(origin_data$cfo_assetsratio, .05,na.rm=TRUE),]

ggplot(data = df, aes(x = cfo_assetsratio, fill = Sector)) +
  geom_histogram(aes(x = cfo_assetsratio), bins = 20) +
  facet_wrap(~ Sector) +  xlab("CFO/Assets Ratio") +  
  ylab("Density") +  
  labs(title = "CFO/Assets Ratio Histograms By Sector from 1970 to 2019") + 
  theme(axis.title.x = element_text(size=12), axis.title.y =element_text(size=12)) +
  theme_bw()


ggplot(data = df, aes(x = cfo_assetsratio, color = Sector))+
  geom_density() +  xlab("CFO/Assets Ratio") +  ylab("Density") +
  labs(title = "CFO/Assets Ratio Distributions By Sector from 1970 to 2019") + 
  theme(axis.title.x = element_text(size=12), axis.title.y =element_text(size=12)) +
  theme_bw()






