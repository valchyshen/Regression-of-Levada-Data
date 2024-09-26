#
#      Oleksandr Valchyshen
#      avpfc@mail.umkc.edu, ovalchyshen@bard.edu
#
#      Started: Sep 25, 2024
#
#      Theme: Public Approval Indexes via Levada Monthly Opinion Polls
#
#-------------------------------------------------------------------------------+

source("C:/R/libraries.R")
library(dplyr)
library(lubridate)

path_plots <- "C:/UMKC/My Working Papers/Dissertation/Plots/"
name_png   <- "war_russians.png"
name_pdf1  <- "war_russians1.pdf"
name_pdf2  <- "war_russians2.pdf"
name_pdf4  <- "war_russians4.pdf"
name_pdf5  <- "war_russians5.pdf"

hd       <- "C:/UMKC/My Working Papers/Dissertation/Data/"
lvd.csv  <- "Levada.csv"  # https://www.levada.ru/en/ratings/approval-of-the-authorities/
lvd2.csv <- "Levada2.csv" # https://www.levada.ru/en/ratings/assessment-of-situation-in-the-country/

# Levada.ru -- Putin Approval
df <- read.csv(paste0(hd,lvd.csv), header = TRUE, sep = ",")
colnames(df) <- c("date","approve","disapprove","noanswer","approve_net")
lvd.ts.ptn <- ts(df[,c(2:5)], start=c(year(min(df$date)),month(min(df$date))), frequency = 12)
d1 <- max(df$date)

# Levada.ru -- Approval of General Situation/Conditions in Russia
df <- read.csv(paste0(hd,lvd2.csv), header = TRUE, sep = ",")
colnames(df) <- c("date","dir_right","dir_wrong","noanswer","dir_net")
df <- na.omit(df)
lvd.ts.ges <- ts(df[,c(2:5)], start=c(year(min(df$date)),month(min(df$date))), frequency = 12)
d2 <- max(df$date)

# War dummy table
df <- read.csv(paste0(hd,"Levada - War Dummy.csv"), header = TRUE, sep = ",")
df$period <- as.Date(df$period, "%m/%d/%Y")
df[is.na(df$war_dummy1)==TRUE,]$war_dummy1 <- 0
df[is.na(df$war_dummy2)==TRUE,]$war_dummy2 <- 0
df$area_sq_km <- as.numeric(df$area_sq_km)
df[is.na(df$area_sq_km)==TRUE,]$area_sq_km <- 0

ts.war_dummy1 <- ts(df$war_dummy1, start=c(year(df$period[1]),month(df$period[1])), frequency=12)
ts.war_dummy2 <- ts(df$war_dummy2, start=c(year(df$period[1]),month(df$period[1])), frequency=12)
ts.war_area   <- ts(df$area_sq_km, start=c(year(df$period[1]),month(df$period[1])), frequency=12)

plot(lvd.ts.ptn) # Levada's monthly survey of putin approval/disapproval
plot(lvd.ts.ges) # Levada's monthly survey of approval/disapproval of general situation/conditions

# First difference as time series of Levana's survey of putin approval/disapproval
fd.ts.ptn <- lvd.ts.ptn-stats::lag(lvd.ts.ptn,-1)
# First difference as time series of Levana's survey of general situation/conditions
fd.ts.ges <- lvd.ts.ges-stats::lag(lvd.ts.ges,-1)

colnames(lvd.ts.ges) <- c("approve","disapprove","indecisive","net")
colnames(lvd.ts.ptn) <- c("approve","disapprove","indecisive","net")
colnames(fd.ts.ges)  <- c("approve","disapprove","indecisive","net")
colnames(fd.ts.ptn)  <- c("approve","disapprove","indecisive","net")

# Plotting the general idea about the data. model & hypothesis
pdf(file=paste0(path_plots,name_pdf5))
par(mfrow=c(3,1), mar=c(2, 2, 2, 2), omi=c(0.2, 0, 0, 0))
plot(lvd.ts.ges[,1], xlab="", ylab="", main="Reported Approval of General Conditions and Putin",
     ylim=c(min(lvd.ts.ges[,1],lvd.ts.ptn[,1]),max(lvd.ts.ges[,1],lvd.ts.ptn[,1])))
lines(lvd.ts.ptn[,1], col="red")
legend("bottomright", 
       c("General Conditions (GC)","Putin Approval (PA)","Launch of new war, land grab"),
       col=c("black","red","red"),
       lty=c(1,1,3), lwd=c(1,1,1), cex=1)
abline(v= time(war_dummy1)[war_dummy1!=0], col="red", lwd=1, lty=3)
plot(lvd.ts.ges[,2], xlab="", ylab="", main="Reported Disapproval of General Conditions and Putin",
     ylim=c(min(lvd.ts.ges[,1],lvd.ts.ptn[,1]),max(lvd.ts.ges[,2],lvd.ts.ptn[,2])))
lines(lvd.ts.ptn[,2], col="red")
abline(v= time(war_dummy1)[war_dummy1!=0], col="red", lwd=1, lty=3)
plot(lvd.ts.ges[,4], xlab="", ylab="", main="Net of Approval & Disapproval of General Conditions and Putin",
     ylim=c(min(lvd.ts.ges[,4],lvd.ts.ptn[,4]),max(lvd.ts.ges[,4],lvd.ts.ptn[,4])))
lines(lvd.ts.ptn[,4], col="red")
abline(v= time(war_dummy1)[war_dummy1!=0], col="red", lwd=1, lty=3)
par(); dev.off()

# Plotting reported time series and their first differences as boxplots
pdf(file=paste0(path_plots,name_pdf4))
par(mfrow=c(2,2), mar=c(2, 2, 2, 2), omi=c(0.2, 0, 0, 0))
boxplot(lvd.ts.ges, main="General Conditions (GC), Reporgted", cex.axis=.8);
boxplot(lvd.ts.ptn, main="Putin Approval (PA), Reporgted", cex.axis=.8);
boxplot(fd.ts.ges,  main="General Conditions (GC), 1st Difference", cex.axis=.8);
boxplot(fd.ts.ptn,  main="Putin Approval (PA), 1st Difference", cex.axis=.8);
par(); dev.off()

# Plotting two net indexes for stationarity: Gen Sit/Cond & Putin App 
pdf(file=paste0(path_plots,name_pdf1))
par(mfrow=c(2,4), mar=c(2, 2, 2, 2), omi=c(0.2, 0, 0, 0))
# Gen Sit/Cond 
plot.ts(lvd.ts.ges[,4], 
        main="Gen.Conditions (GC)",
        xlab=paste("Time: # of obs =",length(lvd.ts.ges[,4])), ylab="Net: Approve - Dissapprove")
acf(lvd.ts.ges[,4], main="ACF: GC")
plot.ts(fd.ts.ges[,4], main="1st Diff. of GC", xlab=paste("Time: # of obs =",length(fd.ts.ges[,4])), ylab="")
acf(fd.ts.ges[,4], main="ACF: 1st Diff. of GC")
# Putin App 
plot.ts(lvd.ts.ptn[,4], 
        main="Putin Approval (PA)",
        xlab=paste("Time: # of obs =",length(lvd.ts.ptn[,4])), ylab="Net: Approve - Dissapprove")
acf(lvd.ts.ptn[,4], main="ACF: PA")
plot.ts(fd.ts.ptn[,4], main="1st Diff. of PA", xlab=paste("Time: # of obs =",length(fd.ts.ptn[,4])), ylab="")
acf(fd.ts.ptn[,4], main="ACF: 1st Diff. of PA")
par(); dev.off()

# Plotting of Levada Surveys' Key Approval Indexes
pdf(file=paste0(path_plots,name_pdf2))
par(mfrow=c(1,3), mar=c(2, 2, 2, 2), omi=c(0.2, 0, 0, 0))
plot.ts(fd.ts.ges[,4], 
        main="General Conditions (GC)",
        xlab=paste("Time: Number of obs =",length(fd.ts.ges[,4])),
        ylab="First difference: Net")
plot.ts(fd.ts.ptn[,4], main="Putin Approval (PA)",
        xlab=paste("Time: Number of obs =",length(fd.ts.ptn[,4])),
        ylab="First difference: Net")
plot.ts(ts.war_dummy1, main="War Dummy #1",
        xlab=paste("Time: Number of obs =",length(ts.war_dummy1)),
        ylab="0 = Peace & Old Wars / 1 = New War")
par(); dev.off()

summary(fd.ts.ges)
summary(fd.ts.ptn)

# Align all time series to the Putin data
d <- as.Date(time(fd.ts.ptn))[1]
ges <- window(fd.ts.ges, start=c(year(d),month(d)))[,4]
war_dummy1 <- window(ts.war_dummy1, start=c(year(d),month(d)))
war_dummy2 <- window(ts.war_dummy2, start=c(year(d),month(d)))
war_area <- window(ts.war_area, start=c(year(d),month(d)))
ptn <- fd.ts.ptn[,4]
length(ges); length(ptn); length(war_dummy1); length(war_dummy2); length(war_area)

#m1 <- lm(ges ~ ptn)
#summary(m1)

m2 <- lm(ptn ~ war_dummy1)
summary(m2)

m3 <- lm(ptn ~ war_dummy1 + war_area)
summary(m3)

# Align all time series to the Gen Situation/Conditions data
d <- as.Date(time(fd.ts.ges))[1]
ges <- fd.ts.ges[,4]
war_dummy1 <- window(ts.war_dummy1, start=c(year(d),month(d)))
war_dummy2 <- window(ts.war_dummy2, start=c(year(d),month(d)))
war_area <- window(ts.war_area, start=c(year(d),month(d)))
length(ges); length(war_dummy1); length(war_dummy2); length(war_area)

m4 <- lm(ges ~ war_dummy1)
summary(m4)

m5 <- lm(ges ~ war_dummy1 + war_area)
summary(m5)

library(stargazer)
stargazer(m2,m3,m4,m5, type = "latex",
          title="Regression Results", single.row=FALSE,
          ci=FALSE, ci.level=0.9, omit.stat=c("f", "ser"))
