rm(list=ls())  #  this removes old files that were previously loaded
setwd("~/Desktop/dfsf")  #  all files must be in the 
#  designated folder

# read in the myfunctions.R file
source("myfunctions-1.R")
library(tidyverse)
#
# Read in data.  Most datafiles will have a header row,
# which are column headings
data <- read.csv("~/Desktop/REX-Gotwals/archive/yield_df.csv", header=T)
head(data)
names(data)


India <- filter(data, Area == "India")
Brazil <- filter(data, Area == "Brazil")
Kenya <- filter(data, Area == "Kenya")
Netherlands <- filter(data, Area == "Netherlands")

hist(data$Year)
hist(data$avg_temp)
hist(data$average_rain_fall_mm_per_year)

# Function to add correlation coefficients
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  Cor <- abs(cor(x, y)) # Remove abs function if desired
  txt <- paste0(prefix, format(c(Cor, 0.123456789), digits = digits)[1])
  if(missing(cex.cor)) {
    cex.cor <- 0.4 / strwidth(txt)
  }
  text(0.5, 0.5, txt,
       cex = 1 + cex.cor * Cor) # Resize the text by level of correlation
}

# Plotting the correlation matrix
pairs(~ India$Year + India$hg.ha_yield + India$pesticides_tonnes + India$avg_temp, 
      data = India,
      upper.panel = panel.cor,    # Correlation panel
      lower.panel = panel.smooth)

simpleFit <- lm(India$avg_temp~India$Year)
plot(India$avg_temp~India$Year, main="Plot of Year vs. Temperature", xlab = "Year", ylab="Temperature")
abline(simpleFit)
# Fair to assume that temperatures have been rising.

simpleFit <- lm(India$pesticides_tonnes~India$Year)
plot(India$pesticides_tonnes~India$Year, main="Plot of Year vs. Pesticide", xlab = "Year", ylab="Pesticide in Tonnes")
abline(simpleFit)
# Clear decrease

#IndiaMaize
IndiaMaize <- filter(India, Item == "Maize")

simpleFit <- lm(IndiaMaize$hg.ha_yield~IndiaMaize$Year)
plot(IndiaMaize$hg.ha_yield~IndiaMaize$Year, main="Plot of Year vs. Temperature", xlab = "Year", ylab="Temperature")
abline(simpleFit)

simpleFit <- lm(IndiaMaize$hg.ha_yield~IndiaMaize$avg_temp)
plot(IndiaMaize$hg.ha_yield~IndiaMaize$avg_temp, main="Plot of Year vs. Temperature", xlab = "Year", ylab="Temperature")
abline(simpleFit)

multiFit <- lm(IndiaMaize$hg.ha_yield~IndiaMaize$avg_temp + IndiaMaize$Year)
summary(multiFit)

BIC(lm(IndiaMaize$hg.ha_yield~1))
BIC(lm(IndiaMaize$hg.ha_yield~IndiaMaize$avg_temp))
BIC(lm(IndiaMaize$hg.ha_yield~IndiaMaize$pesticides_tonnes))
BIC(lm(IndiaMaize$hg.ha_yield~IndiaMaize$Year))
BIC(lm(IndiaMaize$hg.ha_yield~IndiaMaize$avg_temp + IndiaMaize$pesticides_tonnes))

#IndiaWheat
IndiaWheat <- filter(India, Item == "Wheat")

simpleFit <- lm(IndiaWheat$hg.ha_yield~IndiaWheat$Year)
plot(IndiaWheat$hg.ha_yield~IndiaWheat$Year, main="Plot of Year vs. Temperature", xlab = "Year", ylab="Temperature")
abline(simpleFit)

simpleFit <- lm(IndiaWheat$hg.ha_yield~IndiaWheat$avg_temp)
plot(IndiaWheat$hg.ha_yield~IndiaWheat$avg_temp, main="Plot of Year vs. Temperature", xlab = "Year", ylab="Temperature")
abline(simpleFit)

multiFit <- lm(IndiaWheat$hg.ha_yield~IndiaWheat$avg_temp + IndiaWheat$Year)
summary(multiFit)

BIC(lm(IndiaWheat$hg.ha_yield~1))
BIC(lm(IndiaWheat$hg.ha_yield~IndiaWheat$avg_temp))
BIC(lm(IndiaWheat$hg.ha_yield~IndiaWheat$pesticides_tonnes))
BIC(lm(IndiaWheat$hg.ha_yield~IndiaWheat$Year))
BIC(lm(IndiaWheat$hg.ha_yield~IndiaWheat$avg_temp + IndiaWheat$pesticides_tonnes))



#BRAZIL

pairs(~ Brazil$Year + Brazil$hg.ha_yield + Brazil$pesticides_tonnes + Brazil$avg_temp,
      data = Brazil,
      upper.panel = panel.cor,    # Correlation panel
      lower.panel = panel.smooth)

simpleFit <- lm(Brazil$avg_temp~Brazil$Year)
plot(Brazil$avg_temp~Brazil$Year, main="Plot of Year vs. Temperature", xlab = "Year", ylab="Temperature")
abline(simpleFit)
# Fair to assume that temperatures have been rising.

simpleFit <- lm(Brazil$pesticides_tonnes~Brazil$Year)
plot(Brazil$pesticides_tonnes~Brazil$Year, main="Plot of Year vs. Temperature", xlab = "Year", ylab="Temperature")
abline(simpleFit)
# Clear decrease

#BrazilMaize
BrazilMaize <- filter(Brazil, Item == "Maize")

simpleFit <- lm(BrazilMaize$hg.ha_yield~BrazilMaize$Year)
plot(BrazilMaize$hg.ha_yield~BrazilMaize$Year, main="Plot of Year vs. Temperature", xlab = "Year", ylab="Temperature")
abline(simpleFit)

simpleFit <- lm(BrazilMaize$hg.ha_yield~BrazilMaize$avg_temp)
plot(BrazilMaize$hg.ha_yield~BrazilMaize$avg_temp, main="Plot of Year vs. Temperature", xlab = "Year", ylab="Temperature")
abline(simpleFit)

multiFit <- lm(BrazilMaize$hg.ha_yield~BrazilMaize$avg_temp + BrazilMaize$Year)
summary(multiFit)

BIC(lm(BrazilMaize$hg.ha_yield~1))
BIC(lm(BrazilMaize$hg.ha_yield~BrazilMaize$avg_temp))
BIC(lm(BrazilMaize$hg.ha_yield~BrazilMaize$pesticides_tonnes))
BIC(lm(BrazilMaize$hg.ha_yield~BrazilMaize$Year))
BIC(lm(BrazilMaize$hg.ha_yield~BrazilMaize$avg_temp + BrazilMaize$pesticides_tonnes))

#BrazilWheat
BrazilWheat <- filter(Brazil, Item == "Wheat")

simpleFit <- lm(BrazilWheat$hg.ha_yield~BrazilWheat$Year)
plot(BrazilWheat$hg.ha_yield~BrazilWheat$Year, main="Plot of Year vs. Temperature", xlab = "Year", ylab="Temperature")
abline(simpleFit)
summary(simpleFit)

simpleFit <- lm(BrazilWheat$hg.ha_yield~BrazilWheat$avg_temp)
plot(BrazilWheat$hg.ha_yield~BrazilWheat$avg_temp, main="Plot of Year vs. Temperature", xlab = "Year", ylab="Temperature")
abline(simpleFit)

multiFit <- lm(BrazilWheat$hg.ha_yield~BrazilWheat$pesticides_tonnes)
summary(multiFit)

BIC(lm(BrazilWheat$hg.ha_yield~1))
BIC(lm(BrazilWheat$hg.ha_yield~BrazilWheat$avg_temp))
BIC(lm(BrazilWheat$hg.ha_yield~BrazilWheat$pesticides_tonnes))
BIC(lm(BrazilWheat$hg.ha_yield~BrazilWheat$Year))
BIC(lm(BrazilWheat$hg.ha_yield~BrazilWheat$avg_temp + BrazilWheat$pesticides_tonnes))



#KENYA

pairs(~ Kenya$Year + Kenya$hg.ha_yield + Kenya$pesticides_tonnes + Kenya$avg_temp,
      data = Kenya,
      upper.panel = panel.cor,    # Correlation panel
      lower.panel = panel.smooth)

simpleFit <- lm(Kenya$avg_temp~Kenya$Year)
plot(Kenya$avg_temp~Kenya$Year, main="Plot of Year vs. Temperature", xlab = "Year", ylab="Temperature")
abline(simpleFit)
# Fair to assume that temperatures have been rising.

simpleFit <- lm(Kenya$pesticides_tonnes~Kenya$Year)
plot(Kenya$pesticides_tonnes~Kenya$Year, main="Plot of Year vs. Temperature", xlab = "Year", ylab="Temperature")
abline(simpleFit)
# Clear decrease

#KenyaMaize
KenyaMaize <- filter(Kenya, Item == "Maize")

simpleFit <- lm(KenyaMaize$hg.ha_yield~KenyaMaize$Year)
plot(KenyaMaize$hg.ha_yield~KenyaMaize$Year, main="Plot of Year vs. Temperature", xlab = "Year", ylab="Temperature")
abline(simpleFit)

simpleFit <- lm(KenyaMaize$hg.ha_yield~KenyaMaize$avg_temp)
plot(KenyaMaize$hg.ha_yield~KenyaMaize$avg_temp, main="Plot of Year vs. Temperature", xlab = "Year", ylab="Temperature")
abline(simpleFit)

multiFit <- lm(KenyaMaize$hg.ha_yield~KenyaMaize$avg_temp + KenyaMaize$Year)
summary(multiFit)

BIC(lm(KenyaMaize$hg.ha_yield~1))
BIC(lm(KenyaMaize$hg.ha_yield~KenyaMaize$avg_temp))
BIC(lm(KenyaMaize$hg.ha_yield~KenyaMaize$pesticides_tonnes))
BIC(lm(KenyaMaize$hg.ha_yield~KenyaMaize$Year))
BIC(lm(KenyaMaize$hg.ha_yield~KenyaMaize$avg_temp + KenyaMaize$pesticides_tonnes))

#KenyaWheat
KenyaWheat <- filter(Kenya, Item == "Wheat")

simpleFit <- lm(KenyaWheat$hg.ha_yield~KenyaWheat$Year)
plot(KenyaWheat$hg.ha_yield~KenyaWheat$Year, main="Plot of Year vs. Temperature", xlab = "Year", ylab="Temperature")
abline(simpleFit)
summary(simpleFit)

simpleFit <- lm(KenyaWheat$hg.ha_yield~KenyaWheat$avg_temp)
plot(KenyaWheat$hg.ha_yield~KenyaWheat$avg_temp, main="Plot of Year vs. Temperature", xlab = "Year", ylab="Temperature")
abline(simpleFit)

multiFit <- lm(KenyaWheat$hg.ha_yield~KenyaWheat$pesticides_tonnes)
summary(multiFit)

BIC(lm(KenyaWheat$hg.ha_yield~1))
BIC(lm(KenyaWheat$hg.ha_yield~KenyaWheat$avg_temp))
BIC(lm(KenyaWheat$hg.ha_yield~KenyaWheat$pesticides_tonnes))
BIC(lm(KenyaWheat$hg.ha_yield~KenyaWheat$Year))
BIC(lm(KenyaWheat$hg.ha_yield~KenyaWheat$avg_temp + KenyaWheat$pesticides_tonnes))



#NETHERLANDS
pairs(~ Netherlands$Year + Netherlands$hg.ha_yield + Netherlands$pesticides_tonnes + Netherlands$avg_temp,
      data = Netherlands,
      upper.panel = panel.cor,    # Correlation panel
      lower.panel = panel.smooth)

simpleFit <- lm(Netherlands$avg_temp~Netherlands$Year)
plot(Netherlands$avg_temp~Netherlands$Year, main="Plot of Year vs. Temperature", xlab = "Year", ylab="Temperature")
abline(simpleFit)
# Fair to assume that temperatures have been rising.

simpleFit <- lm(Netherlands$pesticides_tonnes~Netherlands$Year)
plot(Netherlands$pesticides_tonnes~Netherlands$Year, main="Plot of Year vs. Temperature", xlab = "Year", ylab="Temperature")
abline(simpleFit)
# Clear decrease

#NetherlandsMaize
NetherlandsMaize <- filter(Netherlands, Item == "Maize")

simpleFit <- lm(NetherlandsMaize$hg.ha_yield~NetherlandsMaize$Year)
plot(NetherlandsMaize$hg.ha_yield~NetherlandsMaize$Year, main="Plot of Year vs. Temperature", xlab = "Year", ylab="Temperature")
abline(simpleFit)

simpleFit <- lm(NetherlandsMaize$hg.ha_yield~NetherlandsMaize$avg_temp)
plot(NetherlandsMaize$hg.ha_yield~NetherlandsMaize$avg_temp, main="Plot of Year vs. Temperature", xlab = "Year", ylab="Temperature")
abline(simpleFit)

multiFit <- lm(NetherlandsMaize$hg.ha_yield~NetherlandsMaize$avg_temp + NetherlandsMaize$Year)
summary(multiFit)

BIC(lm(NetherlandsMaize$hg.ha_yield~1))
BIC(lm(NetherlandsMaize$hg.ha_yield~NetherlandsMaize$avg_temp))
BIC(lm(NetherlandsMaize$hg.ha_yield~NetherlandsMaize$pesticides_tonnes))
BIC(lm(NetherlandsMaize$hg.ha_yield~NetherlandsMaize$Year))
BIC(lm(NetherlandsMaize$hg.ha_yield~NetherlandsMaize$avg_temp + NetherlandsMaize$pesticides_tonnes))

#NetherlandsWheat
NetherlandsWheat <- filter(Netherlands, Item == "Wheat")

simpleFit <- lm(NetherlandsWheat$hg.ha_yield~NetherlandsWheat$Year)
plot(NetherlandsWheat$hg.ha_yield~NetherlandsWheat$Year, main="Plot of Year vs. Temperature", xlab = "Year", ylab="Temperature")
abline(simpleFit)
summary(simpleFit)

simpleFit <- lm(NetherlandsWheat$hg.ha_yield~NetherlandsWheat$avg_temp)
plot(NetherlandsWheat$hg.ha_yield~NetherlandsWheat$avg_temp, main="Plot of Year vs. Temperature", xlab = "Year", ylab="Temperature")
abline(simpleFit)

multiFit <- lm(NetherlandsWheat$hg.ha_yield~NetherlandsWheat$pesticides_tonnes)
summary(multiFit)

BIC(lm(NetherlandsWheat$hg.ha_yield~1))
BIC(lm(NetherlandsWheat$hg.ha_yield~NetherlandsWheat$avg_temp))
BIC(lm(NetherlandsWheat$hg.ha_yield~NetherlandsWheat$pesticides_tonnes))
BIC(lm(NetherlandsWheat$hg.ha_yield~NetherlandsWheat$Year))
BIC(lm(NetherlandsWheat$hg.ha_yield~NetherlandsWheat$avg_temp + NetherlandsWheat$pesticides_tonnes))

NetherlandsMaize$Area <- NULL
NetherlandsMaize$Item <- NULL
NetherlandsMaize$average_rain_fall_mm_per_year <- NULL
data$Area <- NULL
data$Item <- NULL
data$average_rain_fall_mm_per_year <- NULL
pca <- prcomp(data, scale=TRUE)
summary(pca)

pca.variance <- pca$sdev^2
pca.variance.per <- round(pca.variance / sum(pca.variance)*100, 1)
barplot(pca.variance.per, main="Scree Plot of PCs vs Variance", xlab="Principal Components", ylab="Variance", ylim=c(0, 100))

plot(pca$x[,1], pca$x[,2], xlab="PC1", ylab="PC2", main="PC1 vs. PC2")

pca.data <- data.frame(Sample=rownames(pca$x), X=pca$x[,1], Y=pca$x[,2])
ggplot(data=pca.data, aes(x=X, y=Y, label=Sample)) + geom_text() + xlab(paste("PC1 - ", pca.variance.per[1], "%", sep="")) + ylab(paste("PC2 - ", pca.variance.per[2], "%", sep="")) + theme_bw() + ggtitle("PC1 vs. PC2")


loading_scores <- pca$rotation[,1]
drug_scores <- abs(loading_scores)
drug_scores_ranked <- sort(drug_scores, decreasing=T)
names(drug_scores_ranked)

