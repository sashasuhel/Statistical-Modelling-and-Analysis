# F21SA Statistical Modelling and Analysis
# H00410394 Sasha Fathima Suhel

# R language Version
R.version.string

# Reading and storing csv
# Internet network data is stored in an csv file, x column contains file sizes
myData = read.csv("filesize.csv")[["x"]]

library(moments)
library(Pareto)

# Histogram to summarize data
png(filename = "Histogram.png",
    width = 480, height = 480, units = "px", pointsize = 12,
    bg = "white")
par(mar=c(4,4,1,1)+0.1)
hist(myData, main = "Histogram of Internet Network Data",
     xlab="Recent File Sizes",ylim = c(0,1000),xlim = c(1000,80000) ,breaks = 100)
suppress <- dev.off()
# Summary statistics
cat("Network Data\n")
cat("Mean:", mean(myData), "kB\n")
cat("Standard deviation:", sd(myData), "kB\n")
cat("Median:", median(myData), "kB\n")
cat("Quantiles:", quantile(myData), "(all kB)\n")
cat("Skewness:", skewness(myData) )

# MLE
alphahat = length(myData)/(sum(log(myData)) - length(myData)*(log(1000))) ; alphahat

# Estimated Standard Error
ese = alphahat / sqrt(length(myData)) ; ese
# Confidence Interval
z_025 <- qnorm(p = 0.025, lower.tail = FALSE) # From NCST Table 5
CI_L = alphahat - (z_025*ese)
CI_U = alphahat + (z_025*ese)
cat("Confidence Interval: [",CI_L,",",CI_U,"] ")

# Estimating the derivation for Y'
y_dash=numeric(0)
for(i in 1:5000){
  y_dash[i]=mean(Pareto::rPareto(length(myData),1000,alphahat))
}
# Plotting histogram for the sample means from Y'
png(filename = "Y_dash.png",
    width = 480, height = 480, units = "px", pointsize = 12,
    bg = "white")
par(mar=c(4,4,1,1)+0.1)
hist(y_dash, main = "Histogram of Y'",
     xlab="Recent File Sizes", breaks = 100)
suppress <- dev.off()

# Summary of the new data
cat("Network Data\n")
cat("Mean:", mean(y_dash), "kB\n")
cat("Standard deviation:", sd(y_dash), "kB\n")
cat("Median:", median(y_dash), "kB\n")
cat("Quantiles:", quantile(y_dash), "(all kB)\n")
cat("Skewness:", skewness(y_dash) )

# To obtain Maximum Possible Limit
m_p_l = qPareto(0.99,length(myData),alphahat) ;m_p_l



