library(foreign)
data <- read.dta('/Users/juan/Downloads/jtpa.dta')

head(data)

assigned <- data[which(data$assignmt == 1),]
non_assigned <- data[-which(data$assignmt == 1),]

treated_compliers <- data[which(data$training==1 & data$assignmt==1),]
non_treated_compliers <- data[which(data$training==0 & data$assignmt==0),]

treated_non_compliers <- data[which(data$training==1 & data$assignmt==0),]
non_treated_non_compliers <- data[which(data$training==0 & data$assignmt==1),]

jtpa <- read.dta('/Users/juan/Downloads/jtpa.dta')

names(jtpa)

encouraged <- which(jtpa$assignmt == 1)
not_encouraged <- which(jtpa$assignmt == 0)

round(table(jtpa$training[encouraged])/length(encouraged), 2)
round(table(jtpa$training[not_encouraged])/length(not_encouraged), 2)


