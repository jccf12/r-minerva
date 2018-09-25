#loading data
mydataset <- read.csv(file="/Users/juan/Downloads/JCdatasetforCS112\ -\ TA.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)

#examine type of each column
str(mydataset)

#fixing type for columns with dates (originally num)
mydataset[, 4] <- as.Date(mydataset[, 4] - 25569, origin = as.Date('1970-01-01'))
mydataset[, 5] <- as.Date(mydataset[, 5] - 25569, origin = as.Date('1970-01-01'))
mydataset[, 6] <- as.Date(mydataset[, 6] - 25569, origin = as.Date('1970-01-01'))
mydataset[, 7] <- as.Date(mydataset[, 7] - 25569, origin = as.Date('1970-01-01'))

#fixing type for columns with numeric values (originally chr)
mydataset$cumulative.disbursements <- as.numeric(mydataset$cumulative.disbursements)
mydataset$undisbursed.amount <- as.numeric(mydataset$undisbursed.amount)

#exclude projects that are either approved before 1995-01-01 *OR* approved after 2017-01-01
to_be_excluded <- which(mydataset$approval.date < as.Date("1995-01-01") | mydataset$approval.date > as.Date("2017-01-01"))
mydataset1 <- mydataset[-to_be_excluded, ] #value stored in a different data.frame

#Algorithm to determine my questions:
#Surnmae first three letters: 'CAS', digit for each letter modulo 10: 1, 3, 9
set.seed(139) #3-digit seed
my_questions <- sort(sample(c(1:10), 3, replace = FALSE))
my_questions
#My questions are 3, 4, and 6




#Question 3: Does this data set contain missing data?
#missing data is represented as NA, thus I calculated the number of NAs in the data frame
missing <- sum(is.na(mydataset1))
missing

#missing turns out to be 3673, because it is not 0
#Answer: yes



#Question 4: Approximately how many projects are approved each year?

#create vector that contains boundary dates for each year
dates <- c("1995-01-01", "1996-01-01", "1997-01-01", "1998-01-01", "1999-01-01", "2000-01-01", "2001-01-01", 
                   "2002-01-01", "2003-01-01", "2004-01-01", "2005-01-01", "2006-01-01", "2007-01-01", "2008-01-01", 
                   "2009-01-01", "2010-01-01", "2011-01-01", "2012-01-01", "2013-01-01", "2014-01-01", "2015-01-01", 
                   "2016-01-01", "2017-01-01")

#vector that will store number of projects per year
projectsByYear = c()

#for loop that determines the number of projects per year and adds each value to the projectsByYear vector
for (i in 1:length(dates)-1) {
  projectsByYear[i] = sum(mydataset1$approval.date >= as.Date(dates[i]) & mydataset1$approval.date < as.Date(dates[i+1]))
}

#naming vector with the year and printing vector to see number of approved projects by year
names(projectsByYear) = c(1995:2016)
projectsByYear

#print average number of approved projects each year
sum(projectsByYear)/length(projectsByYear)

#Answer: 235.6 approved projects by year



#Question 6: What fraction of projects have completion dates that are different from revised completion dates?

#vector that evaluates when the original completion date is not equal to revised completion date
question6vector <- mydataset1$original.completion.date != mydataset1$revised.completion.date

#sums TRUE values after removing the NA's
different <- sum(question6vector, na.rm = TRUE)

#fraction of projects for which original is different from revised completion date
different/dim(mydataset1)[1]

#Answer: 0.768

