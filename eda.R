library(ggplot2)
# Assignment 1

# Read the data from csv file into dataFrame.
myData <- read.csv("censusData.csv")

# Column names and length of each column.
print (summary(myData))
c <- (colnames(myData))

for (val in c)
{
  print (length(myData[[val]]))
  print (c(val, class(myData[[val]])))
}

# Question 1

"age: type = Integer, Ratio-scaled Attribute, This is a ratio scaled attribute with an inherit zero point and we can talk one person's age as multiple of others."
"work: type = factor, Nominal Attribute, This is nominal because it is names of work they do it relating to names."
"edu: type = factor, Ordinal Attribute, This is regarding the education level which has order but the magnitude between the levels is unkown."
"marital: type = factor, Nominal Attribute, This is nominal because it is relating to names of their marital status."
"occupation: type = factor, Nominal Attribute, This is nominal because it is names of work they do it relating to names."
"race: type = factor, Nominal Attribute, This is nominal because it is relating to names of their races."
"sex: type = factor, Binary(symmetric) Attribute, Binary attributes are only with two categories, here male and female have equal weights."
"hrs_per_week: type = integer, Ratio-scaled Attribute,"
"income: type = factor, Ratio-scaled Attribute, Ratio-scaled Attribute, This is a ratio scaled attribute with an inherit zero point and we can talk one person's salary as multiple of others."

# Question 2.a
pct_missing_values <- c()
missing_values <- c()

for (val in c)
{
  each <- table(myData[val])
  if (!is.na(each['?'])) {
    pct <- (each['?']/length(myData[[val]])) * 100
    missing_values <- c(missing_values, each['?'])
    pct_missing_values <- c(pct_missing_values, pct)
  } else {
    pct_missing_values <- c(pct_missing_values, 0)
    missing_values <- c(missing_values, 0)
  }
}

# Question 2.b
missing_data <- c()
for (i in 1:length(myData[['sex']])){
  if ((myData[i,2] == '?') & (myData[i,5] == '?')){
    missing_data <- c(missing_data, 2)
  }else if ((myData[i,2] != '?') & (myData[i,5] == '?')) {
    missing_data <- c(missing_data, 1)
  }else if ((myData[i,2] == '?') & (myData[i,5] != '?')) {
    missing_data <- c(missing_data, 1)
  }else {
      missing_data <- c(missing_data, 0)
  }  
}
qplot(final, geom="histogram")


# Question 3 .a
# for age
# qplot(myData$age, geom='histogram')
# for number of hrs
# qplot(myData$hrs_per_week, geom='histogram')
# for income
# qplot(myData$income, geom='histogram')

# Question 3.b
#ggplot() + aes(myData$age)+ geom_histogram(binwidth=1, colour="black", fill="white")

# Question 5
ggplot(myData, aes(x=age, y=income)) +geom_point() + geom_jitter(position = position_jitter(width = 5))