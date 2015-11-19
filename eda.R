library(ggplot2)
library(corrplot)
# Assignment 1

# Read the data from csv file into dataFrame.
myData <- read.csv("censusData.csv")

# Column names and length of each column.
c <- (colnames(myData))

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

names(pct_missing_values) <- c

print (pct_missing_values)

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

hist_plot <- qplot(missing_data, geom="histogram", 
                   binwidth=0.1, 
                   main="Missing values per data point",
                   xlab="Missing values",
                   ylab="Count")

print (hist_plot)


# Question 3 .a

#for age
age_hist <- ggplot(myData, aes(x=age)) + geom_histogram(binwidth=5, 
                                            alpha=0.7, 
                                            fill="yellow", 
                                            col="red")+
                                            ggtitle("Histogram for Age") +
                                            xlab("Age")+
                                            ylab("Count")
print (age_hist)

# for number of hrs
hours_per_week_hist <- ggplot(myData, aes(x=hrs_per_week)) + geom_histogram(binwidth=8, 
                                            alpha=0.7, 
                                            fill="yellow", 
                                            col="red")+
                                            ggtitle("Histogram for hrs_worked_per_week") +
                                            xlab("Hours per week")+
                                            ylab("Count")
print (hours_per_week_hist)

# for income
income_hist = ggplot(myData, aes(x=income)) + geom_histogram(alpha=0.7, 
                                               fill="yellow", 
                                               col="red")+
                                               ggtitle("Histogram for Income") +
                                               xlab("Income")+
                                               ylab("Count")

print (income_hist)

# Question 3.b
age_income <- ggplot(myData, aes(x=age)) + geom_histogram(binwidth=5, 
                                            alpha=0.7, 
                                            fill="yellow", 
                                            col="red")+
                                            ggtitle("Histogram for Age with respect to Income") +
                                            xlab("Age")+
                                            ylab("Count")+ 
                                            facet_grid(income ~ .)
print (age_income)

hrs_per_week_income <- ggplot(myData, aes(x=hrs_per_week)) + geom_histogram(binwidth=10, 
                                            alpha=0.7, 
                                            fill="yellow", 
                                            col="red")+
                                            ggtitle("Histogram for Hours worked per week with respect to Income") +
                                            xlab("Hours worked per week")+
                                            ylab("Count")+ 
                                            facet_grid(income ~ .)
print (hrs_per_week_income)

# Question 3.c

# age and income
age_income_boxplot <- ggplot(data=myData, aes(x=income, y=age)) + geom_boxplot(alpha=0.5,
                                                         fill="yellow",
                                                         col="red")+
                                                         ggtitle("Box plots of Income and Age")
print (age_income_boxplot)

# hrs_per_week and income
hrs_per_week_income_boxplot <- ggplot(data=myData, aes(x=income, y=hrs_per_week)) + geom_boxplot(alpha=0.5,
                                                                  fill="yellow",
                                                                  col="red")+
                                                                  ggtitle("Box plots of Income and Hours worked per week")

print (hrs_per_week_income_boxplot)

# Question 4.a

# Plotting of each categorical feature
work_bar_plot <- ggplot(myData, aes(x=work)) + geom_bar(binwidth=10, 
                                            alpha=0.7, 
                                            fill="yellow", 
                                            col="red")+
                                            ggtitle("Bar plot of Work")+
                                            xlab("Work")+
                                            ylab("Count")

print (work_bar_plot)

education_bar <- ggplot(myData, aes(x=edu)) + geom_bar(binwidth=10, 
                                       alpha=0.7, 
                                       fill="yellow", 
                                       col="red")+
                                       ggtitle("Bar plot of Education")+
                                       xlab("Education")+
                                       ylab("Count")

print (education_bar)

marital_barplot <- ggplot(myData, aes(x=marital)) + geom_bar(binwidth=10, 
                                      alpha=0.7, 
                                      fill="yellow", 
                                      col="red")+
                                      ggtitle("Bar plot of Marital status")+
                                      xlab("Marital")+
                                      ylab("Count")

print (marital_barplot)

occupation_barplot <- ggplot(myData, aes(x=occupation)) + geom_bar(binwidth=10, 
                                          alpha=0.7, 
                                          fill="yellow", 
                                          col="red")+
                                          ggtitle("Bar plot of Occupation")+
                                          xlab("Occupation")+
                                          ylab("Count")


print (occupation_barplot)

race_barplot <- ggplot(myData, aes(x=race)) + geom_bar(binwidth=10, 
                                          alpha=0.7, 
                                          fill="yellow", 
                                          col="red")+
                                          ggtitle("Bar plot of Race")+
                                          xlab("Race")+
                                          ylab("Count")

print (race_barplot)

# Question 4.b
work_income <- ggplot(myData, aes(x=work)) + geom_bar(binwidth=10, 
                                       alpha=0.7, 
                                       fill="yellow", 
                                       col="red")+
                                       ggtitle("Bar plot of Work")+
                                       xlab("Work")+
                                       ylab("Count")  + facet_grid(income ~ .)

print (work_income)

edu_income <- ggplot(myData, aes(x=edu)) + geom_bar(binwidth=10, 
                                      alpha=0.7, 
                                      fill="yellow", 
                                      col="red")+
                                      ggtitle("Bar plot of Education")+
                                      xlab("Education")+
                                      ylab("Count")  + facet_grid(income ~ .)

print (edu_income)

marital_income <- ggplot(myData, aes(x=marital)) + geom_bar(binwidth=10, 
                                          alpha=0.7, 
                                          fill="yellow", 
                                          col="red")+
                                          ggtitle("Bar plot of Marital status")+
                                          xlab("Marital")+
                                          ylab("Count")  + facet_grid(income ~ .)

print (marital_income)

occupation_income <- ggplot(myData, aes(x=occupation)) + geom_bar(binwidth=10, 
                                             alpha=0.7, 
                                             fill="yellow", 
                                             col="red")+
                                             ggtitle("Bar plot of Occupation")+
                                             xlab("Occupation")+
                                             ylab("Count")  + facet_grid(income ~ .)

print (occupation_income)

race_income <- ggplot(myData, aes(x=race)) + geom_bar(binwidth=10, 
                                       alpha=0.7, 
                                       fill="yellow", 
                                       col="red")+
                                       ggtitle("Bar plot of Race")+
                                       xlab("Race")+
                                       ylab("Count") + facet_grid(income ~ .)

print (race_income)

# Question 5
age_hrs_per_week_scatter <- ggplot(myData, aes(x=age, y=hrs_per_week)) + geom_point(shape=19, 
                                                        color="blue", 
                                                        alpha=0.1)+
                                                        ggtitle("Age vs hrs_per_week")+
                                                        xlab("Age")+
                                                        ylab("Hours per week")+
                                                        geom_point(position = position_jitter(w = 0.01, h = 0.01))

print (age_hrs_per_week_scatter)
correlation <- cor(myData$age, myData$hrs_per_week)
print(correlation)

# Question 6

# Correlatio plot between age and hrs_per_week
correlation_matrix <- cor(data.frame(myData$age, myData$hrs_per_week))
corrplot(correlation_matrix)