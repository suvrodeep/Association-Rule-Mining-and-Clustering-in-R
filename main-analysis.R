#Include packages
#install.packages("arules")
#install.packages("ggplot2")
#install.packages("caret")
#install.packages("dummies")
#install.packages("stats")
#install.packages("e1071")
#install.packages("dplyr")
#install.packages("formattable")
#install.packages("arulesViz")
library(ggplot2)
library(caret)
library(dummies)
library(stats)
library(class)
library(e1071)
library(dplyr)
library(formattable)
library(arules)
library(arulesViz)
library(datasets)
#
#
#Setting file name
filename <- "data.csv"
#
#
#Reading csv into dataframe and create success class
df <- read.csv(filename, header = TRUE)
#
#
#Creating new variable
df$NPV <- as.vector(df$NPV)
df$AMOUNT_REQUESTED <- as.vector(df$AMOUNT_REQUESTED)
df$PROFITABLE <- ifelse((df$NPV > 0), 1, 0)
df$PROFITABLE <- as.factor(df$PROFITABLE)
#
#
#Creating dummy variables
dummy_cl_names <-
  c("CHK_ACCT", "SAV_ACCT", "HISTORY", "JOB", "TYPE")
drop.cols <- c("OBS.", "CREDIT_EXTENDED", "AGE")
df$CHK_ACCT <- as.factor(df$CHK_ACCT)
df$SAV_ACCT <- as.factor(df$SAV_ACCT)
df$HISTORY <- as.factor(df$HISTORY)
df$JOB <- as.factor(df$JOB)
df$TYPE <- as.factor(df$TYPE)
df.dum <-
  dummy.data.frame(data = select(df,-one_of(drop.cols)) , names = dummy_cl_names)
str(df.dum)
#
#
#K means clustering
set.seed(12345)
drop.cols <- c("NPV", "PROFITABLE")
#df.km <- data.frame(scale(select(df.dum, -one_of(drop.cols))))
df.km <- select(df.dum,-one_of(drop.cols))
#
#
#K means with K=5
km1 <- kmeans(df.km, 5, nstart = 20)
km1$centers
dist(km1$centers)
df.dum$CLUSTERS <- km1$cluster
table(df.dum$PROFITABLE, df.dum$CLUSTERS)
#
#
#Bar chart of percentage of persons in each cluster
grouped <- count(df.dum, CLUSTERS)
grouped$Percentage <- grouped$n / sum(grouped$n) * 100
ggplot(data = grouped,
       mapping = aes(x = grouped$CLUSTERS, y = grouped$Percentage)) + xlab("Cluster") + ylab("Percentage") +  geom_col()
grouped1 <-
  summarise(group_by(df.dum, CLUSTERS),
            count = n(),
            meanNPV = mean(NPV))
tab1 <- formattable(select(grouped1,-count))
tab1
#
#
#Repeat K means with K = 4
set.seed(12345)
km2 <- kmeans(df.km, 4, nstart = 20)
km2$centers
dist(km2$centers)
df.dum$CLUSTERS <- km2$cluster
table(df.dum$PROFITABLE, df.dum$CLUSTERS)
#
#
#Bar chart of percentage of persons in each cluster
grouped <- count(df.dum, CLUSTERS)
grouped$Percentage <- grouped$n / sum(grouped$n) * 100
ggplot(data = grouped,
       mapping = aes(x = grouped$CLUSTERS, y = grouped$Percentage)) + xlab("Cluster") + ylab("Percentage") +  geom_col()
grouped1 <-
  summarise(group_by(df.dum, CLUSTERS),
            count = n(),
            meanNPV = mean(NPV))
tab1 <- formattable(select(grouped1,-count))
tab1
#
#
#Repeat K means with K = 6
set.seed(12345)
km3 <- kmeans(df.km, 6, nstart = 20)
km3$centers
dist(km3$centers)
df.dum$CLUSTERS <- km3$cluster
table(df.dum$PROFITABLE, df.dum$CLUSTERS)
#
#
#Bar chart of percentage of persons in each cluster
grouped <- count(df.dum, CLUSTERS)
grouped$Percentage <- grouped$n / sum(grouped$n) * 100
ggplot(data = grouped,
       mapping = aes(x = grouped$CLUSTERS, y = grouped$Percentage)) + xlab("Cluster") + ylab("Percentage") +  geom_col()
grouped1 <-
  summarise(group_by(df.dum, CLUSTERS),
            count = n(),
            meanNPV = mean(NPV))
tab1 <- formattable(select(grouped1,-count))
tab1
#
#
#Repeat K means with K = 10
set.seed(12345)
km4 <- kmeans(df.km, 10, nstart = 20)
km4$centers
dist(km4$centers)
df.dum$CLUSTERS <- km4$cluster
table(df.dum$PROFITABLE, df.dum$CLUSTERS)
#
#
#Bar chart of percentage of persons in each cluster
grouped <- count(df.dum, CLUSTERS)
grouped$Percentage <- grouped$n / sum(grouped$n) * 100
ggplot(data = grouped,
       mapping = aes(x = grouped$CLUSTERS, y = grouped$Percentage)) + xlab("Cluster") + ylab("Percentage") +  geom_col()
grouped1 <-
  summarise(group_by(df.dum, CLUSTERS),
            count = n(),
            meanNPV = mean(NPV))
tab1 <- formattable(select(grouped1,-count))
tab1
#
#
#Association rule rule mining
select.cols <-
  c("CHK_ACCT",
    "SAV_ACCT",
    "HISTORY",
    "EMPLOYMENT",
    "OWN_RES",
    "JOB",
    "PROFITABLE")
df.ar <- select(df, one_of(select.cols))
df.ar$EMPLOYMENT <- as.factor(df.ar$EMPLOYMENT)
df.ar$OWN_RES <- as.factor(df.ar$OWN_RES)
rules <-
  apriori(
    df.ar,
    parameter = list(supp = 0.001, conf = 0.8),
    appearance = list(default = "lhs", rhs = "PROFITABLE=1"),
    control = list(verbose = F)
  )
plot(rules)
best.rules <-
  rules[quality(rules)$confidence > 0.9 &
          quality(rules)$support > 0.05]
plot(best.rules)
best.rules <- sort(best.rules, by = "confidence", decreasing = TRUE)
inspect(best.rules[1:5])
#
#
#Taking first rule CHK_ACCT=3,EMPLOYMENT=4,OWN_RES=1,JOB=2
total.profit <-
  sum(df$NPV[which(df$CHK_ACCT == 3 &
                     df$JOB == 2 & df$OWN_RES == 1 & df$EMPLOYMENT == 4)])
total.profit
#
#
#Combining clustering and association rule mining
#K means with K=5
km1 <- kmeans(df.km, 5, nstart = 20)
df.dum$CLUSTERS <- km1$cluster
select.cols <- c("CLUSTERS", "PROFITABLE")
df.ark <- select(df.dum, one_of(select.cols))
df.ark$CLUSTERS <- as.factor(df.ark$CLUSTERS)
rules <-
  apriori(
    df.ark,
    parameter = list(supp = 0.001, conf = 0.1),
    appearance = list(default = "lhs", rhs = "PROFITABLE=1"),
    control = list(verbose = F)
  )
plot(rules)
best.rules <- rules[quality(rules)$confidence > 0.5]
plot(best.rules)
best.rules <- sort(best.rules, by = "confidence", decreasing = TRUE)
inspect(best.rules)
#
#
#Selecting first rule CLUSTERS=3
total.profit <- sum(df.dum$NPV[which(df.dum$CLUSTERS == 3)])
total.profit
