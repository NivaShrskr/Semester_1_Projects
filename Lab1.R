batting <- read.csv('Lab1_MLB_Batting_2021.csv')
batting
library(ggplot2)
library(RColorBrewer)

#Question 1
#The slugging percentage measures the power of a hitter. 
#Create a graph of the univariate distribution of slugging percentage and describe it with the appropriate statistics.
batting_ggplot <- ggplot(batting)
batting_ggplot + geom_histogram(aes(x=SluggingPer), col='dark grey',fill='pink')
fivenum(batting$SluggingPer)


#Question 2
#How many players in each position are there in this data set? 
#Remember, it includes only includes players that batted at least 100 times in the 2021 season
#Who is the only pitcher, and what team did he play for?
table(batting$Position)
pitcher = which(batting$Position=='Pitcher')
batting[pitcher, "Name"]
batting[pitcher, "Team"]

#Question 3
#Is slugging percentage related to the number of times a player struck-out? 
#Create the appropriate graph and describe the relationship with the appropriate statistic.
batting_ggplot + geom_point(aes(x=SluggingPer,y=StrikeOuts, color=StrikeOuts)) + labs(title = 'Slugging Percentage vs Strike Out Frequency', x="Slugging Percentage", y="Strike Out Frequency")
cor(batting$SluggingPer, batting$StrikeOuts)

#Question 4
#Is slugging percentage related to the number of times a player struck-out? 
#Create the appropriate graph and describe the relationship with the appropriate statistic.
#Code for creating new column
batting$Power_Hitter <- batting$HomeRuns >=20
#How many hitters are in data set
sum(batting$Power_Hitter)

#Question 5
#Create a graph that displays the relationship between strike-outs and slugging percentage split by the two “power hitter” groups.
#Describe the relationship between strike-outs and slugging for each group separately, using the appropriate statistics. 
batting_ggplot <- ggplot(batting)
batting_ggplot + geom_point(aes(x=SluggingPer,y=StrikeOuts, color=Power_Hitter)) + labs(title = 'Slugging Percentage vs Strike Out Frequency', x="Slugging Percentage", y="Strike Out Frequency")
pow = batting[batting$Power_Hitter==TRUE,]
cor(pow$SluggingPer,pow$StrikeOuts)
nopow = batting[batting$Power_Hitter==FALSE,]
cor(nopow$SluggingPer,nopow$StrikeOuts)
