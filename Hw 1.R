bonds <- read.csv('Homework1_Bonds.csv')
bonds

#Question 1
#How many of these bonds were approved by voters, and how many were defeated? 
bonds$Result=='Carried'
sum(bonds$Result=='Carried',na.rm=T)
bonds$Result == 'Defeated'
sum(bonds$Result=='Defeated',na.rm=T)

#Are there any differences in how likely a bond was to pass across the four different government types?  
#Calculate the appropriate descriptive statistics to answer this question.
table(bonds$Type) 
table(bonds$Type,bonds$Result)
prop.table(table(bonds$Type,bonds$Result), margin = 1) 

#Question 2
#Some of these bonds were on the ballot during presidential elections and therefore had very high voter turnout.
#Calculate a new variable in the dataframe called “VotesTotal” that is the sum of the votes “for” and “against” the bond measure.
bonds$VotesTotal <- bonds$VotesFor + bonds$VotesAgainst
#When and where did the bond measure with the highest voter turnout occur?  What was it for?
maxturnout <- bonds[which.max(bonds$VotesTotal), ]
maxturnout
fivenum(bonds$VotesTotal) 

#Question 3
#Let’s look at the margins by which the bonds that did pass were approved, ignoring those with very low voter turnout. 
#Create a subset of this data set that contains the bond measures that were approved and had at least 10 total votes. 
approvedbonds <- bonds[bonds$Result == 'Carried' & bonds$VotesTotal >= 10, ]
approvedbonds
#Next, create a variable that gives the percentage of total votes that were for the bond measure. 
approvedbonds$PercentVotesFor <- (approvedbonds$VotesFor / approvedbonds$VotesTotal *100)
approvedbonds$PercentVotesFor
#Finally, make a graph of the distribution of your new variable for your subset of bond measures 
boxplot(approvedbonds$PercentVotesFor, main = 'Percentage Distribution of Approved Voters', xlab= 'Percentage of Votes (%)', horizontal = TRUE ,col=c('pink'))
#and describe its distribution with the appropriate statistics
#(note that "statistics" is plural, this means at least 2 calculated numeric values). 
sd(approvedbonds$PercentVotesFor)
fivenum(approvedbonds$PercentVotesFor)

#Question 4
#Is the margin a bond was approved by related to its cost?
#Use your subset from #3 to create a graph to display this relationship and calculate the appropriate descriptive 
#statistic to help answer this question.
plot(approvedbonds$PercentVotesFor,approvedbonds$Amount,main='Approved Bonds and Cost',xlab='Percentage Approved Bonds',ylab='Cost',pch=25, col ='magenta')
cor(approvedbonds$PercentVotesFor,approvedbonds$Amount)
