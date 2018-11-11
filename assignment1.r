#Exercise 1 

# d. Country with the lowest literacy

literacy_min <- min(WHO$LiteracyRate, na.rm = TRUE)
row_lowest_literacy <- subset(WHO, LiteracyRate == literacy_min)
print(row_lowest_literacy$Country)

#The answer is Mali

# e. Richest Country in Europe based on GNI

europe <- subset(WHO, Region == 'Europe')
max_gni_europe <-max(europe$GNI, na.rm = TRUE)
row_with_max_gni_europe <- subset(europe,GNI == max_gni_europe)
print(row_with_max_gni_europe$Country)

#The answer is Luxembourg

# f. Mean Life expectancy of countries in Africa
WHO.Africa = subset(WHO, Region == "Africa")
mean(WHO.Africa$LifeExpectancy)


# g. Number of countries with population greater than 10,000
sum(WHO$Population >10000)

#The answer is 86

# h. Top 5 countries in the Americas with the highest child mortality

WHO.Americas = subset(WHO,Region == "Americas")
head(sort(WHO.Americas$ChildMortality, decreasing = TRUE))
Ordered_childmortality_americas <- order(WHO.Americas$ChildMortality, decreasing = TRUE)
americas_ordered_by_childmortality <- WHO.Americas[Ordered_childmortality_americas,]
top_5_childmortality_americas_head <- head(americas_ordered_by_childmortality,5)
top_5_childmortality_americas_indexing <- americas_ordered_by_childmortality[1:5,]
top_5_childmortality_americas_indexing$Country


# The answers are as follows:
#Haiti
#Bolivia
#Guyana
#Guatemala
#Dominican Republic

#Exercise 2
# NBA Dataset


# a.) The year the bulls had the highest winning percentage

library(readr)
Historical_NBA_Performance <- read_csv("Historical NBA Performance.csv")
View(Historical_NBA_Performance)

bulls <- subset(Historical_NBA_Performance, Team == 'Bulls')
bulls_highest_winpct <- max(bulls$`Winning Percentage`)
bulls_row_highest_winpct <- subset(bulls, `Winning Percentage` == bulls_highest_winpct)
bulls_row_highest_winpct$Year

# Answer is 1995-1996

# b.) Teams with an even win-loss record in a year

teams_500 <- subset(Historical_NBA_Performance, `Winning Percentage` == 0.5)
teams_500

# Answer: 53 entries of teams with .500 records

#Exercise 3

#Season_Stats.csv
library(readr)
Seasons_Stats <- read_csv("Seasons_Stats.csv")
View(Seasons_Stats)

# c.) What year/season does Lebron James score the highest?
LeBron <- subset(Seasons_Stats, Player == 'LeBron James')
LeBron_max_pts <- max(LeBron$PTS, na.rm = TRUE)
subset(LeBron, PTS == LeBron_max_pts)$Year

#answer is 2006

# d.) What year/season does Michael Jordan score the highest?
MJordan <- subset(Seasons_Stats, Player == 'Michael Jordan*')
subset(MJordan, PTS == max(MJordan$PTS))$Year

#answer is 1987

# e.) PLayer efficiency rating of Kobe Bryant in the year where his MP is the lowest?
Kobe <- subset(Seasons_Stats, Player =='Kobe Bryant')
subset(Kobe, MP == min(Kobe$MP))$PER

#answer is a PER of 10.7

# Exercise 4

# National Universities Rankings.csv
universities <-read.csv('National Universities Rankings.csv')

# a.) University with the most number of undergrads

print(universities[which.max(universities$Undergrad.Enrollment),]$Name)

replaceCommas <- function(x){
  +     x<-as.numeric(gsub("\\,", "", x))}

MostUndergrads <- replaceCommas(universities$Undergrad.Enrollment)
print(universities[which.max(MostUndergrads),]$Name)

#Answer is University of Central Florida

# b.) Average Tuition in the Top 10 University

top_10 <- universities[order(universities$Rank),][1:10,]
top_10$tuition_no_dollar <- gsub(pattern = "\\$|\\,",replacement = "", top_10$Tuition.and.fees)
mean(as.numeric(top_10$tuition_no_dollar))

#Average Tuition is USD 49895.2

#test version control