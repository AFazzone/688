rm(list=ls()); cat("\014")
library(SportsAnalytics)
library(XML)
library(httr)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyverse)
library(RColorBrewer)



nba2021 <- fetch_NBAPlayerStatistics("20-21")

levels(nba2021$League)
names(nba2021)

Boston <- (subset(nba2021, Team== "BOS"))


#Create percentage Column
Boston$threePerct <- Boston$ThreesMade/Boston$ThreesAttempted
Boston$FreePerct <- Boston$FreeThrowsMade/Boston$FreeThrowsAttempted
Boston$FieldPerct <- Boston$FieldGoalsMade/Boston$FieldGoalsAttempted

#Fill in Null Values
Boston$threePerct[is.na(Boston$threePerct)] = 0
Boston$FreePerct[is.na(Boston$threePerct)] = 0
Boston$FieldPerct[is.na(Boston$threePerct)] = 0


max(Boston$threePerct)
highest_3s <- which.max(Boston$threePerct)
Boston$Name[highest_3s]

#Find and plot top 5 Three percent shooters in Boston
tib <- as_tibble(Boston)
names(tib)
tib_arr <- arrange(tib, desc(threePerct))
tib_arr
three_per <- pull(tib_arr, threePerct)
names2 <- pull(tib_arr, Name)
coul <- brewer.pal(5, "Set2") 
barplot(head(three_per), main = "Three Pointer Percentage",
        ylab = "Percent", names = head(names2), col = coul,
        cex.sub = .01)

#Find and plot top 5 Free throw percent shooters in Boston
tib_arr2 <- arrange(tib, desc(FreePerct))
tib_arr2
free_per <- pull(tib_arr2, FreePerct)
names2 <- pull(tib_arr2, Name)
coul <- brewer.pal(5, "Set2") 
barplot(head(free_per), main = "Free Throw Percentage",
        ylab = "Percent", names = head(names2), col = coul,
        cex.sub = .01)

#Find and plot top 5 Field Goal
tib <- as_tibble(Boston)
names(tib)
tib_arr3 <- arrange(tib, desc(FieldPercent))
tib_arr3
three_per <- pull(tib_arr3, FieldPercent)
names2 <- pull(tib_arr3, Name)
coul <- brewer.pal(5, "Set2") 
barplot(head(three_per), main = "Field Goal Percentage",
        ylab = "Percent", names = head(names2), col = coul,
        cex.sub = .01)




names(nba2021)

#Celtics vs Lakers total points


names(nba2021)
tib1 <- as_tibble(nba2021)
year_total <- tib1 %>% group_by(Team) %>% summarise(sum(TotalPoints))
three_total <- tib1 %>% group_by(Team) %>% summarise(sum(ThreesMade))
field_total <- tib1 %>% group_by(Team) %>% summarise(sum(FieldGoalsMade))
free_total <- tib1 %>% group_by(Team) %>% summarise(sum(FreeThrowsMade))

points <- c("Free Throws", "Feild Goals", "Three Pointers")

b1 <-subset(free_total, Team=="BOS")
b2 <-subset(field_total, Team=="BOS")
b3 <-subset(three_total, Team=="BOS")

B_points <- c(as.numeric(b1[2]),as.numeric(2*b2[2]),as.numeric(3*b3[2]))
B_points

l1 <-subset(free_total, Team=="LAL")
l2 <-subset(field_total, Team=="LAL")
l3 <-subset(three_total, Team=="LAL")

L_points <- c(as.numeric(l1[2]),as.numeric(2*l2[2]),as.numeric(3*l3[2]))
L_points

b_vs_l <- rbind(B_points, L_points)
colnames(b_vs_l) <- points
rownames(b_vs_l) <- c("Celtics", "Lakers")
mosaicplot(t(b_vs_l), color=c("limegreen", "purple3"),
           main = "Celtics vs. Lakers Points")

#Celtics vs Lakers Defense

names(nba2021)
tib22 <- as_tibble(nba2021)
tib22
reb_total <- tib22 %>% group_by(Team) %>% summarise(sum(TotalRebounds))
steal_total <- tib22 %>% group_by(Team) %>% summarise(sum(Steals))
blocks_total <- tib22 %>% group_by(Team) %>% summarise(sum(Blocks))

defense <- c( "Steals","Rebounds", "Blocks")

b22 <-subset(reb_total, Team=="BOS")
b11 <-subset(steal_total, Team=="BOS")
b33 <-subset(blocks_total, Team=="BOS")

B_def <- c(as.numeric(b11[2]),as.numeric(b22[2]),as.numeric(b33[2]))
B_def

l22 <-subset(reb_total, Team=="LAL")
l11<-subset(steal_total, Team=="LAL")
l33 <-subset(blocks_total, Team=="LAL")

L_def <- c(as.numeric(l11[2]),as.numeric(l22[2]),as.numeric(l33[2]))
L_def

b_vs_l_def <- rbind(B_def, L_def)
colnames(b_vs_l_def) <- defense
rownames(b_vs_l_def) <- c("Celtics", "Lakers")
mosaicplot(t(b_vs_l_def), color=c("limegreen", "purple3"),
           main = "Celtics vs. Lakers Defense")



#Celtics vs Lakers Penalties


names(nba2021)
tib33 <- as_tibble(nba2021)
turn_total <- tib33 %>% group_by(Team) %>% summarise(sum(Turnovers))
pf_total <- tib33 %>% group_by(Team) %>% summarise(sum(PersonalFouls))
#ff_total <- tib33 %>% group_by(Team) %>% summarise(sum(FlagrantFouls))
penalties <- c("Turnovers", "PersonalFouls")

b111 <-subset(turn_total, Team=="BOS")
b222<-subset(pf_total, Team=="BOS")
#b333 <-subset(ff_total, Team=="BOS")
b222
B_pen <- c(as.numeric(b111[2]),as.numeric(b222[2]))
B_pen

l111 <-subset(turn_total, Team=="LAL")
l222<-subset(pf_total, Team=="LAL")
#l333 <-subset(ff_total, Team=="LAL")

L_pen <- c(as.numeric(l111[2]),as.numeric(l222[2]))
L_pen

b_vs_l_pen <- rbind(B_pen, L_pen)
b_vs_l_pen
colnames(b_vs_l_pen) <- penalties
rownames(b_vs_l_pen) <- c("Celtics", "Lakers")
mosaicplot(t(b_vs_l_pen), color=c("limegreen", "purple3"),
           main = "Celtics vs. Lakers Penalties")




#analysis by team 

team_PersonalFouls <- tib1 %>% group_by(Team) %>% summarise(sum(PersonalFouls))
team_PersonalFouls
tib_arr3 <- arrange(team_PersonalFouls, desc(team_PersonalFouls[2]))
tib_arr3
names(tib)

team_Turnovers <- tib1 %>% group_by(Team) %>% summarise(sum(Turnovers))
team_Turnovers

tib_arr4 <- arrange(team_Turnovers, desc(team_Turnovers[2]))
tib_arr4

#Plot points per position across all of NBA

position_points <- tib1 %>% group_by(Position) %>% summarise(sum(TotalPoints))
tib4 <- tibble(position_points)

pos_pts <- pull(tib4, c(2)) 
pos <- pull(tib4, c(1)) 
pos
pie(pos_pts, main = "Points Per Position",labels = round((pos_pts*100)/sum(pos_pts),2), col= coul)
legend("left",as.character(pos), cex = .8, fill = coul)
tib4
#c("Center", "Power Forward", "Point Guard", "Small Forward", "Shooting Guard")


#Plot rebounds per position across all of NBA

position_rebound <- tib1 %>% group_by(Position) %>% summarise(sum(TotalRebounds))
tib5 <- tibble(position_rebound)

pos_reb <- pull(tib5, c(2)) 
pos <- pull(tib5, c(1)) 
pos
pie(pos_reb, main = "Rebounds Per Position",labels = round((pos_reb*100)/sum(pos_reb),2), col= coul)
legend("left",as.character(pos), cex = .8, fill = coul)

#Plot assists per position across all of NBA

position_assist <- tib1 %>% group_by(Position) %>% summarise(sum(Assists))
tib5 <- tibble(position_assist)

pos_ast <- pull(tib5, c(2)) 
pos <- pull(tib5, c(1)) 
pos
pie(pos_ast, main = "Assists Per Position",labels = round((pos_ast*100)/sum(pos_ast),2), col= coul)
legend("left",as.character(pos), cex = .8, fill = coul)



