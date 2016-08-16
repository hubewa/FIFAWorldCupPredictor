cupGames = createDataFrame()
cupGames = dataFrameFill(cupGames, "wcGames2014a.csv", "points2014.csv", "2014 World Cup")
cupGames = dataFrameFill(cupGames, "2010matches.csv", "points2010.csv", "2010 World Cup")
cupGames = dataFrameClean(cupGames)

alpha = lm(Team1Score ~ pointDifferential * Team1PastGoals * Team2PastGoals * Team1GoalsConceeded * Team2GoalsConceeded, data = cupGames)
bravo = lm(Team1Score ~ pointDifferential + Team1PastGoals + Team2PastGoals + Team1GoalsConceeded + Team2GoalsConceeded, data = cupGames)
charlie = lm(Team1Score ~ pointDifferential*Team2PastGoals*Team1GoalsConceeded+pointDifferential*Team2PastGoals*Team1GoalsConceeded*Team2GoalsConceeded, data = cupGames)


#a = pDiff, b = 1pastgoals, c = 1goalsconceded, d = 2pastgoals, e = 2goalsconceded
delta<-lm(Team1Score~pointDifferential+Team1PastGoals*Team2GoalsConceeded+Team2PastGoals*Team1GoalsConceeded,data=cupGames)
#a + b * e + c * d
echo<-lm(Team1Score~pointDifferential+Team1PastGoals+Team2GoalsConceeded,data=cupGames)
#a + b + e
foxtrot<-lm(Team1Score~pointDifferential+pointDifferential*Team2GoalsConceeded + pointDifferential*Team1PastGoals*Team2GoalsConceeded,data=cupGames)
#a + a * b + a * b * e
golf<-lm(Team1Score~pointDifferential+pointDifferential*Team1PastGoals+pointDifferential*Team1PastGoals*Team2GoalsConceeded,data=cupGames)
#a + a * b + a * b * e
hotel<-lm(Team1Score~pointDifferential*Team1PastGoals*Team2GoalsConceeded+pointDifferential*Team1GoalsConceeded*Team2PastGoals,data=cupGames)
#a * b * e + a * c * d
india<-lm(Team1Score~pointDifferential*Team1PastGoals + pointDifferential*Team2GoalsConceeded + Team1PastGoals*Team2GoalsConceeded,data=cupGames)
#a * b + a * e + b * e
juliet<-lm(Team1Score~pointDifferential*Team1PastGoals + pointDifferential*Team2GoalsConceeded + Team1PastGoals*Team2GoalsConceeded + pointDifferential*Team1PastGoals*Team2GoalsConceeded,data=cupGames)

kilo <- lm(Team1Score~pointDifferential*Team2GoalsConceeded,data=cupGames)

AIC(alpha)

simulateGame("Germany", "Argentina", kilo, cupGames, "points2014.csv", "2014 World Cup")



penalties = createDataFrame()
penalties = penaltiesDataframe(cupGames,penalties)





alphaP = lm(team1PenaltiesScore ~ pointDifferential + Team1PastGoals + Team2PastGoals + Team1GoalsConceeded + Team2GoalsConceeded, data = cupGames)
betaP = lm(team1PenaltiesScore ~ pointDifferential + Team2PastGoals + Team2GoalsConceeded, data = cupGames)
charlieP = lm(team1PenaltiesScore ~ pointDifferential * Team2PastGoals * Team2GoalsConceeded, data = cupGames)
deltaP = lm(team1PenaltiesScore ~ Team2PastGoals * Team2GoalsConceeded, data = cupGames)

simulateGame("Germany","Argentina", betaP, cupGames, "points2014.csv", "2014 World Cup")