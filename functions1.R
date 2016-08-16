createDataFrame <- function(){
  df<-data.frame(Team1Rank=integer(),
                 Team2Rank=integer(),
                 pointDifferential = integer(), #to do with points based on FIFA Rankings
                 Team1PastGoals=integer(), 
                 Team2PastGoals=integer(),
                 Team1GoalsConceeded=integer(),
                 Team2GoalsConceeded=integer(),
                 Team1Score=integer(),
                 Team2Score=integer(),
                 team1PenaltiesScore=integer(),
                 team2PenaltiesScore=integer(),
                 tournament=character(),
                 stringsAsFactors=FALSE)
  return(df)
}

removeDataFrame <- function(dataframe){
  rm(dataframe)
}

dataFrameFill <- function(dataframe,fileGames,fileRankings,tournament){
  games <- read.csv(file=fileGames, header=TRUE)
  ranking <- read.csv(file=fileRankings, header=TRUE, stringsAsFactors=FALSE)
  
  
  #Creates an array describing the number of goals scored
  teamGoals <- vector("integer", 200)
  teamGoalsConceeded <- vector("integer", 200)
  
  gameRows <- nrow(games)
  i = 1
  while(i <= gameRows){
    Team1Rank = grep(as.character(games$Team1[i]),ranking$Team)
    Team2Rank = grep(as.character(games$Team2[i]),ranking$Team)
        
    pointDiff = ranking$TotalPoints[Team1Rank] - ranking$TotalPoints[Team2Rank]
    
    score = as.character((games$Result[i]))
    
    a = as.integer(strsplit(score, "-")[[1]])
    Team1Goals = a[1]
    
    #checks for Penalties
    if(is.na(a[2])){
      Team2Goals = a[1]
      pennScore = as.character(games$Penalty.shoot.out[i])
      b = as.integer(strsplit(pennScore, "-")[[1]])
      team1PennScore = b[1]
      team2PennScore = b[2]
    } else {
      Team2Goals = a[2]
      team1PennScore = ""
      team2PennScore = ""
    }
    
    
    index = nrow(dataframe)+1
    dataframe[index,] <- c(Team1Rank,Team2Rank,pointDiff,teamGoals[Team1Rank],teamGoals[Team2Rank],teamGoalsConceeded[Team1Rank],teamGoalsConceeded[Team2Rank],Team1Goals,Team2Goals,team1PennScore,team2PennScore,tournament)
    
    #THIS LINE IS IMPORTANT FOR REPORT
    dataframe[index+1,] <- c(Team2Rank,Team1Rank,-pointDiff,teamGoals[Team2Rank],teamGoals[Team1Rank],teamGoalsConceeded[Team2Rank],teamGoalsConceeded[Team1Rank],Team2Goals,Team1Goals,team2PennScore,team1PennScore,tournament)
    
    
    teamGoals[Team1Rank] = teamGoals[Team1Rank] + Team1Goals
    teamGoals[Team2Rank] = teamGoals[Team2Rank] + Team2Goals
    teamGoalsConceeded[Team1Rank] = teamGoalsConceeded[Team1Rank] + Team2Goals
    teamGoalsConceeded[Team2Rank] = teamGoalsConceeded[Team2Rank] + Team1Goals
    i = i+1
  }
  return(dataframe)
} 

dataFrameClean <- function(dataframe){
  m <- ncol(dataframe)
  for(i in 1:(m-1)){
    dataframe[,i] <- as.numeric(dataframe[,i])
  }
  return(dataframe)
}

#This function will create an array in the order of (nation1 goals, nation1Conceeded) for a particular tournament
goalStats <- function(nationRank,dataframe,tournament){
  index <- nrow(dataframe)
  goalScored = 0
  goalConceeded = 0
  for(i in 1:index){
    if(nationRank == dataframe$Team1Rank[i] & tournament == dataframe$tournament[i]){
      goalScored = goalScored + dataframe$Team1Score[i]
      goalConceeded = goalConceeded + dataframe$Team2Score[i]
    }
  }
  a = c(goalScored,goalConceeded)
  return(a)
}


simulateGame <- function(nation1,nation2,model,dataframe,fileRanking,tournament){
  ranking <- read.csv(file=fileRanking, header=TRUE, stringsAsFactors=FALSE)
  
  Team1Rank = grep(nation1,ranking$Team)
  Team2Rank = grep(nation2,ranking$Team)
  
  point = ranking$TotalPoints[Team1Rank] - ranking$TotalPoints[Team2Rank]
                    
  Team1GoalStats = goalStats(Team1Rank,dataframe,tournament)
  Team2GoalStats = goalStats(Team2Rank,dataframe,tournament)
  
  pointDifferential<-c(point, -point)
  Team1PastGoals<-c(Team1GoalStats[1],Team2GoalStats[1])
  Team1GoalsConceeded <- c(Team1GoalStats[2], Team2GoalStats[2])
  Team2PastGoals <- c(Team2GoalStats[1], Team1GoalStats[1])
  Team2GoalsConceeded <- c(Team2GoalStats[2],Team1GoalStats[2])
  
  predictor.df <- data.frame(pointDifferential,Team1PastGoals,Team1GoalsConceeded,Team2PastGoals,Team2GoalsConceeded)
  
  TeamScore = predict(model,newdata = predictor.df,se.fit=T, interval="confidence", level=0.95)
  
  print(predictor.df)
  
  return(TeamScore)
}

penaltiesDataframe<-function(dataframe,penDataframe){
  gameRows <- nrow(dataframe)
  
  for(i in 1:gameRows){
    if(!is.na(dataframe$team1PenaltiesScore[i])){
      index = nrow(penDataframe)+1
      penDataframe[index,] <- c(dataframe$Team1Rank[i],dataframe$Team2Rank[i],dataframe$pointDifferential[i],dataframe$Team1PastGoals[i],dataframe$Team2PastGoals[i],dataframe$Team1GoalsConceeded[i],dataframe$Team2GoalsConceeded[i],dataframe$Team1Score[i],dataframe$Team2Score[i],dataframe$team1PenaltiesScore[i],dataframe$team2PenaltiesScore[i],dataframe$tournament[i])
    }
  }
  return(penDataframe)
}