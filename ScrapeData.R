
library(rvest)
library(lubridate)

##################################################################################################

# Function to get each games results for each week
GetGamesHistory = function(FirstYear = 2010, LastYear = 2019)
{
  games.URL.stem = "http://www.pro-football-reference.com/years/"
  
  for (year in FirstYear:LastYear)
  {
    URL = paste(games.URL.stem, year, "/games.htm", sep="")
    
    dfThisSeason <- read_html(URL)%>%
    html_table(fill=TRUE)%>%
      .[[1]] 
    
    # Clean up the df
    dfThisSeason = subset(dfThisSeason, Week!="Week")
    dfThisSeason = subset(dfThisSeason, Week!="")
    dfThisSeason$Date = as.character(dfThisSeason$Date)
    dfThisSeason$GameDate = mdy(paste(dfThisSeason$Date, year))
    dfThisSeason$Year = as.integer(year(dfThisSeason$GameDate))
    
    year(dfThisSeason$GameDate) = with(dfThisSeason, ifelse(month(GameDate) <=6, year(GameDate)+1, year(GameDate)))
    
    if (year == FirstYear)
    {
      dfAllSeasons = dfThisSeason
    } else {
      dfAllSeasons = rbind(dfAllSeasons, dfThisSeason)
    }
    
  }

  dfAllSeasons = dfAllSeasons[,c(15, 1, 5, 7, 9, 10, 11, 13, 12, 14, 6, 16)]
  colnames(dfAllSeasons) = c("GameDate", "Week", "Winner", "Loser", "WinnerPoints", "LoserPoints", "WinnerYrds", "LoserYrds", "WinnerTurnOvers", "LoserTurnOvers", "Location", "Year")
  
  dfAllSeasons$Winner = as.character(dfAllSeasons$Winner)
  dfAllSeasons$Loser = as.character(dfAllSeasons$Loser)
  dfAllSeasons$WinnerPoints = as.integer(as.character(dfAllSeasons$WinnerPoints))
  dfAllSeasons$LoserPoints = as.integer(as.character(dfAllSeasons$LoserPoints))
  dfAllSeasons$ScoreDifference = dfAllSeasons$WinnerPoints - dfAllSeasons$LoserPoints
  dfAllSeasons$WinnerYrds = as.integer(as.character(dfAllSeasons$WinnerYrds))
  dfAllSeasons$LoserYrds = as.integer(as.character(dfAllSeasons$LoserYrds))
  dfAllSeasons$WinnerTurnOvers = as.integer(as.character(dfAllSeasons$WinnerTurnOvers))
  dfAllSeasons$LoserTurnOvers = as.integer(as.character(dfAllSeasons$LoserTurnOvers))
  dfAllSeasons$Location = as.character(dfAllSeasons$Location)
  
  dfAllSeasons$HomeWin = 1
  #dfAllSeasons$AwayWin = 1
  dfAllSeasons$HomeWin[dfAllSeasons$Location == "@"] = 0
  #dfAllSeasons$AwayWin[dfAllSeasons$Location == ""] = 0
  
  dfAllSeasons$Week = as.numeric(as.character(dfAllSeasons$Week))
  
  dfAllSeasons = dfAllSeasons[!is.na(dfAllSeasons$Week),]
  dfAllSeasons = subset(dfAllSeasons, Week < 18)
  
  # set right teams
  #home team
  dfAllSeasons$HomeTeam = ifelse(dfAllSeasons$Location == "@", dfAllSeasons$Loser, dfAllSeasons$Winner) 
  dfAllSeasons$HomeScore = ifelse(dfAllSeasons$Location == "@", dfAllSeasons$LoserPoints, dfAllSeasons$WinnerPoints) 
  dfAllSeasons$HomeYrds = ifelse(dfAllSeasons$Location == "@", dfAllSeasons$LoserYrds, dfAllSeasons$WinnerYrds)
  dfAllSeasons$HomeTurnOvers = ifelse(dfAllSeasons$Location == "@", dfAllSeasons$LoserTurnOvers, dfAllSeasons$WinnerTurnOvers)
  #away team
  dfAllSeasons$AwayTeam = ifelse(dfAllSeasons$Location == "", dfAllSeasons$Loser, dfAllSeasons$Winner) 
  dfAllSeasons$AwayScore = ifelse(dfAllSeasons$Location == "", dfAllSeasons$LoserPoints, dfAllSeasons$WinnerPoints)
  dfAllSeasons$AwayYrds = ifelse(dfAllSeasons$Location == "", dfAllSeasons$LoserYrds, dfAllSeasons$WinnerYrds)
  dfAllSeasons$AwayTurnOvers = ifelse(dfAllSeasons$Location == "", dfAllSeasons$LoserTurnOvers, dfAllSeasons$WinnerTurnOvers)

  #dfAllSeasons <- dfAllSeasons[ -c(11) ]
  
  dfAllSeasons = subset(dfAllSeasons, !is.na(ScoreDifference))
  
  return (dfAllSeasons)
  
}

# Function to get current season scheduled games
GetCurrentSeason = function(year = 2020)
{
  games.URL.stem = "http://www.pro-football-reference.com/years/"
  
  URL = paste(games.URL.stem, year, "/games.htm", sep="")
  
  dfThisSeason <- read_html(URL)%>%
    html_table(fill=TRUE)%>%
    .[[1]] 
  
  # Clean up the df
  dfThisSeason = subset(dfThisSeason, Week!="Week")
  dfThisSeason = subset(dfThisSeason, Week!="")
  colnames(dfThisSeason)[3] <- "Date"
  dfThisSeason$Date = as.character(dfThisSeason$Date)
  dfThisSeason$GameDate = mdy(paste(dfThisSeason$Date, year))
  dfThisSeason$Year = as.integer(year(dfThisSeason$GameDate))
  
  year(dfThisSeason$GameDate) = with(dfThisSeason, ifelse(month(GameDate) <=6, year(GameDate)+1, year(GameDate)))
  
  dfThisSeason$Week = as.numeric(as.character(dfThisSeason$Week))
  dfThisSeason = dfThisSeason[!is.na(dfThisSeason$Week),]
  
  dfThisSeason = dfThisSeason[,c(10, 1, 4, 7, 11)]
  colnames(dfThisSeason) = c("GameDate", "Week", "AwayTeam", "HomeTeam", "Year")
  
  return (dfThisSeason)
  
}

# Function to get each teams offensive stats
GetAllTeamOffenseStats = function(FirstYear = 2010, LastYear = 2019)
{
  games.URL.stem = "http://www.pro-football-reference.com/years/"
  
  for (year in FirstYear:LastYear)
  {
    URL = paste(games.URL.stem, year, sep="")
    
    dfTeamStats = read_html(URL) %>% html_nodes(xpath = '//comment()') %>%    # select comments
      html_text() %>%    # extract comment text
      paste(collapse = '') %>%    # collapse to single string
      read_html() %>%    # reread as HTML
      html_node('table#team_stats') %>%    # select desired node
      html_table()
    
    names(dfTeamStats) <- as.matrix(dfTeamStats[1, ])
    dfTeamStats <- dfTeamStats[-1, ]
    dfTeamStats[] <- lapply(dfTeamStats, function(x) type.convert(as.character(x)))
    dfTeamStats$Year = as.integer(year)
    
    if (year == FirstYear)
    {
      dfAllSeasons = dfTeamStats
    } else {
      dfAllSeasons = rbind(dfAllSeasons, dfTeamStats)
    }
    
  }
  
  dfAllSeasons$Rk = as.numeric(as.character(dfAllSeasons$Rk))
  dfAllSeasons = dfAllSeasons[!is.na(dfAllSeasons$Rk),]
  
  return (dfAllSeasons)
  
}

# Function to get each teams annual standings win/loss
GetAllTeamStandings = function(FirstYear = 2010, LastYear = 2019)
{
  games.URL.stem = "http://www.pro-football-reference.com/years/"
  
  for (year in FirstYear:LastYear)
  {
    URL = paste(games.URL.stem, year, sep="")
    
    dfNFCStandings = read_html(URL)  %>%   
      html_node('table#NFC') %>%    # select desired node
      html_table()
    
    dfAFCStandings = read_html(URL)  %>%   
      html_node('table#AFC') %>%    # select desired node
      html_table()
    
    dfAllTeams = rbind(dfNFCStandings, dfAFCStandings)
    dfAllTeams$Year = as.integer(year)
    
    dfAllTeams$W = as.numeric(as.character(dfAllTeams$W))
    dfAllTeams = dfAllTeams[!is.na(dfAllTeams$W),]
    
    nms <- c("T")
    Missing <- setdiff(nms, names(dfAllTeams))  # Find names of missing columns
    dfAllTeams[Missing] <- 0 
    
    if (year == FirstYear)
    {
      dfAllTeamSeasons = dfAllTeams
    } else {
      dfAllTeamSeasons = rbind(dfAllTeamSeasons, dfAllTeams)
    }
    
  }
  
  return (dfAllTeamSeasons)
  
}


gamesHistorydf = GetGamesHistory()

write.csv(gamesHistorydf, file = "NFLGamesHistory.csv",row.names=T, na="")

gamesCurrentdf = GetCurrentSeason()

write.csv(gamesCurrentdf, file = "NFLGamesCurrentSeason.csv",row.names=T, na="")

teamOffenseStatsDf = GetAllTeamOffenseStats()

write.csv(teamOffenseStatsDf, file = "NFLTeamStats.csv",row.names=T, na="")

teamStandingsDf = GetAllTeamStandings()

write.csv(teamStandingsDf, file = "NFLTeamStandings.csv",row.names=T, na="")

######################

# total number of games
total_games = nrow(gamesHistorydf)

# total number of home wins
home_wins = sum(gamesHistorydf$HomeWin)

# total home wins/total number of games
home_win_rate = home_wins/total_games

######################


FRANCHISES = c('crd', 'atl', 'rav', 'buf', 'car', 'chi', 'cin', 'cle', 'dal',
              'den', 'det', 'gnb', 'htx', 'clt', 'jax', 'kan', 'mia', 'min',
              'nwe', 'nor', 'nyg', 'nyj', 'rai', 'phi', 'pit', 'sdg', 'sfo',
              'sea', 'ram', 'tam', 'oti', 'was')

FRANCHISE_NAMES = c('Arizona Cardinals', 'Atlanta Falcons', 'Baltimore Ravens',
                   'Buffalo Bills', 'Carolina Panthers', 'Chicago Bears',
                   'Cincinnati Bengals', 'Cleveland Browns', 'Dallas Cowboys',
                   'Denver Broncos', 'Detroit Lions', 'Green Bay Packers',
                   'Houston Texans', 'Indianapolis Colts',
                   'Jacksonville Jaguars', 'Kansas City Chiefs',
                   'Miami Dolphins', 'Minnesota Vikings',
                   'New England Patriots', 'New Orleans Saints',
                   'New York Giants', 'New York Jets', 'Oakland Raiders',
                   'Philadelphia Eagles', 'Pittsburgh Steelers',
                   'Los Angeles Chargers', 'San Francisco 49ers', 'Seattle Seahawks',
                   'Los Angeles Rams', 'Tampa Bay Buccaneers',
                   'Tennessee Titans', 'Washington Redskins')


# Function to get each teams game history
BuildTeamStats = function(FirstYear = 2010, LastYear = 2019)
{
  games.URL.stem = "http://www.pro-football-reference.com/teams/"
  
  counter = 1
  
  for (team in FRANCHISES) 
  {
    
    for (year in FirstYear:LastYear)
    {
      URL = paste(games.URL.stem, team, "/", year, ".htm", sep="")
      
      dfTeamGames = read_html(URL)  %>%   
        html_node('table#games') %>%    # select desired node
        html_table()
      
      names(dfTeamGames) <- as.matrix(dfTeamGames[1, ])
      dfTeamGames <- dfTeamGames[-1, ]
      dfTeamGames[] <- lapply(dfTeamGames, function(x) type.convert(as.character(x)))
      
      dfTeamGames$Tm = as.numeric(as.character(dfTeamGames$Tm))
      dfTeamGames = dfTeamGames[!is.na(dfTeamGames$Tm),]
      
      dfTeamGames$Week = as.numeric(as.character(dfTeamGames$Week))
      dfTeamGames = dfTeamGames[!is.na(dfTeamGames$Week),]
      
      # Clean up the df
      dfTeamGames = subset(dfTeamGames, Week!="Week")
      dfTeamGames = subset(dfTeamGames, Week!="")
      dfTeamGames$Date = as.character(dfTeamGames$Date)
      dfTeamGames$GameDate = mdy(paste(dfTeamGames$Date, year))
      dfTeamGames$Year = as.integer(year(dfTeamGames$GameDate))
      dfTeamGames$Team = as.character(team)
      
      year(dfTeamGames$GameDate) = with(dfTeamGames, ifelse(month(GameDate) <=6, year(GameDate)+1, year(GameDate)))
   
      
      if (year == FirstYear)
      {
        dfCurrentTeamGames = dfTeamGames
      } else {
        dfCurrentTeamGames = rbind(dfCurrentTeamGames, dfTeamGames)
      }
      
    }

    
    if (counter == 1)
    {
      dfAllTeamGames = dfCurrentTeamGames
    } else {
      dfAllTeamGames = rbind(dfAllTeamGames, dfCurrentTeamGames)
    }
    
    counter = counter + 1
    
  }
  
  dfAllTeamGames = dfAllTeamGames[,c(26, 1, 28, 27, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25)]
  colnames(dfAllTeamGames) = c("GameDate", "Week", "Tm", "Year", "W_L", "OT", "Rec", "Location", "Opp", "TmScore", "OppScore",
                               "Tm1stD", "TmTotYd", "TmPassY", "TmRushY", "TmTO", "Opp1stD", "OppTotYd", "OppPassY", "OppRushY", "OppTO",
                               "ExpOff", "ExpDef", "ExpSpTms")
  
  
  
  return (dfAllTeamGames)
  
}


allTeamStatsDf = BuildTeamStats()

write.csv(allTeamStatsDf, file = "NFLTeamGameStats.csv",row.names=T, na="")

write.csv(FRANCHISE_NAMES, file = "NFLFranchises.csv",row.names=T, na="")



# build ows based on previous season or build as each season independent?
#https://rpubs.com/bjameslebo/NFLPredictions
#https://rpubs.com/bjameslebo/NFLRankings

#library(nflscrapR)
#game_play_by_play(GameID = 2013020300)
#scrape_json_play_by_play (GameID = 2013020300)
#https://gist.github.com/guga31bb/5634562c5a2a7b1e9961ac9b6c568701

############################################################################
## References:
#http://timothykylethomas.me/nfl-prediction-1.html
#https://www.credera.com/blog/business-intelligence/using-machine-learning-predict-nfl-games/#disqus_thread
#https://github.com/pjoos/NFL_Project/blob/master/webscraping_main.py
#https://github.com/King-Pie/NFL_predict
#https://www.pro-football-reference.com/years/2018/opp.htm
#https://www.pro-football-reference.com/years/2018/
#https://github.com/xsankar/hairy-octo-hipster/blob/master/ELO-538.R
#https://fivethirtyeight.com/methodology/how-our-nfl-predictions-work/
#https://medium.com/s/story/how-i-outsmarted-fivethirtyeights-nfl-forecasting-algorithm-592a301fa318
