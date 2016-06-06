# someofmybestwork
#Alex Truman
#Econ 524 Project 3: Creating the Data Frame
#Called "finaldf"

#Initial pull from the read-in data
df=data.frame(Date=character(),AwayTeam=character(),AwayScore=numeric(),HomeTeam=character(),HomeScore=numeric(),Neutral=character())
widths=c(10,28,5,28,3,25)
for(i in 1960:2010){
  url=paste("http://homepages.cae.wisc.edu/~dwilson/rsfc/history/howell/cf",i,"gms.txt",sep="")
  print(url)
  newdf=read.fwf(url,widths=widths)
  df=rbind(df,newdf)
}
df=df[,-6]
colnames(df)=c("Date","AwayTeam","AwayScore","HomeTeam","HomeScore")

#Delete the ties----------
#Experimenting with different deletion methods
#Create vector with N/A if that row had a tie
#Bind to the df, and omit all rows with NA
dropit=matrix(0,nrow=nrow(df))
for(d in 1:nrow(df)){
  if(df[d,3]==df[d,5]){
    dropit[d,1]=NA
  }
}
df=cbind(df,dropit)
df=na.omit(df)
df=df[,-6]
rownames(df)=1:nrow(df)
#--------------------------

df[,1]=as.character(df[,1])

#Switch the January games
#Take the substring of the date referring to year
#If grep finds 01 preceding eight characters, subtract one from the year
years=matrix(0,nrow=nrow(df))
for(j in 1:nrow(df)){
  years[j,1]=substr(df[j,1],7,10)
}
years=as.numeric(years)
df=cbind(df,years)


for(i in 1:nrow(df)){
  if(grepl("01........",df[i,1])){
    df[i,6]=df[i,6]-1
  }
}

#Remove Spaces in Names
df[,2]=gsub(" ","",df[,2])
df[,4]=gsub(" ","",df[,4])


#Remove All DII Team Games
stepone=df[-(1:35409),]
steptwo=stepone
stepthree=steptwo

#Stepone is a matrix with all rows from any given year
#Steptwo is a matrix with from that year where any game with DII team is removed
#At the end of the loop, steptwo is added to stepthree (the matrix containing all years without DII games)
#Stepone, steptwo are reset to empty and the next year is calculated
for(y in 1960:2010){
  for(i in 1:nrow(df)){
    if(df[i,6]==y){
      stepone=rbind(stepone,df[i,])
    }
  }
  
  for(x in 1:nrow(stepone)){
    if(length(c(which(stepone$AwayTeam==stepone[x,2]),which(stepone$HomeTeam==stepone[x,2])))>5 & length(c(which(stepone$AwayTeam==stepone[x,4]),which(stepone$HomeTeam==stepone[x,4])))>5){
      steptwo=rbind(steptwo,stepone[x,])
    }
  }
  stepthree=rbind(stepthree,steptwo)
  stepone=df[-(1:35409),]
  steptwo=df[-(1:35409),]
}    

workingdf=stepthree

#Concatenate team name and years
for(i in 1:nrow(workingdf)){
  workingdf[i,2]=paste(c(workingdf[i,2],workingdf[i,6]),collapse="")
  workingdf[i,4]=paste(c(workingdf[i,4],workingdf[i,6]),collapse="")
}

#Unames=Vector of all unique teams (team ID does not carry over by year)
unames=unique(c(unique(workingdf[,2]),unique(workingdf[,4])))

#Teammat will contain teamnames conc. with year, team ID, and the opponents broken into home,away,total
teammat=matrix(,nrow=length(unames),ncol=5)
teammat[,1]=unames
teammat[,1]=as.character(teammat[,1])
teammat[,2]=1:length(unames)
teammat[,3]=as.character(teammat[,3])
teammat[,4]=as.character(teammat[,4])
teammat[,5]=as.character(teammat[,5])
####Opponents Columns
#Away grabs the index for where a team was an away team, home does the reverse
away=matrix(,nrow=31467)
for(i in 1:31467){
  away[i,1]=which(workingdf[i,2]==teammat[,1])
}
home=matrix(,nrow=31467)
for(i in 1:31467){
  home[i,1]=which(workingdf[i,4]==teammat[,1])
}

#Replace team names with team ID#
workingdf[,2]=as.character(away)
workingdf[,4]=as.character(home)

#Teammat[,3:4] find all instances of the team ID, and concatenate to a list of the team ID in the opposite cell
for(j in 1:length(unames)){
  if(sum(workingdf[,2]==j)>0){
    teammat[j,3]=paste(c(workingdf[which(workingdf$AwayTeam==j),4]),collapse = ",")
  }
  if(sum(workingdf[,4]==j)>0){
    teammat[j,4]=paste(c(workingdf[which(workingdf$HomeTeam==j),2]),collapse = ",")
  }
}  
#teammat[,5] is the complete list of opponents (I initially broke them up so
#it would be easier for me to visually confirm it was working)
for(j in 1:length(unames)){
  if(is.na(teammat[j,3])){
    teammat[j,5]=teammat[j,4]
  } else
    if(is.na(teammat[j,4])){
      teammat[j,5]=teammat[j,3]
    } else{
      teammat[j,5]=paste(c(teammat[j,3],teammat[j,4]),collapse = ",")
    }
}
#Creates a vector of the team ID for the winning team; loser does the reverse
winner=matrix(0,nrow=nrow(workingdf))
for(w in 1:nrow(workingdf)){
  if(workingdf[w,3]>workingdf[w,5]){
    winner[w,1]=workingdf[w,2]
  } else {
    winner[w,1]=workingdf[w,4]
  }
}

loser=matrix(0,nrow=nrow(workingdf))
for(l in 1:nrow(workingdf)){
  if(workingdf[l,3]<workingdf[l,5]){
    loser[l,1]=workingdf[l,2]
  } else {
    loser[l,1]=workingdf[l,4]
  }
}

#Newteammat has Team Name, # of Wins, # of Losses, Opponents List, Year
#Will become finaldf after I put in in data frame form
newteammat=matrix(NA,nrow=nrow(teammat),ncol=5)
#Team Name and Year is a de-concatenation
#i.e. teammat[1,1]="OregonState1960"==>"OregonState","1960"
newteammat[,1]=substr(teammat[,1],1,nchar(teammat[,1])-4)
newteammat[,5]=substr(teammat[,1],nchar(teammat[,1])-3,nchar(teammat[,1]))
newteammat[,4]=teammat[,5]
for(i in 1:nrow(newteammat)){
  newteammat[i,2]=length(which(winner==i))
}
for(i in 1:nrow(newteammat)){
  newteammat[i,3]=length(which(loser==i))
}
colnames(newteammat)=c("Team","Wins","Losses","Opponents","Year")
finaldf=as.data.frame(newteammat)

#Save it as "AlexTrumandf"
save(finaldf,file="AlexTrumandf.Rda")


#Econ 524 Project 3 Function
#Alex Truman

#Load the data frame
load("AlexTrumandf.Rda")
#Create a "tempyear" matrix with all rows from the given year (y)
year=function(y){
  tempyear=matrix(0,nrow=0,ncol=5)
  colnames(tempyear)=colnames(finaldf)
  for(i in 1:nrow(finaldf)){
    if(finaldf[i,5]==y){
      tempyear=rbind(tempyear,finaldf[i,])
    }
  }
  for(i in 1:5){
    tempyear[,i]=as.character(tempyear[,i])
  }
  for(j in 2:3){
    tempyear[,j]=as.numeric(tempyear[,j])
  }
  
  #Creates a matrix which essentially deconcatenates the opponent list string
  #Rows= # unique teams from that year
  #Columns= highest number of games played by any team that year
  x=matrix(0, nrow=nrow(tempyear),ncol=max(tempyear[,2]+tempyear[,3]),byrow=TRUE)
  for (i in 1:nrow(x)){
    for(j in 1:ncol(x)){
      x[i,j]=as.numeric(strsplit(tempyear[i,4],",",fixed=TRUE)[[1]][j])
    }
  }  
  #Evaluate conditional statements
  x[is.na(x)]=0
  
  #Creates the first stage of colley matrix
  A=matrix(0,nrow=nrow(x),ncol=nrow(x))
  row.names(A)=tempyear[,1]
  colnames(A)=tempyear[,1]
  
  
  #Fills in diagonals,
  #or finds the index for the team ID ranking and shows the negative number of games
  #between the two teams
  for(i in 1:nrow(x)){
    for(j in 1:nrow(x)){
      if(i==j){
        A[i,j]=2+tempyear[i,2]+tempyear[i,3]
      } else if(sum(x[i,1:ncol(x)]==as.numeric(rownames(tempyear[j,])))>0){
        A[i,j]=-1*sum(x[i,1:ncol(x)]==as.numeric(rownames(tempyear[j,])))
      }
    }
  }
  
  #The b matrix
  b=matrix(1+((tempyear[,2]-tempyear[,3])/2),nrow=nrow(A))
  
  #Solution
  Solution=solve(A,b)
  
  #Sorting the Solution Matrix, and Adding in the Ranking
  SolutionSort=matrix(c(rownames(Solution),Solution[,1]),ncol=2)
  SolutionSort=as.data.frame(SolutionSort)
  SolutionSort=SolutionSort[order(SolutionSort$V2,decreasing=TRUE),]
  SolutionSort=cbind(SolutionSort,1:nrow(SolutionSort))
  colnames(SolutionSort)=c("Team","Rating","Ranking")
  print(SolutionSort)
}

#Example
year(2005)
