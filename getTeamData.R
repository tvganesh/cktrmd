getTeamvsOppositionData <- function(team,opposition,dir="./data",file="team001.csv",type="batting",
                          homeOrAway=c(1,2,3),result=c(1,2,4)) {
    
    # Initial url to ""
    url <-""
    suburl1 <- "http://stats.espncricinfo.com/ci/engine/stats/index.html?"
    suburl2 <-"class=1;"
    suburl3 <- "template=results;"
    suburl4 <- "view=innings"
    
    team1 <- paste("team=",team,";",sep="")
    opposition1 <- paste("opposition=",opposition,";",sep="")
  
    
    # Set the home or away
    str1=str2=str3=""
    if(sum(homeOrAway == 1)==1){
        str1 ="home_or_away=1;"
    }
    if (sum(homeOrAway == 2)==1) {
        str2="home_or_away=2;"
    }
    if (sum(homeOrAway == 3)==1) {
        str3="home_or_away=3;"
    }
    HA<-paste(str1,str2,str3,sep="")
    
    # Set the type batting or bowling
    t <- paste("type=",type,";",sep="");
    
    # Set the result based on input
    str1=str2=str3=""
    if(sum(result==1)==1){
        str1 ="result=1;"
    }
    if(sum(result==2)==1){
        str2 ="result=2;"
    }
    if(sum(result==4)==1){
        str3 ="result=4;"
    }
    result1<- paste(str1,str2,str3,sep="")
    repeat {
       pageNo=38
       page <- paste("page=",pageNo,";",sep="")
       # Create composite URL
       url <- paste(suburl1,suburl2,HA,opposition1,page, result1,team1,suburl3,t,suburl4,sep="")
       print(url)
       # Read the data from ESPN Cricinfo
       tables=readHTMLTable(url,stringsAsFactors = F)
       # Choose appropriate columns
       t <- tables$"Innings by innings list"
    
    
    # Choose appropriate columns
    t <- tables$"Innings by innings list"
    
    if(type=="batting") {
        cols <- c(1:9,11,12,13)
    } else if (type=="bowling") {
        # Check if there are the older version of 8 balls per over (BPO) column
        n <- names(t)
        
        # Select BPO column for older bowlers
        if(n[2] =="BPO") {
            cols <- c(1:8,10,11,12)
        }
        else {
            cols <- c(1:7,9,10,11)
        }
    }
    
    s <- t[,cols]
    
    
    dir.create(dir,mode="0777",showWarnings=FALSE)
    file <-paste(dir,file,sep="/")
    
    file.create(file)
    
    # Write to file 
    write.csv(s,file=file)
    
    # Return the data frame
    s
    
}