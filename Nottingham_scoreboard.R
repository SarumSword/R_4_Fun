value <- c(2,4,3,3,9,8,6,7,20,5,1)
tpe <- c("Apples","Chicken","Bread","Cheese","Bow","Cloth","Pepper","Mead","20's","5's","1's")
king <- c(20,10,15,15)
queen <- c(10,5,10,10)

print("How many players? 3,4,5, or 6?")
players <- readline()
while(typeof(players)=="character" || players<3 && players>6){
  if(typeof(players)=="character"){
    if(length(players)==1){
      players <- as.double(players)
    }else{
      players <- strsplit(x = tolower(players),split = " ")[[1]][1]
      if("three" %in% players){
        players <- 3
      }else if("four" %in% players){
        players <- 4
      }else if("five" %in% players){
        players <- 5
      }else if("six" %in% players){
        players <- 6
      }else{
        print("Choose only from 3,4,5, or 6")
        players <- readline()
      }
    }
  }else{
    print("Choose only from 3,4,5, or 6")
    players <- readline()
  }
}

player_names <- vector(length = players)
results <- matrix(0L,nrow = players,ncol = length(tpe)+length(king)+length(queen)+1)
colnames(results) <- c(tpe,paste("king",tpe[1:4]),paste("queen",tpe[1:4]),"Results")

for (i in 1:players) {
  print(paste("Provide name/designation of player",i))
  player_names[i] <- readline()
  for(j in 1:length(tpe)){
    temp <- -1
    while (typeof(temp)!="double" || temp<0) {
      print(paste("Provide *numeric* quantity of",tpe[j],"for",player_names[i]))
      temp <- readline()
      temp <- as.double(temp)
      if(NA %in% temp){
        temp <- -1
      }
    }
    results[i,j]<- temp
  }
}
rownames(results) <- player_names

for(i in 1:4){
  king_bonus <- which(results[,i]==max(results[,i]))
  queen_bonus <- which(results[,i]==max(results[-king,i]))
  results[king_bonus,i+11] <- king[i]/length(king_bonus)
  results[queen_bonus,i+15] <- king[i]/length(queen_bonus)
}

for(i in 1:players){
  results[i,ncol(results)] <- sum(results[i,1:(ncol(results)-1)])
}

print(results)

print(paste("Position",1:players,"goes to",player_names[order(results[,ncol(results)],decreasing = TRUE)]))
