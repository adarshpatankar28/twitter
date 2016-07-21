giveslistwithcoordinate<-function(ddff){
  if(nrow(ddff)==0){
    ddff.list<-list("NO LOCATION TO SHOW",c(77.1717, 28.62739))
  }else{
    
    ddff.list <- split(ddff, seq(nrow(ddff)))
  for(i in 1:nrow(ddff))
  { s<-ddff$s2.i[i] 
  if(ddff$s2.i[i]<982)
  {ddff.list[[i]]<-list(ddff.list[[i]],roadswithoutdup[[ddff$s2.i[i]]][[1]])
  }else if(ddff$s2.i[i] > 981 &  ddff$s2.i[i]< 2741)
  {ddff.list[[i]]<-list(ddff.list[[i]],matrix(c(newpoints$coords.x1[s-981],newpoints$coords.x2[s-981]),nrow=1,ncol = 2))
  }else if(ddff$s2.i[i] >2740 &  ddff$s2.i[i]< 3558)
  {ddff.list[[i]]<-list(ddff.list[[i]],matrix(c(newplaces$coords.x1[s-2740],newplaces$coords.x2[s-2740]),nrow=1,ncol = 2))
  }
  }
  }
  ddff.list}