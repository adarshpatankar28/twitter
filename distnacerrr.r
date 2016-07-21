distance<-function(tocheck){
  if(length(tocheck)==0){
    mymatch.s1.s3<-data.frame()
  }else{
    getbig <- function(input){
      input<-tolower(input)
      splitted<-strsplit(input,split =" ")
      splitted<-splitted[[1]]
      out<-list()
      for(i in 1:length(splitted)){ 
        if(nchar(splitted[i])==1)
        {
          out[[i]]<-splitted[i]
        }
        else{
          out[[i]]<-substring(splitted[i], 1:(nchar(splitted[i])-1), 2:nchar(splitted[i]))
        }
      }
      out
      output<-unlist(out)
      output
    }
    ####simial
    similar<-function(of1,of2){
      
      union<-length(of1)+length(of2)
      count=0
      for (x in of1){
        for (y in of2){
          if(x == y){
            count =count + 1
            break}}}
      (2.0 *count)/union*100
      
    }
    
    
    
    tocheck<-as.array(tocheck)
    if(is.matrix(apply(tocheck,1,getbig))){
      maatr<-(apply(tocheck,1,getbig))
      afterbigtocheck<-split(maatr, rep(1:ncol(maatr), each = nrow(maatr)))
      
    }else{
      afterbigtocheck<-(apply(tocheck,1,getbig))
    }
    ssee<-list()
    for(i in 1:length(afterbigtodata)){
      ssee[[i]]<-sapply(afterbigtocheck,similar,afterbigtodata[[i]])}
    hhh<-matrix(unlist(ssee), nrow=length(afterbigtocheck))
    hhh
    mymin.name<-apply(hhh, 1, max)
    mymatch.s1.s3<-NULL  
    for(i in 1:nrow(hhh))
    {
      s2.i<-match(mymin.name[i],hhh[i,])
      s1.i<-i
      mymatch.s1.s3<-rbind(data.frame(s2.i=s2.i,s1.i=s1.i,frm.database=database[s2.i], to.check=tocheck[s1.i], adist=mymin.name[i]),mymatch.s1.s3)
    }
    (mymatch.s1.s3)
    mymatch.s1.s3<-mymatch.s1.s3[which(mymatch.s1.s3$adist>72),]
    mymatch.s1.s3
  }
  
}