function(input){
  ###########tokenntags###############
  tagsntokens<-function(v) 
  {
    mm<-strsplit(v,"\t")
    org1<-mm[[1]][4]
    tok1<-mm[[1]][1]
    tag1<-mm[[1]][2]
    toks1<-strsplit(tok1,split =" ")
    tags1<-strsplit(tag1,split =" ")
    ddd<-  data.frame(toks1[[1]],tags1[[1]],stringsAsFactors = FALSE)
    colnames(ddd)<-c("tokens","tags")
    ddd
  }
  #########################################
  tnt.df<-tagsntokens(input)
  tagfrmdf<-paste(tnt.df$tags,collapse = " ")
  tagfrmdf<-gsub("\\^","F",tagfrmdf)         #####tonoun
  tagfrmdf<-gsub("N","F",tagfrmdf)          #########tonun
  tagfrmdf<-paste(tagfrmdf,"", collapse = "")
  newnpchunkerrr <- function(input) {
    p <- "(\\<D\\> )?((\\<A\\> )*)?((\\<F\\> )+)"
    r <- "( \\1\\2\\4) "
    output <- gsub(input, pattern = p, replacement = r)
    ########## output <- paste("(S", output, ")", sep = "")
    output
  }
  prenounpre <- function(input) {
    p <- "(\\<P\\> )((\\<A\\> )*)?((\\<F\\> )+)(\\<P\\> )"
    r <- "( \\2\\4) "
    output <- gsub(input, pattern = p, replacement = r)
    #### output <- paste("(S ", output, ")", sep = "")
    output
  }
  chunkedprep<-prenounpre(tagfrmdf)
  chunkednoun<-newnpchunkerrr(tagfrmdf)
  
  ####################################
  findphrases<-function(input){
    input<-strsplit(input,split=" ")
    indleft<-grep("\\(", input[[1]],value = F)
    indrght<-grep("\\)", input[[1]],value = F)
    if(length(indleft)==0){
      nounphrase<-vector()
      qq<-vector(mode = "character")
      qq
    }
    else{
      n<-length(input[[1]])
      newindex<-vector()
      for(i in 1:n){
        q<-length(indleft[indleft<i])
        w<-length(indleft[indrght<i])
        newindex[i]<-i-(q+w)
      }
      a<-indleft+1
      b<-indrght-1
      aa<-newindex[a]
      bb<-newindex[b]
      nounphrase<-list()
      for(i in 1:length(aa)){
        nounphrase<-c(list(tnt.df[aa[i]:bb[i],]),(nounphrase))
      }
      nounphrase
      qqq<-vector()
      for(i in 1:length(nounphrase)){
        qqq[i]<-paste((nounphrase[[i]]$tokens), collapse = " ")}
      qqq
    }
  }
  nouns<-findphrases(chunkednoun)
  prep<-findphrases(chunkedprep)
  tocheck<-c(prep,nouns)
  tocheck
}