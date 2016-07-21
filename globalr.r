# Global setups
afterbigtodata<-readRDS("afterbigtodatarecent.rds")
database<-readRDS("array(database).rds")
roadswithoutdup<-readRDS("roadswithcoordinate.rds")
newpoints<-readRDS("pointswithcoordinate.rds")
newplaces<-readRDS("placeswithcoordinate.rds")


library(twitteR)
library(rjson)
library(base64enc) # fix for twitter oauth in shinyapps.io
runOnline = T

# Load twitter authorization
if(runOnline){
  ##########secrets <- fromJSON(file='twitter_secrets.json.nogit')
  api_key <- "fMuhJ19QVaKNOyXvxuLM2W2IX" # From dev.twitter.com
  api_secret <- "yBX9RY9HHYmzDZV2ddavxbYYN02NK9pDTWhWVhUQKspt7Voc7R" # From dev.twitter.com
  token <- "335259796-kYgi4K5QFAkuUxNXXTOKYhzoA8YIVNDrs2hxCQmT" # From dev.twitter.com
  token_secret <- "2AbO47qIw66mp5puFYwCFFcO9v9ifwUdzbHhNGylVVhAm" # From dev.twitter.com
  
  # Create Twitter Connection
  setup_twitter_oauth(api_key, api_secret, token, token_secret)
  
}

# Grab tweets
getTweets <- function(){
  
  if(runOnline){
    
    tweets <- searchTwitter("@dtptraffic", n=10, lang="en",geocode="28.6139,77.2090,30mi",resultType = "recent")
    tweets.df <- twListToDF(tweets)
    tweets.df$created<-as.character(tweets.df$created)
    tweets.df
    
   return(tweets.df)}
}

# Grab text data
getTaggedData <- function(tweets.df) {
  
  tatt<-as.character(tweets.df$text)
  tatt<-iconv(tatt, "latin1", "ASCII", sub="")
  tatt<-gsub("\n","",tatt)
  write(tatt,file="tweetonly.txt",sep = "\n")
  aaa<- system("java -jar ark-tweet-nlp-0.3.2.jar tweetonly.txt", intern = TRUE)
  ###twe<-aaa[5]
  aaaa<-aaa[2:11]
  aaaa
}


#####################################2ndscript#############################
#coordinate<-tweets.df[,c("longitude","latitude","id")]
#location<-coordinate[!is.na(coordinate$longitude),c("longitude","latitude")]

###if not geolocated


###tocheck<-givestocheck(twe)
###saveRDS(givestocheck,"givestocheck.rds")
givestochecklist<-function(aaaa){
  givestocheck<-readRDS("givestocheck.rds")
tochecklist<-(lapply(aaaa,givestocheck))
tochecklist
}
####saveRDS(distance,"distance.rds")

givesddfflist<-function(tochecklist){
  distance<-readRDS("distance.rds")
  #####ddff<-distance(tocheck)
  ddfflist<-lapply(tochecklist,distance)
  ddfflist
}
####saveRDS(giveslistwithcoordinate,"giveslistwithcoordinate.rds")

givesddff.list<-function(ddfflist){
  giveslistwithcoordinate<-readRDS("giveslistwithcoordinate.rds")
  ######333ddff.li<-giveslistwithcoordinate(ddff)
  ddff.listkilist<-lapply(ddfflist,giveslistwithcoordinate)
  ddff.listkilist
}

####saveRDS(plotthelist,"plotthelist.rds")

givesoutputss<-function(ddff.listkilist){
  plotthelist<-readRDS("plotthelist.rds")
  outputss<-lapply(ddff.listkilist,plotthelist)
  outputss
}

##############3plotthelist(ddff.li)
######333ddff.li<-giveslistwithcoordinate(ddff)

