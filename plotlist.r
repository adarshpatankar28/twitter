plotthelist<-function(ddff.list){
  n <- leaflet()
  n <- addTiles(n)
  for(i in 1:length(ddff.list)){
    
    if(!is.data.frame(ddff.list[[i]][[1]])){
      n <- addMarkers(n, lng=77.17170, lat=28.62739, popup="NO LOCATION TO SHOW") 
    }else{
    
    n <- addMarkers(n, lng=ddff.list[[i]][[2]][,1], lat=ddff.list[[i]][[2]][,2], popup=as.character(ddff.list[[i]][[1]]$frm.database))
  }}
  n
}