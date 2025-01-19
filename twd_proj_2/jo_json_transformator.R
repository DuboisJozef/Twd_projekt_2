library(rjson)

lok_joz <- fromJSON(file = "data/Os_czasu_jo_raw.json")
lok_joz <- lok_joz$semanticSegments


arr <- c()
j <- 0
for(i in 1:length(lok_joz)){
  if(is.null(lok_joz[[i]]$timelinePath)){
    j <- j+1
    arr[j] <- i
  }
}


lok_joz_new <- lok_joz[arr]

write(toJSON(lok_joz_new), file = "../data/Os_czasu_jo.json")
