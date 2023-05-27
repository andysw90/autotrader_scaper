if(nPages<=100){
  modelList[[i]] <- character(0) 
}else{
  carYears_raw <- html_text(html_nodes(carmakePage,'.js-year-to'))
  carYears_raw2 <- unlist(strsplit(carYears_raw,split = "\\)"))
  carYears_raw3 <- strsplit(carYears_raw2,split = "\\(")
  carYears_raw4 <- data.frame(do.call(rbind,carYears_raw3)[-1,],stringsAsFactors = FALSE)
  colnames(carYears_raw4) <- c("year","count")
  carYears_raw4$year <- gsub(pattern = " ",
                             replacement = "",
                             x = carYears_raw4[,1])
  carYears_raw4$count <- as.numeric(gsub(pattern = ",",
                                         replacement = "",
                                         x = carYears_raw4[,2]))
  carYears_raw5 <- carYears_raw4
  keepGoing <- TRUE
  yearBucket <- list()
  tooManyCarsYears <- list()
  counter <- counter2 <- 1
  while((max(carYears_raw5$count)>1000) && keepGoing){
    indx <- tail(which(carYears_raw5$count>1000),n = 1)
    if(indx==length(carYears_raw5$year)){
      tooManyCarsYears[[counter2]] <- carYears_raw5$year[indx]
      carYears_raw5$count <- carYears_raw5$count - carYears_raw5$count[indx]
      carYears_raw5 <- carYears_raw5[-indx,]
      counter2 <- counter2 + 1
    }else{
      yearBucket[[counter]] <- carYears_raw5$year[(indx + 1):length(carYears_raw5$year)]
      counter <- counter + 1
      carYears_raw5$count <- carYears_raw5$count - carYears_raw5$count[indx + 1]
      carYears_raw5 <- carYears_raw5[-((indx + 1):length(carYears_raw5$year)),]
    }
    
  }
  
}