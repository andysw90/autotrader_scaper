library(rvest)

## Get all models
getModels <- function(){
  nominalPage <- read_html(makeURL())
  flyoutOptions <- html_text(html_nodes(nominalPage,'.js-flyout-options'))
  makeCounts <- getFlyoutData(flyoutOptions[1])
  makes <- makeCounts$text
  
  modelList <- vector(mode = "list",length = length(makes))
  for(i in 1:length(makes)){
    thisMake <- makes[8]
    # carmakeURL <- makeURL(make = thisMake)
    # carmakePage <- read_html(carmakeURL)
    nPages <- ceiling(makeCounts$count[i]/100)
    if(nPages>100){
      modelCounts <- getFlyoutData(html_text(html_nodes(carmakePage,'.js-flyout-options'))[2])  
      toSplit <- modelCounts[which(modelCounts$count>1000),]
      modelYears <- sapply(toSplit$text,function(x) getModelYears(thisMake,x))
      
    }
    
    
    
    
  }
}

make <- "Ford"
model <- "Focus"
year <- 2012
URLtoFetch <- makeURL(make = make,model = model,yearFrom = year,yearTo = year,includeWriteoffs = FALSE)
firstPage <- read_html(URLtoFetch)
numberOfListings_raw <- html_text(html_nodes(firstPage,'.js-results-count'))




numberOfListings_numeric <- as.numeric(gsub(pattern = ",",
                                            replacement = "",
                                            x = unlist(strsplit(numberOfListings_raw,split=" "))[1]))
nPages <- ceiling(numberOfListings_numeric / 100)

if(nPages>100){
  stop(paste("Too many listings for",make,model,"(total is",numberOfListings_numeric,")."))
}else{
  cat(nPages)
}

nPages <- 100
dataOut <- vector(mode="list",length = nPages)
for(i in 1:nPages){
  tpage<-read_html(paste0(URLtoFetch,i))
  dataOut_raw <- data.frame(model = html_text(html_nodes(tpage,'.listing-title.title-wrap'))[2:11],
                            year = html_text(html_nodes(tpage,'.listing-key-specs li:nth-child(1)'))[2:11],
                            miles = html_text(html_nodes(tpage,'.listing-key-specs li:nth-child(3)'))[2:11],
                            engine_size = html_text(html_nodes(tpage,'.listing-key-specs li:nth-child(4)'))[2:11],
                            bhp = html_text(html_nodes(tpage,'.listing-key-specs li:nth-child(5)'))[2:11],
                            transmission = html_text(html_nodes(tpage,'.listing-key-specs li:nth-child(6)'))[2:11],
                            fuel = html_text(html_nodes(tpage,'.listing-key-specs li:nth-child(7)'))[2:11],
                            seller = html_text(html_nodes(tpage,'.seller-type'))[2:11],
                            price = html_text(html_nodes(tpage,'.vehicle-price'))[2:11],
                            # description = html_text(html_nodes(tpage,'.listing-description'))[2:11],
                            stringsAsFactors = FALSE)
  dataOut_raw2 <- dataOut_raw
  keyWords <- list(model = "",
                   year = "reg",
                   miles = "miles",
                   engine_size = "L",
                   bhp = "bhp",
                   transmission = c("Manual","Automatic","auto"),
                   fuel = c("Petrol","Diesel","hybrid"),
                   seller = "seller",
                   price = "£")
  
  for(j in 1:length(dataOut_raw2)){
    thisKeyWord <- keyWords[[j]]
    if(thisKeyWord[1] != ""){
      correctEntries <- unlist(lapply(thisKeyWord,function(y) grep(y,dataOut_raw2[,j]))) + sum(is.na(dataOut_raw2[,j]))
      incorrectEntries <- setdiff(c(1:length(dataOut_raw$model)),correctEntries)
      if(length(incorrectEntries)>0){
        for(k in incorrectEntries){
          # k <- incorrectEntries[1]
          correctIndx <- unlist(lapply(thisKeyWord,function(y) grep(y,dataOut_raw[k,])))
          if(length(correctIndx)==1){
            dataOut_raw2[k,j] <- dataOut_raw[k,correctIndx]
          }else{
            dataOut_raw2[k,j] <- NA
          }
        }  
      }
    }
  }
  dataOut[[i]] <- dataOut_raw2
}
dataOut_tmp <- do.call(rbind,dataOut)
yearInfo <- 
  t(sapply(dataOut_tmp$year,function(x) {
    if(!is.na(x)){
      year <- as.numeric(unlist(strsplit(x,split=" "))[1])
      plate <- gsub(x = unlist(strsplit(x,split=" "))[2],
                    pattern = "\\(",
                    replacement = "")
    }else{
      year <- NA
      plate <- NA
    }
    return(data.frame(year,plate,stringsAsFactors = FALSE))
  }))
dataOut_tmp$plate <- yearInfo[,1]
dataOut_tmp$year <- yearInfo[,2]
modelInfo <- t(sapply(dataOut_tmp$model,function(x) {
  x <- gsub(pattern = "\n",replacement = "",x = x)
  splt <- unlist(strsplit(x,split = " "))
  splt <- splt[-which(splt=="")]
  doors <- as.numeric(gsub(x = splt[grep("dr",splt)],
                           pattern = "dr",
                           replacement = ""))
  trim_tmp <- splt[-c(1,2,3,length(splt))]
  trim <- paste0(trim_tmp[1:(min(3,length(trim_tmp)))],collapse=" ")
  return(data.frame(make = splt[1],
                    model = splt[2],
                    doors = if(length(doors)>0){doors}else{"Unknown"},
                    trim = if(length(trim)>0){trim}else{"Unknown"},
                    stringsAsFactors = FALSE))
}))
dataOut_tmp$make <- modelInfo[,1]
dataOut_tmp$modelPure <- modelInfo[,2]
dataOut_tmp$doors <- modelInfo[,3]
dataOut_tmp$trim <- modelInfo[,4]
dataOut_tmp$miles <- as.numeric(sapply(dataOut_tmp$miles,function(x) gsub(pattern = ",",
                                                                          replacement = "",
                                                                          x = gsub(pattern = " miles",
                                                                                   replacement = "",
                                                                                   x = x))))
dataOut_tmp$seller <- sapply(dataOut_tmp$seller,function(x){
  if(length(grep("Private",x))>0){
    "Private"
  }else if(length(grep("Trade",x))>0){
    "Trade"
  }else{
    NA
  }
})

dataOut_final <- data.frame(Make = unlist(dataOut_tmp$make),
                            Model = unlist(dataOut_tmp$modelPure),
                            Trim = unlist(dataOut_tmp$trim),
                            Plate = unlist(dataOut_tmp$year),
                            Year = unlist(dataOut_tmp$plate),
                            Engine = unlist(dataOut_tmp$engine_size),
                            Fuel = unlist(dataOut_tmp$fuel),
                            BHP = unlist(dataOut_tmp$bhp),
                            Mileage = unlist(dataOut_tmp$miles),
                            Doors = unlist(dataOut_tmp$doors),
                            Seller = unlist(dataOut_tmp$seller))



## Next things to do:
# - Triangulate the location. Get distance from 3 points in corners of country, do trig to find location.
# - Add catcher on batch downloader in case it fails
# - This will only work for first 100 pages. Need to rewrite the downloader to loop through settings/search options if want to get massive datasets

