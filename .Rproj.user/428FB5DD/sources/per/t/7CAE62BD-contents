# Downloader_functions.R
makeURL <- function(condition = c("Used"),
                    make = NULL,
                    model = NULL,
                    yearFrom = NULL,
                    yearTo = NULL,
                    minMileage = NULL,
                    maxMileage = NULL,
                    bodyType = NULL,
                    fuelType = NULL,
                    minEngine = NULL,
                    maxEngine = NULL,
                    minSeats = NULL,
                    maxSeats = NULL,
                    location="OX118NB",
                    distance = 1500,
                    includeWriteoffs = TRUE,
                    onlyWriteoffs = FALSE)
{
  paste0('http://www.autotrader.co.uk/car-search?sort=sponsored',
         '&radius=',distance,
         '&postcode=',location,
         paste0('&onesearchad=',condition),
         if(!is.null(make)){paste0('&make=',make,collapse = "")},
         if(!is.null(model)){paste0('&model=',model,collapse = "")},
         if(!is.null(yearFrom)){paste0('&year-from=',yearFrom,collapse = "")},
         if(!is.null(yearTo)){paste0('&year-to=',yearTo,collapse="")},
         if(!is.null(minMileage)){paste0('&minimum-mileage=',minMileage,collapse = "")},
         if(!is.null(maxMileage)){paste0('&maximum-mileage=',maxMileage,collapse = "")},
         if(!is.null(bodyType)){paste0('&body-type=',bodyType)},
         if(!is.null(fuelType)){paste0('&fuel-type=',fuelType)},
         if(!is.null(minEngine)){paste0('&minimum-badge-engine-size=',minEngine)},
         if(!is.null(maxEngine)){paste0('&maximum-badge-engine-size=',maxEngine)},
         if(!is.null(minSeats)){paste0('&minimum-seats=',minSeats)},
         if(!is.null(maxSeats)){paste0('&maximum-seats=',maxSeats)},
         if(includeWriteoffs){
           paste0('&exclude-writeoff-categories=on')
         }else if(onlyWriteoffs){
           paste0('&only-writeoff-categories=on')
         })
}

getNpages <- function(htmlPage){
  numberOfListings_raw <- html_text(html_nodes(htmlPage,'.js-results-count'))
  numberOfListings_numeric <- as.numeric(gsub(pattern = ",",
                                              replacement = "",
                                              x = unlist(strsplit(numberOfListings_raw,split=" "))[1]))
  nPages <- ceiling(numberOfListings_numeric / 100)
  return(nPages)
}

getFlyoutData <- function(rawInput){
  # rawInput <- flyoutOptions[1]
  rawInput2 <- unlist(strsplit(rawInput,split = "\n"))
  rawInput3 <- 
    unlist(sapply(rawInput2,function(x) {
      splt <- unlist(strsplit(x,split=""))
      if(length(splt)>0){
        spaceIndx <- which(splt==" ")
        toRemove <- which(sapply(1:length(splt),function(x) spaceIndx[x]==x))
        paste0(splt[-toRemove],collapse = "")
      }
    }))
  rawInput4 <- rawInput3[-which(rawInput3=="")]
  rawInput5 <- sapply(rawInput4,function(x) unlist(strsplit(x,split = "\\("))[1])
  textOutput <- sapply(rawInput5,function(x) paste0(head(unlist(strsplit(x,split = "")),-1),collapse = ""))
  countOutput <- 
    sapply(rawInput4,function(x) 
      as.numeric(gsub(pattern = ",",
                      replacement = "",
                      x = gsub(pattern = ")",
                               replacement = "",
                               x = unlist(strsplit(x,split = "\\("))[2]))))
  return(data.frame(text=textOutput,count=countOutput,stringsAsFactors = FALSE))
}

getModelYears <- function(thisMake,thisModel){
  MakeModelPage <- read_html(makeURL(make = thisMake,
                                     model = thisModel))
  yearsRaw <- html_text(html_nodes(MakeModelPage,'.js-year-from'))
  yearsRaw2 <- unlist(strsplit(yearsRaw,split=" "))
  yearsRaw3 <- unlist(sapply(yearsRaw2,function(x) as.numeric(unlist(strsplit(x,split = "\\)")))))
  return(as.vector(sort(yearsRaw3[-which(is.na(yearsRaw3))])))
}

