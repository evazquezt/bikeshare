BikeshareData <- setClass("BikeshareData",
         slots = c(location   = "character",
             startTime  = "POSIXt",
             endTime    = "POSIXt",
             startLoc   = "integer",
             endLoc     = "integer",
             bike       = "integer",
             memberType = "integer",
             stations = "list"))

as.data.frame.BikeshareData = function(x,row.names, optional,...){
    df=data.frame(x@startTime,x@endTime, x@startLoc, x@endLoc, x@bike, x@memberType)
    colnames(df) = c("startTime","endTime","startLoc","endLoc","bike","memberType")
    df
}

setValidity("BikeshareData",
            function(object) {
                messages <- character()
                slots <- c("startTime", "endTime", "startLoc", "endLoc","bike","memberType")
                lengths <- sapply(slots,
                   function(what) length(slot(object, what)))
                if(length(unique(lengths))>1){
                    messages <- paste("unequal lengths: ",
                                      paste(slots, lengths, sep =":", collapse = ", "))
                }
                if(length(stations) == 0){
                    messages <- "No bikeshare stations"
                }
                else if(sum(sapply(stations, function(x){class(x) != "BikeshareStation"})) > 0){
                    mesages <- "Bikeshare stations must be of class BikeshareStation"
                }   
                if(length(messages)){
                    messages
                }
                else{
                    TRUE
                }
            })
