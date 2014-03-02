BikeshareData <- setClass("BikeshareData",
         slots = c(location   = "character",
             startTime  = "POSIXt",
             endTime    = "POSIXt",
             startLoc   = "integer",
             endLoc     = "integer",
             bike       = "integer",
             memberType = "integer",
             stationNames = "character"))

as.data.frame.BikeshareData = function(x,row.names, optional,...){data.frame(x@startTime,x@endTime, x@startLoc, x@endLoc, x@bike, x@memberType)}

setValidity("BikeshareData",
            function(object) {
                messages <- character()
                slots <- c("startTime", "endTime", "startLoc", "endLoc","bike","memberType")
                lengths <- sapply(slots,
                   function(what) length(slot(object, what)))
                if(length(unique(lengths))>1)
                    messages <- paste("unequal lengths: ",
                                      paste(slots, lengths, sep =":", collapse = ", "))
                 if(length(messages))
                    messages
                else
                    TRUE
            })
