BikeshareCity <- setClass("BikeshareCity", slots = c(name="character",
                             delim="character",
                             header="logical",
                             keepCols="numeric",
                             memberTypes="character",
                             timeFormat = "character"))

.cities <- function(){
    WAS = BikeshareCity(name="Washington, DC", delim=",", header=TRUE, keepCols = c(2,3,4,5,6,7), timeFormat="%m/%d/%Y %H:%M", memberTypes=c("Registered","Casual"))
    CHI = BikeshareCity(name="Chicago", delim=",", header=TRUE, keepCols=c(2,3,6,8,4,10), timeFormat="%Y-%m-%d %H:%M", memberTypes=c("Subscriber","Customer"))
    BOS = BikeshareCity(name="Boston", delim=",", header=TRUE, keepCols = c(4,6,5,7,8,9), timeFormat = "%Y-%m-%d %H:%M", memberTypes=c("Registered","Casual"))
    
    list(WAS= WAS, CHI=CHI, BOS=BOS)
}

