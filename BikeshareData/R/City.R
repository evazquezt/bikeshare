City <- setClass("City", slots = c(name="character",
                             delim="character",
                             header="logical",
                             keepCols="numeric",
                             memberTypes="character",
                             timeFormat = "character"))

WAS = City(name="Washington, DC", delim=",", header=TRUE, keepCols = c(2,3,4,5,6,7), timeFormat="%m/%d/%Y %H:%M", memberTypes=c("Registered","Casual"))

