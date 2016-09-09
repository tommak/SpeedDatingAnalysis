library(ggplot2)
library(reshape)

setwd("Documents/Analysis/Speed Dating Analysis")
data <- read.csv("data/Speed Dating Data.csv")

keep <- c("attr1_1", "sinc1_1", "intel1_1", 
          "fun1_1", "amb1_1", "shar1_1",
          "attr2_1", "sinc2_1", "intel2_1", 
          "fun2_1", "amb2_1", "shar2_1")

agg_fun <- function(arr) { return (arr[1]) }
pref <- melt (aggregate(data[,keep], by=data[c("iid", "gender", "age")], 
                        FUN = agg_fun),
              id = c("iid", "gender", "age"))

get_attrib <- function (attrib) {
  atstr <- as.character(attrib)
  patt <- "[0-9]_[0-9]$"
  pos <- regexpr(patt, atstr)[1]
  if (pos >= 1)
    return (substr(atstr, 1, pos-1))
  else 
    return (NA)
}  

get_attrib_type <- function (attrib) {
  atstr <- as.character(attrib)
  patt <- "[0-9]_[0-9]$"
  pos <- regexpr(patt, atstr)[1]
  if (pos >= 1)
    return (substr(atstr, pos, nchar(atstr)))
  else
    return (NA)
  
}  

pref$attrib <- get_attrib(pref$variable)
pref$attrib_type <- get_attrib_type(pref$variable)

