#' freqTable
#'
#' Make Frequency Table
#' @param x vector. numbers to make frequency table.
#' @return result table.
#' @export
#' @examples
#' freqTable(x = x.vec)

freqTable <- function(x)
{
  x.num <- length(x)
  x.classNum <- round(x.num^(1/3))
  x.classInterval <- round((max(x)-min(x)) / x.classNum)
  
  x.floor <- floor(min(x))
  rest <- ceiling(max(x))%%x.classInterval
  supplement <- x.classInterval - rest
  x.ceiling <- ifelse(rest==0, ceiling(max(x)), ceiling(max(x))+supplement)
  
  x.cut <- cut(x, breaks=seq(x.floor, x.ceiling, by=x.classInterval), right = FALSE)
  x.levels <- gsub(",", "~", levels(x.cut))
  x.cut <- cut(x, breaks=seq(x.floor, x.ceiling, by=x.classInterval), right = FALSE, labels = x.levels)
  
  result <- table(x.cut)
  
  return(result)
}