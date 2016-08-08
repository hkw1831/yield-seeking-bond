red.color <- function(input) {
  return (paste("<font color=\"#FF0000\">", input, "</font>"))
}

as.percentage.display <- function(input) {
  paste(round(input*100,digits=2),"%",sep="")
}

as.percentage <- function(input) {
  return (input * 100)
}
