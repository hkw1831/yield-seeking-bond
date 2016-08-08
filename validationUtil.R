is.valid <- function(input) {
  if (any(is.na(input)) || any(!is.numeric(input))) {
    return (FALSE)
  } else {
    return (TRUE)
  }
}