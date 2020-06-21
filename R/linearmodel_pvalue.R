#' Title
#' @title Returns p-value of a linear model
#' @description Extracts p-value out of a linear model produced by function lm(); function found somewhere in the Internet
#' @param modelobject
#'
#' @return
#' @export
#'
#' @examples
#' bla
lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=FALSE)
  attributes(p) <- NULL
  return(p)
}
