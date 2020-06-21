#' Plotting Richness Peak
#'
#' @param data
#' @param X
#' @param Y
#'
#' @return
#' @export
#'
#' @examples
peakplot <- function(data, X, Y){
  ggplot(data, aes(y=Y, x=X, col=datasettotsimpl))+
    geom_point()+
    xlab(expression(D[alpha][max](m)))+
    ylab(expression(R[alpha][max](N)))+
    theme(legend.position = c(0.9, 0.2),legend.title=element_blank())+
    scale_x_reverse()
}
