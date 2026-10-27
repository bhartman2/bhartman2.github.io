#' detectionrate
#' 
#' computes the probability of a single cell of a confusion matrix; detection rate itself is the upper left cell
#'
#' @param conf_mat confidence matrix created by yardstick
#' @param cells integer vector cell numbers to report 1=top left, 2=top right, 3=bottom left, 4=bottom right
#'
#' @returns numeric vector of probabilities. default is actual defection rate, probability of cell 1
#' @export
#'
#' @examples
detectionrate = function (conf_mat, cells=1) {
  require(yardstick)
  # require(broom)
  x = conf_mat %>% yardstick::tidy()
  out = x$value/sum(x$value)
  return(out[cells])
}
