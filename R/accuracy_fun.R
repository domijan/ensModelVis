#' Title
#'
#' @param truth
#' @param pred
#'
#' @return
#' @export
#'
#' @examples
accuracy_fun <- function(truth, pred){
  sum(diag(table(truth, pred)))/sum(table(truth, pred))
}
