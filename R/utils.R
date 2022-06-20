is_ggridges_installed = function() {
  isTRUE(requireNamespace("ggridges"))
}

#' @importFrom stats weighted.mean
#' @noRd
weighted_mean = function(x, w, ..., na.rm=F){
  if(na.rm){
    keep = !is.na(x)&!is.na(w)
    w = w[keep]
    x = x[keep]
  }
  weighted.mean(x, w, ..., na.rm=F)
}