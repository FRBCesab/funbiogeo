# Internals

stop_condition <- function(x, ...) {
  
  if (x) {
    stop(paste0(...), .call = FALSE)
  }
  
  invisible(x)
}

seq_nrow <- function(x) seq_len(nrow(x))

seq_ncol <- function(x) seq_len(ncol(x))
