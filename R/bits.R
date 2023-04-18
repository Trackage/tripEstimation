"bits" <-
function(object,bit) {
  out <- (object %/% (2^bit)) %% 2
  if (!is.null(dim(object))) dim(out) <- dim(object)
  out
}

"bits<-" <-
function(object,bit,value) {
  mask <- 2^bit
  object <- object+(value - ((object %/% mask) %% 2))*mask
  object
}


