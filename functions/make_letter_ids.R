make_letter_ids <- function(nids, lsource=letters){
  
  letterblank <- c("", lsource)
  chars <- ceiling(log(nids+1, length(lsource)))
  
  baseletters <- replicate(chars, letterblank, simplify=FALSE)
  lettergrid <- expand.grid(baseletters)
  lettergrid <- lettergrid[,rev(seq_len(ncol(lettergrid)))]
  
  
  all_letters <- unique(apply(as.matrix(lettergrid), 1, paste0, collapse=""))
  all_letters <- all_letters[all_letters != ""]
  
  wanted_letters <- all_letters[seq_len(nids)]
  
  wanted_letters
  
}
