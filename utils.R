prefix_columns <- function(df, prefix, ignore_first=1){
  names(df) <- paste0(c(rep("", ignore_first), rep(prefix, ncol(df)-ignore_first)),names(df))
  df
}

suffix_columns <- function(df, suffix, ignore_first=1){
  names(df) <- paste0(names(df), c(rep("", ignore_first), rep(suffix, ncol(df)-ignore_first)))
  df
}

rep.row <- function(x,n){
  df <- data.frame(matrix(rep(x,each=n),nrow=n))
  names(df) <- names(x)
  df
}

rep.col <- function(x,n){
  df <- data.frame(matrix(rep(x,each=n), ncol=n, byrow=TRUE))
  names(df) <- names(x)
  df
}