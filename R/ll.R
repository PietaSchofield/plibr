#' ll list full info on objects
#'
#' This is a steal however it probably needs a rewrite in the plyr world
#'
#' @param name see ls()
#' @param pos see ls()
#' @param pattern see ls()
#' @param allNames see ls()
#' @param envir see ls()
#' @param orderBy field to orderby
#' @param decreasing reverse direction
#' @param head restrict number
#' @param n number to list
#'
#' @export
ll <- function (name=NULL, pos = 1L, envir = as.environment(pos),allNames = FALSE,
                pattern=".*", order.by=NULL, decreasing=FALSE, head=FALSE, n=100) {

    napply <- function(names, fn) sapply(names, function(x) fn(get(x, pos = pos)))

    objs <- ls(pos = pos, pattern = pattern)
    if(length(objs)>0){
      obj.class <- napply(objs, function(x) as.character(class(x))[1])
      obj.mode <- napply(objs, mode)
      obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
      obj.size <- napply(objs, object.size)
      obj.prettysize <- sapply(obj.size, function(r) prettyNum(r, big.mark = ",") )
      obj.dim <- t(napply(objs, function(x) as.numeric(dim(x))[1:2]))

      vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
      obj.dim[vec, 1] <- napply(objs, length)[vec]

      out <- data.frame(obj.type, obj.size,obj.prettysize, obj.dim)
      names(out) <- c("Type", "Size", "PrettySize", "Rows", "Columns")

      if (!missing(order.by))
        out <- out[order(out[[order.by]], decreasing=decreasing), ]
        out <- out[c("Type", "PrettySize", "Rows", "Columns")]
        names(out) <- c("Type", "Size", "Rows", "Columns")
      if (head)
        out <- head(out, n)
    }else{
      out=objs
    }
    out
}
