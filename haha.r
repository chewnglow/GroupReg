haha <- function(lv)
{
  l <- list.search(l$vec, identical(.@level, lv))
  vec <- vector()
  par <- vec
  for(i in 1:length(l))
  {
    vec <- c(vec, l[[i]]@pos)
    par <- c(par, l[[i]]@parent)
  }
  list(par=par, vec=vec)
}