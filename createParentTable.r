createParentTable <- function(tree)
{
  rootnd <- node(pos = N+1, level = 1)
  nodevec <- c(rootnd)
  parentsvec <- c(rootnd)
  lv <- 1
  n <- N
  childrenvec <- list()
  while(n-1)
  {
    for(parent in parentsvec)
    {
      children <- getChild(tree, parent@pos)
      children <- children[which(children!=parent@parent)]
      for(child in children)
      {
        n <- n-1
        childrenvec <- c(childrenvec, node(pos = child, parent = parent@pos, level = lv+1))
      }
    }
    nodevec <- c(nodevec, childrenvec)
    parentsvec <- childrenvec
    childrenvec <- vector()
    lv <- lv+1
  }
  return(nodevec)
}
# children <- c(29, 100)
# for(child in children)
# {
#   n <- n-1
#   temp <- node(pos = child, parent = 93, level = lv+1)
#   childrenvec <- append(childrenvec, temp)
#   1+1
# }