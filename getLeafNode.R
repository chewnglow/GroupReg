# leaf node
getLeafNode <- function(tree)
{
  # search those vector only appear once in both 
  vec <- as.vector(tree[, 1:2])
  numbers <- rep(0, max(vec))
  for(i in vec)
    numbers[i] <- numbers[i]+1
  return(which(numbers==1))
}