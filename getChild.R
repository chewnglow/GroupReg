getChild<-function(tree, pos)
{
  temp <- as.vector(tree[which(tree[,1]==pos|tree[,2]==pos),1:2])
  child <- temp[which(temp[]!=pos)]
  return(child)
}