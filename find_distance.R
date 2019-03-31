find_distance<-function(t, M=NULL)
{
  setwd("/home/sugon/Group_Reg/files/")
  result <- c()
  # Calculate distance between each otherz
  if(t==0)
  {
    for(i in seq(N))
    {
      img<-antsImageRead(paste("ext", i, ".nii.gz", sep = ""))
      result<-rbind(result, as.array(img))
    }
  }
  else
 { 
    for(i in seq(N))
    {
      img<-antsImageRead(paste("mov", t-1, "-", s, ".nii.gz", sep = ""))
      result<-c(result, as.array(img))
    }
    result <- c(result, as.array(M))
  }

  #distance<-pdist(result)
  distance<-as.matrix(parallelDist(result, threads = 64))

  #calculate distance sum
  return(distance)
  #return(order(sum))
}
# Return a 100 length vector of image index distance ranking.