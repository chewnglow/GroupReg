find_init<-function()
{
  setwd("/mnt/17b03647-e766-4d10-990c-0b2e5e4a1985/T1Img/temp/")
  max_length<-0
  start<-Sys.time()
  for(i in 1:100)
  {
    img<-antsImageRead(paste("ext", i, ".nii.gz", sep = ""))
    arr<-as.array(img)
    len<-length(arr)
    if(len>max_length)
      max_length<-len
    print(paste("Finding the largest length: obj ", i))
  }
  mid<-Sys.time()
  print(paste("Finding time ", mid-start))
  for(i in 1:100)
  {
    img<-antsImageRead(paste("ext", i, ".nii.gz", sep = ""))
    arr<-as.array(img)
    length(arr)<-max_length
    arr[is.na(arr)]<-0
    if(i == 1)
      result<-arr
    else
      result<-rbind(result,arr)
    print(paste("Binding arrays, obj ", i))
  }
  bind<-Sys.time()
  print(paste("Binding time ", bind-mid))
  
  print("Calculating distance ...")
  distance<-dist(result)
  distime<-Sys.time()565
  print(paste("Calculating time ", distime-bind))
  return(order(rowSums(distance)))
}
# Return a 100 length vector of distance ranking.




