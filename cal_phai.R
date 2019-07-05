# Calculate patches in one pic
cal_mean <- function(omega, patches, centers)
{
  mean <- array(dim=size, data = 0)
  # for all images
  for(n in seq(N))
  {
    phai <- c()
    # for all patches in an image
    for(i in seq(length(centers[,1])))#num of patches
    {
      center <- centers[i,]
      center_in <- c()
      # cal centers in this patch
      for(j in seq(length(centers[,1])))
        if(((centers[j,1]>=center[1]-b/2)&(centers[j,1]<center[1]+b/2)) & 
           ((centers[j,2]>=center[2]-b/2)&(centers[j,2]<center[2]+b/2)) & 
           ((centers[j,3]>=center[3]-b/2)&(centers[j,3]<center[3]+b/2)) &
           j!=n)
          center_in <- c(center_in, j)#the inside patch number
      phai <- c(phai, sum(omega[center_in, j])/length(center_in))
      print(paste("calculating phai of patch", i, "of image", n))
    }
    print(paste("constucting mean image at position x:", patches[[n]]$x[1], "-", patches[[n]]$x[2],
                                                 ", y:", patches[[n]]$y[1], "-", patches[[n]]$y[2], 
                                                 ", z:", patches[[n]]$z[1], "-", patches[[n]]$z[2]))
    
    mean[patches[[n]]$x[1]:patches[[n]]$x[2], patches[[n]]$y[1]:patches[[n]]$y[2], patches[[n]]$z[1]:patches[[n]]$z[2]] <- 
      (phai * patches[[n]]$data+
         mean[patches[[n]]$x[1]:patches[[n]]$x[2], patches[[n]]$y[1]:patches[[n]]$y[2], patches[[n]]$z[1]:patches[[n]]$z[2]])/2
  }
  mean
}