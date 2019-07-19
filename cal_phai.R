# Calculate patches in one pic
cal_mean <- function(omega, patches, centers)
{
  mean_empty <- array(dim=size, data = 0)
  mean <- mean_empty
  # for all images
  for(n in seq(N))
  {
    # cal appending times
    mean_added <- mean_empty
    phai <- c()
    # for all patches in an image
    print(paste0("Calculating image ", n))
    
    for(i in seq(length(centers[,1])))#num of patches
    {
      center <- centers[i,]
      center_in <- c()
      centers_coor <- c()
      # cal centers in this patch
      for(j in seq(length(centers[,1])))
        if(((centers[j,1]>=center[1]-b/2)&(centers[j,1]<center[1]+b/2)) & 
           ((centers[j,2]>=center[2]-b/2)&(centers[j,2]<center[2]+b/2)) & 
           ((centers[j,3]>=center[3]-b/2)&(centers[j,3]<center[3]+b/2)) &
           j!=n){
          center_in <- c(center_in, j)#the inside patch number
          centers_coor <- c(centers_coor, c(centers[j, 1], centers[j, 2], centers[j, 3]))
        }     
      # phai set
      phai <- c(phai, sum(omega[center_in, n])/length(center_in))
      # print(paste("calculating phai of patch", i, "of image", n))
      
      # construct mean image patches
      x_range <- patches[[i]]$x[1]:patches[[i]]$x[2]
      y_range <- patches[[i]]$y[1]:patches[[i]]$y[2]
      z_range <- patches[[i]]$z[1]:patches[[i]]$z[2]
      
      print(paste("constucting mean image at position x:", head(x_range, 1), "-", tail(x_range, 1),
                  ", y:", head(y_range, 1), "-", tail(y_range, 1),
                  ", z:", head(z_range, 1), "-", tail(z_range, 1)))
      
      mean_empty[x_range, y_range, z_range] <- mean_empty[x_range, y_range, z_range]+sum(omega[center_in, n])/length(center_in)*patches[[i]]$data
      mean_added[x_range, y_range, z_range] <- mean_added[x_range, y_range, z_range] + 1
    }
      # print(paste("constucting mean image at position x:", head(x_range, 1), "-", tail(x_range, 1),
      #                                            ", y:", head(y_range, 1), "-", tail(y_range, 1),
      #                                            ", z:", head(z_range, 1), "-", tail(z_range, 1)))
      mean <- mean+mean_empty/mean_added
      mean_empty <- array(dim = size, data = 0)
      mean_added <- mean_empty
    
    # mean[patches[[n]]$x[1]:patches[[n]]$x[2], patches[[n]]$y[1]:patches[[n]]$y[2], patches[[n]]$z[1]:patches[[n]]$z[2]] <- 
    #   (phai[n] * patches[[n]]$data+
    #      mean[patches[[n]]$x[1]:patches[[n]]$x[2], patches[[n]]$y[1]:patches[[n]]$y[2], patches[[n]]$z[1]:patches[[n]]$z[2]])/2
    
    
  }
  mean
}