cal_weight <- function(img, M)
{
  for(num in seq(N))
  {
    print(paste("Calculating", num))
    if(num==mean_node@pos)next
    # img <- flirt(infile = paste("ext", num), reffile = paste("ext", mean_node@pos), outfile = paste0("mov0-", num), dof = 12)
    img <- readnii(paste0("mov0-", num, ".nii.gz"))
    patch <- 1
    omega_ind <- c()
    
    while(T)
    {
      while(T)
      {
        while(T)
        {
          print(paste("> Calculating patch", i, j, k))
          x <- i:(i+b)
          y <- j:(j+b)
          z <- k:(k+b)
          img_patch <- img[x, y, z]
          M_patch <- M[x, y, z]
          patches <- list.append(patches, list(img_patch, x, y, z))
          d <- parallelDist(rbind(as.vector(img_patch), as.vector(M_patch)))
          omega_ind <- c(omega_ind, d)
          
          if(k+2*b<size[3])
          {
            n_z <- n_z+1
            k <- k+b
          }
          else if(k!=size[3]-b)
          {
            covered_z <- img[, , (size[3]-b):(k+b)]
            last_k <- k
            k <- size[3]-b
          }
          else
          {
            covered_areas <- list.append(covered_areas, list(
              img=(covered_z+img[, , k:(1+2*b+k-size[3])])/2), 
              x=i, y=j, z=k:(size[3]-last_k))
            k <- 1
            break
          }
        }
        if(j+2*b<size[2])
        {
          n_y <- n_y+1
          j <- j+b
        }
        else if(j!=size[2]-b)
        {
          covered_y <- img[, (size[2]-b):(j+b), ]
          last_j <- j
          j <- size[2]-b
        }
        else 
        {
          covered_areas <- list.append(covered_areas, list(
            img = (covered_y+img[, j:(2*b+j-size[2]+1), ])/2),
            x = i, z = k, 
          )
          j <- 1
          break
        }
      }
      if(i+2*b<size[1])
      {
        n_x <- n_x+1
        i <- i+b
      }
      else if(i!=size[1]-b)
      {
        covered_x <- img[(size[1]-b):(i+b), , ]
        i <- size[1]-b
      }
      else 
      {
        covered_areas <- list.append(covered_areas, (covered_x+img[i:(1+2*b+i-size[1]), , ])/2)
        i <- 1
        break
      }
    }
    omega <- cbind(omega, omega_ind)
  }
}

return(omega)