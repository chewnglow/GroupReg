cal_weight <- function(M, mean_node, r, iter, MST=NULL)
{
  
  covered_x <- 0
  covered_y <- 0
  covered_z <- 0
  i <- 1
  j <- 1
  k <- 1
  n_x <- 1
  n_y <- 1
  n_z <- 1
  omega <- c()
  k_reached <- F
  patches <- list()
  
  
  for(num in seq(N))
  {
    print(paste("Calculating", num))
    if(num==mean_node){omega <- cbind(omega, rep(0, length(omega_ind)));next;}# fill the No. mean_node column into 0
    if(iter==0)
    # img <- flirt(infile = paste("ext", num), reffile = paste("ext", mean_node), outfile = paste0("mov0-", num), dof = 12)
     img <- readnii(paste0("mov0-", num, ".nii.gz"))
    else
    {
      if(is.null(MST))stop("Empty tree in MST registeration.")
      current_node <- node()
      # MST reg
      #go2leaf
      while(getLeafNode(MST))
      {
        # cal weight
      }
      
      
      
      
      
      img <- antsRegistration(fixed = M, moving = antsImageRead(paste0("mov", iter, "-", num)))
      antsImageWrite(img, paste0("mov", iter+1, "-", num))
    }
    patch <- 1
    omega_coor <- c()
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
          patches <- list.append(patches, list(data = img_patch, x = c(i, i+b), y = c(j, j+b), z = c(k, k+b)))
          d <- parallelDist(rbind(as.vector(img_patch), as.vector(M_patch)))
          omega_ind <- c(omega_ind, exp(-d/r))
          omega_coor <- rbind(omega_coor, c(i+b/2, j+b/2, k+b/2))
          
          # jump to the next patch
          if(k+2*b<size[3])
          {
            n_z <- n_z+1
            k <- k+b
          }
          else if(k!=size[3]-b)
          {
            covered_z <- img[, , (size[3]-b):(k+b)]
            #last_k <- k
            k <- size[3]-b
          }
          else
          {
          #  covered_areas <- list.append(covered_areas, list(
           #   img=(covered_z+img[, , k:(1+2*b+k-size[3])])/2), 
            #  x=i, y=j, z=k:(size[3]-last_k))
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
          #last_j <- j
          j <- size[2]-b
        }
        else 
        {
          #covered_areas <- list.append(covered_areas, list(
          #  img = (covered_y+img[, j:(2*b+j-size[2]+1), ])/2),
          #  x = i, z = k, 
          #)
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
       # covered_areas <- list.append(covered_areas, (covered_x+img[i:(1+2*b+i-size[1]), , ])/2)
        i <- 1
        break
      }
    }
    omega <- cbind(omega, omega_ind)
  }
  
  write.csv(omega, "omega.csv", row.names = F)
  print("Omega has been written to files")
  print("Reforming")
  omega_sum <- apply(omega, 1, "sum")
  omega <- omega/omega_sum
  write.csv(omega, "omega_std.csv", row.names = F)
  print("Reformed omega has been written to files")
  list(omega=omega, patches=patches, omega_coor=omega_coor)
}
