library(ANTsRCore)
library(ANTsR)
library(extrantsr)
library(fslr)
library(Rcpp)
library(RcppParallel)
library(parallelDist)
Sys.setenv(ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS = 64)
setwd("/home/sugon/Group_Reg/files")
source("../code/find_distance.R")
# source("../code/createParentTable.r")
source("../code/patch_voxel.r")
source("../code/notlargerthan.R")
source("../code/cal_weight.R")


# Node Class
node <- setClass(
  "node",
  slots = c(
    pos = "numeric",
    parent = "numeric",
    child = "vector",
    level = "numeric"
  ),
  prototype = list(
    pos = 0,
    parent = 0,
    child = vector(),
    level = 0
  )
)

# Init Params
r<-1
iter<-20
# Bottomright point coor
assign("N", 100, envir = .GlobalEnv)

# Img index of rank of dist to others, increasing. 
#distance <- find_distance(0)
distance <- read.csv("distance.csv")
d_sum <- rowSums(distance)
dist_order <- order(d_sum)

# Init delta_r = max distance 2 other images

delta_r<-max(distance[dist_order[1],])

# Init closet mean image
mean_node <- node(pos = dist_order[1])
M<-antsImageRead(paste("ext", mean_node@pos, ".nii.gz", sep = ""))    

size<-dim(M)
coor <- size
patch_size <- coor
b <- min(size)
b <- round(b*(1-1/iter))+1
r <- r+delta_r*(1/iter)


# Init
# d <- array()
# d[mean_node@pos] <- 0
patches <- list(img=NULL, x=array(), y=array(), z=array())
area_num <- 1
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

omega <- cal_weight(img, M)

rm(img);rm(img_patch);rm(M_patch)
write.csv(omega, "omega.csv", row.names = F)
patch_num <- dim(omega)[1]
omega_sum <- apply(omega, 1, sum)
omega <- omega/omega_sum
write.csv(omega, "omega_std.csv", row.names = F)
covered <- c(n_z+1, n_z+n_y+2, n_x)

# calculate phai
for(i in seq(N))
{
  
}

# Construct mean image
M_data <- array(0, dim = size)
for(i in patches)
  M_data[patches$x, patches$y, patches$z] <- (M_data[patches$x, patches$y, patches$z]+patches[[i]]$img)/2



for(i in seq(N))
{
  if(i == mean_node@pos)next;
  reged <- antsImageRead(paste0("mov0-", i, ".nii.gz"))
  omega[i] <- omega[i]/omega_sum
  ksai[i] <- omega[i]/prod(patch_size)
  M_data <- M_data + ksai[i]*reged[]
  print(paste(i, "merged"))
}
M1 <- makeImage(voxval = M_data/N, imagesize = dim(M), origin = antsGetOrigin(M), direction = antsGetDirection(M), pixeltype = "float", spacing = antsGetSpacing(M))
M <- M1
rm("M1")
antsImageWrite(M, "M0.nii.gz")
M_data <- 0


for(i in seq(100))
{
  if(i==93)next
  img <- readnii(paste0("mov0-", i))
  d[i] <- as.numeric(parallelDist(rbind(as.vector(img[]), mean_vec)))
  omega[i] <- exp(-d[i]/r)
  print(paste("finish", i))
}

# Iteration
for(t in 1:iter)                         
{
  # t-1 
  # Build reg MST
  vec<-vector()
  for(a in 1:N)
    for(b in (a+1):(N+1))
      vec<-rbind(vec, c(a,b))
  distance <- find_distance(t, M)
  distance[lower.tri(distance)]=0
  vec<-cbind(vec, as.vector(distance[which(distance!=0)]))
  MST<-msTreeKruskal(1:101, vec)
  tree <- MST$tree.arcs
  
    # Weight for each img                 
    for(s in 1:N)                                   
    {
      if(t==1)
        img<-antsImageRead(paste("ext", s, ".nii.gz", sep = ""))
      else
        img<-antsImageRead(paste("mov", t-1, "-", s, ".nii.gz", sep = "")) 
      d_sum<-0
      # Patches
      patch_num<-0
      total_patch<-(255-coor[1])*(255-coor[2])*(255-coor[3])
      omega<-matrix(0, nrow = total_patch, ncol = s)
      patch_size <- coor-1 #patch size is a vector
      for(x in coor[1]:255)
          for(y in coor[2]:255)
              for(z in coor[3]:255)
              {
                  mov_patch<-patch_voxel(patch_size, coor, img)
                  fix_patch<-patch_voxel(patch_size, coor, M)
                  d <- parallelDist(as.vector(mov_patch), as.vector(fix_patch))
                  omega[patch_num, s]<-exp(-d/r)
                  d <- as.matrix(d)
                  patch_num<-patch_num+1
              }
    }
    for(patch_num in seq(total_patch))
        omega_sum[patch_num]<-sum(omega[patch_num, seq(N)])
    for(s in seq(N)) 
        for(patch_num in seq(total_patch))
            omega[patch_num, s]<-omega[patch_num, s]/omega_sum[patch_num]
    r<-r+delta_r*(t/iter)
    
    # Compute mean image
    M_data<-M_data+sum(((sum(omega[seq(total_patch), s]))/prod(coor))*img[])
    M<-makeImage(voxval = M_data, origin = antsGetOrigin(M), direction = antsGetDirection(M), pixeltype = float, spacing = antsGetSpacing(M))
    antsImageWrite(M, paste("mean", t, ".nii.gz", sep = ""))
      
    
    # MST BFS registration
    while(T)
    {
      tar@child <- getChild(mean_node)
      for(i in tar@child)
      { 
        
      }
    }
    
    # Warp
    
    
    # go to leaf
    while(tar)
    {
      searched[tar] <- TRUE
      child <- getChild(tree, tar)
      parent <- NULL
      # Go end left
      while (child) {
        parent <- tar
        tar <- child
        child <- getChild(tree, tar)
      }
      
      # Reg
      mov <- antsImageRead(paste("mov", iter-1, "-", tar, ".nii.gz", sep = ""))
      fix <- antsImageRead(paste("mov", iter-1, "-", parent, ".nii.gz", sep = ""))
      # Set as variable to directly reach
      antsImageWrite(antsRegistration(moving = mov, fixed = fix, typeofTransform = "SyNOnly"))

    } 
    
    # Update M
    
    
}
