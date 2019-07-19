library(ANTsRCore)
library(ANTsR)
library(extrantsr)
library(fslr)
library(Rcpp)3686＊
library(RcppParallel)
library(parallelDist)
library(rlist)
library(optrees)
Sys.setenv(ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS = 48)
setwd("/home/sugon/Group_Reg/files")
source("../code/find_distance.R")
# source("../code/createParentTable.r")
source("../code/patch_voxel.r")
source("../code/notlargerthan.R")
source("../code/cal_weight.R")
source("../code/cal_phai.R")
options(digits = 9)


# Node Class
node <- setClass(
  "node",
  slots = c(
    pos = "numeric",
    parent = "numeric",
    child = "vector",
    level = "numeric",
    viewed = "logical"
  ),
  prototype = list(
    pos = 0,
    parent = 0,
    child = vector(),
    level = 0,
    viewed = FALSE
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
t <- 0
# Init delta_r = max distance 2 other images

delta_r<-max(distance[dist_order[1],])

# Init closet mean image
mean_node <- node(pos = dist_order[1])
M_init<-antsImageRead(paste("ext", mean_node@pos, ".nii.gz", sep = ""))    

# Image size
assign("size", dim(M_init), envir = .GlobalEnv)
# patch size
coor <- size
# ?
patch_size <- coor
b <- min(size)
b <- round(b*(1-1/iter))+1
r <- r+delta_r*(1/iter)

# Init
# d <- array()
# d[mean_node@pos] <- 0

# patches: all individual patches matrix values & coordinate range, coor: center coordinate of patches
# patches: 8*99=792 elements, coor: logical patches @ current b&r
omega_list <- cal_weight(M_init, mean_node@pos, r, t)
M <- makeImage(voxval = cal_mean(omega_list$omega, omega_list$patches, omega_list$omega_coor)
          , imagesize = size, origin = antsGetOrigin(M_init), direction = antsGetDirection(M_init), pixeltype = "float", spacing = antsGetSpacing(M_init))
#M <- makeImage(voxval = cal_mean(M, omega_list$omega, omega_list$patches, omega_list$omega_coor)
#               , imagesize = size, origin = antsGetOrigin(M), direction = antsGetDirection(M), pixeltype = "float", spacing = antsGetSpacing(M))
antsImageWrite(M, "M0.nii.gz")

rm("omega_list")


# Iteration
for(t in seq(iter))                         
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
  
  # Cal weight & r        
  omega_list <- cal_weight(M, mean_node@pos, r, t)
  r<-r+delta_r*(t/iter)
  
  
  omega_list <- c()
    for(s in 1:N)                                   
    {
      if(t==0)
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
    
    
  # Compute mean image
  M <- makeImage(voxval = cal_mean(M, omega_list$omega, omega_list$patches, omega_list$omega_coor)
               , imagesize = size, origin = antsGetOrigin(M), direction = antsGetDirection(M), pixeltype = "float", spacing = antsGetSpacing(M))
  
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
