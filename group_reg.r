setwd("/mnt/17b03647-e766-4d10-990c-0b2e5e4a1985/T1Img/temp/")
avg_1<-0
chosen_num<-95            #95 is chosen as ref
iter<-20
sum<-0
batch_size<-29


for(i in seq(batch_size))
{
  if(i != chosen_num)
  {
    res[i]<-flirt(infile = paste("ext", i, sep = ""), reffile = paste("ext", chosen_num, sep = ""), dof = 12, outfile = paste("/home/sugon/group_reg/", i, sep = ""))
    sum<-sum+res[i]@.Data
    res[i]<-oro2ants(res[i])
    
  }
}

for(j in seq(iter))
{
  for(i in seq(batch_size))
  {
    if(i != chosen_num)
    {
      res_n<-antsRegistration(moving = paste("/home/sugon/group_reg/iter", j-1, "_", i, sep = ""),
                 fixed = paste("ext", chosen_num, sep = ""), dof = 12,
                 outfile = paste("/home/sugon/group_reg/", i, sep = ""),
                 typeofTransform = "SyNOnly")
      antsImageWrite(res, paste("/home/sugon/group_reg/", i, ".nii", sep = ""))
      sum<-sum+res@.Data
    }
  }
}
  
  avg_0<-sum/99
  diff=avg_0-avg_1
  print(paste("Iter", i, ":", diff, sep = ""))
  avg_1<-avg_0
}