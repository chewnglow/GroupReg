  path<-"/mnt/17b03647-e766-4d10-990c-0b2e5e4a1985/T1Img/temp/"
  T1path<-"/mnt/17b03647-e766-4d10-990c-0b2e5e4a1985/T1Img/"
  read<-function(i)
  {
    setwd(paste(T1path, 270000+i, sep = ""))
    files<-list.files()
    varNames<-c()
    fileName<-grep("^20+", files, value = TRUE)
    antsImageRead(filename = fileName)
  }
  
  for(i in seq(100))
  {
    origin<-read(i)
    antsImageWrite(n4BiasFieldCorrection(origin, mask = getMask(origin, lowThresh 
                                                                = 670)), 
                   paste(path, "corr", i, ".nii", sep = ""))
    ext<-fslbet(infile = paste(path, "corr", i, ".nii", sep = ""), 
                outfile = paste(path, "ext", i, ".nii.gz", sep = ""), 
                betcmd = "bet", opts = "-R -f 0.3",
                retimg = FALSE)
  }  
  
  for(i in seq(100))
  {
    origin<-read(i)
    antsImageWrite(n4BiasFieldCorrection(origin, mask = getMask(origin, lowThresh = 970)), 
                   paste(path, "corr", i, ".nii", sep = ""))
                     ext<-fslbet(infile = paste(path, "corr", i, ".nii", sep = ""), 
                                 outfile = paste(path, "ext_", i, ".nii.gz", sep = ""), 
                                 betcmd = "bet", opts = "-R -f 0.35",
                                 retimg = FALSE)
                   
  }
  
