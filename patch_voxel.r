# Return the voxel list(matrix) of a patch

patch_voxel<-function(patch_size, coor, img)
{
    start_coor<-coor-patch_size
    end_coor<-coor
    return(img[start_coor[1]:end_coor[1], start_coor[2], end_coor[2], start_coor[3], end_coor[3]])
}