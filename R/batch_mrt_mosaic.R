# The basic work-flow may follow the steps bellow:
# (1) create a .hdf list
# (2) use mosaicModisHDF to mosaic hdf files into a large hdf file, depending on the time series
# (3) use reproject_modis_hdf to reproject the mosaiced hdf files to the desired raster files (e.g. tif files)
# (4) rename the tif files as you want.

# wrap the mosaic and reproject function
# Please note:
# hdf_date represents the study period, which is expected to have the same length as the final tif files.
# raw_hdf_path is the path where the original MOD16 HDF files are stored.
# mosaic_hdf_path is the path to store the mosaic HDF files

#' Mosiac a series of MODSI satellite images using MRT.
#' @param hdf_date The date of the MODIS satellite image.
#' Can be obtained from the \code{\link{get_hdf_dates}} function.
#' @param raw_hdf_path The path of the downloaded hdf files.
#' @param mosaic_hdf_path The path to store the mosaiced hdf files.
#' @param bands_subset A character vector indicating the spectral subset of the input hdf file.
#' Pass to \code{\link{mosaic_modis_hdf}} function.
#' @param delete Logical. If TRUE the original hdf files will be deleted after mosaicing. Default is FALSE.
#' Pass to \code{\link{mosaic_modis_hdf}} function.
#' @description Runs \code{\link{mosaic_modis_hdf}} on a batch of files.
#' It internally use MODIS Reproject Tool (MRT) software to mosaic the downloaded images.
#' @note To have the functionality for Mosaic and reprojecting of the images,
#' you need to first install MRT software (\url{https://lpdaac.usgs.gov/tools/modis_reprojection_tool})
#' on your machine, and introduce its' path through the
#' mrt_path argument.
#' @seealso \code{\link{mosaic_modis_hdf}} \code{\link{reproject_modis_hdf}} \code{\link{get_hdf_dates}}
#' @export
batch_mrt_mosaic <- function(hdf_date,
                             raw_hdf_path,
                             mosaic_hdf_path,
                             bands_subset = "",
                             delete = FALSE){
  temp.hdf.list <- list.files(raw_hdf_path,
                              pattern = hdf_date,
                              full.names = T)
  mosaic_modis_hdf(hdf_fpnames = temp.hdf.list,
                   dst_fpname = file.path(mosaic_hdf_path, paste0(hdf_date, ".hdf")),
                   bands_subset = bands_subset,
                   delete = delete)
}
