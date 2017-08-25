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

#' Reproject a series of MODSI satellite images using MRT.
#' @param hdf_date The date of the MODIS satellite image using MRT.
#' Can be obtained from the \code{\link{get_hdf_dates}} function.
#' @param mosaic_hdf_path The path to store the mosaiced hdf files.
#' @param mosaic_tif_path The path to stroe the mosaiced tif files.
#' @param bands_subset A character vector indicating the spectral subset of the input hdf file.
#' Pass to \code{\link{reproject_modis_hdf}} function.
#' @param proj_type The projection type used. Default is "UTM". Pass to \code{\link{reproject_modis_hdf}} function.
#' @param rename Logical. If TRUE the file will be renamed using the date of the HDF file.
#' @param ... Other arguments pass to \code{\link{reproject_modis_hdf}} function.
#'
#' @description Runs \code{\link{reproject_modis_hdf}} on a batch of files.
#' It internally use MODIS Reproject Tool (MRT) software to mosaic the downloaded images.
#' @note To have the functionality for Mosaic and reprojecting of the images,
#' you need to first install MRT software (\url{https://lpdaac.usgs.gov/tools/modis_reprojection_tool})
#' on your machine, and introduce its' path through the
#' mrt_path argument.
#' @seealso \code{\link{mosaic_modis_hdf}} \code{\link{reproject_modis_hdf}} \code{\link{get_hdf_dates}}
batch_mrt_reporjection <- function(hdf_date,
                                   mosaic_hdf_path,
                                   mosaic_tif_path,
                                   bands_subset = "",
                                   proj_type,
                                   rename = T,
                                   ...){
  reproject_modis_hdf(hdf_name = file.path(mosaic_hdf_path, paste0(hdf_date, ".hdf")),
                      dst_fpname = file.path(mosaic_tif_path, paste0(hdf_date, ".tif")),
                      bands_subset = bands_subset,
                      proj_type = proj_type, ...)
  if(rename){
    file.rename(list.files(mosaic_tif_path, pattern = hdf_date, full.names = T),
                file.path(mosaic_tif_path, paste0(hdf_date, ".tif")))
  }
}
