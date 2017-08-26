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
#' @importFrom foreach %dopar%
#' @importFrom foreach %do%
NULL
#' Reproject a series of MODSI satellite images using MRT.
#' @param mosaic_hdf_path The path to store the mosaiced hdf files.
#' @param mosaic_tif_path The path to stroe the mosaiced tif files.
#' @param .parallel Logical. Should reprojection be executed in parallel mode?
#' @param nCores Numeric. The number of clusters to use when \code{.parallel = T}.
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
#' @export
batch_mrt_reporjection <- function(mosaic_hdf_path,
                                   mosaic_tif_path,
                                   .parallel = T,
                                   nCores = 4,
                                   bands_subset = "",
                                   proj_type,
                                   rename = T,
                                   ...){
  temp_dir_already_exist <- dir.exists("./temp")
  if (!temp_dir_already_exist){
    dir.create("./temp")
    message(paste("Create folder: ",
                  normalizePath(path.expand("./temp"), mustWork = F),
                  sep = ""))
  }
  if(length(list.files(mosaic_tif_path)) != 0){
    unlink(mosaic_tif_path, recursive = T)
    dir.create(mosaic_tif_path)
  }
  hdf_dates <- modisr::get_hdf_dates(mosaic_hdf_path)
  if(.parallel){
    message("Run in parallel mode:")
    cl <- modisr::create_snow_cluster(nCores)
    pb <- utils::txtProgressBar(max = length(hdf_dates), style = 3)
    progress <- function(n) utils::setTxtProgressBar(pb, n)
    opts <- list(progress = progress)
    foreach::foreach(hdf_date = hdf_dates,
                     .options.snow = opts) %dopar% {
                       reproject_modis_hdf(hdf_fpname = file.path(mosaic_hdf_path, paste0(hdf_date, ".hdf")),
                                           dst_fpname = file.path(mosaic_tif_path, paste0(hdf_date, ".tif")),
                                           bands_subset = bands_subset,
                                           proj_type = proj_type, ...)
                       if(rename){
                         file.rename(list.files(mosaic_tif_path, pattern = hdf_date, full.names = T),
                                     file.path(mosaic_tif_path, paste0(hdf_date, ".tif")))
                       }
                     }
  } else{
    foreach::foreach(hdf_date = hdf_dates) %do% {
      reproject_modis_hdf(hdf_fpname = file.path(mosaic_hdf_path, paste0(hdf_date, ".hdf")),
                          dst_fpname = file.path(mosaic_tif_path, paste0(hdf_date, ".tif")),
                          bands_subset = bands_subset,
                          proj_type = proj_type, ...)
      if(rename){
        file.rename(list.files(mosaic_tif_path, pattern = hdf_date, full.names = T),
                    file.path(mosaic_tif_path, paste0(hdf_date, ".tif")))
      }
    }

  }
  on.exit({
    snow::stopCluster(cl)
    close(pb)
    unlink("./temp", recursive = T)
  })
}
