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
#' Mosiac a series of MODSI satellite images using MRT.
#' @param hdf_date The date of the MODIS satellite image.
#' Can be obtained from the \code{\link{get_hdf_dates}} function.
#' @param raw_hdf_path The path of the downloaded hdf files.
#' @param mosaic_hdf_path The path to store the mosaiced hdf files.
#' @param bands_subset A character vector indicating the spectral subset of the input hdf file.
#' Pass to \code{\link{mosaic_modis_hdf}} function.
#' @param delete Logical. If TRUE the original hdf files will be deleted after mosaicing. Default is FALSE.
#' Pass to \code{\link{mosaic_modis_hdf}} function.
#' @param .parallel Logical. Should mosaic be executed in parallel mode?
#' @param nCores Numeric. The number of clusters to use when \code{.parallel = T}.
#'
#' @description Runs \code{\link{mosaic_modis_hdf}} on a batch of files.
#' It internally use MODIS Reproject Tool (MRT) software to mosaic the downloaded images.
#' @note To have the functionality for Mosaic and reprojecting of the images,
#' you need to first install MRT software (\url{https://lpdaac.usgs.gov/tools/modis_reprojection_tool})
#' on your machine, and introduce its' path through the
#' mrt_path argument.
#' @seealso \code{\link{mosaic_modis_hdf}} \code{\link{reproject_modis_hdf}} \code{\link{get_hdf_dates}}
#' @export
batch_mrt_mosaic <- function(raw_hdf_path,
                             mosaic_hdf_path,
                             bands_subset = "",
                             delete = FALSE,
                             .parallel = TRUE,
                             nCores = 4){
  if(length(list.files(mosaic_hdf_path)) != 0){
    unlink(mosaic_hdf_path, recursive = T)
    dir.create(mosaic_hdf_path)
  }
  temp_dir_already_exist <- dir.exists("./temp")
  if (!temp_dir_already_exist){
    dir.create("./temp")
    message(paste("Create folder: ",
                  normalizePath(path.expand("./temp"), mustWork = F),
                  sep = ""))
  }
  hdf_dates <- modisr::get_hdf_dates(raw_hdf_path)
  if(.parallel){
    message("Run in parallel mode:")
    cl <- modisr::create_snow_cluster(nCores)
    pb <- utils::txtProgressBar(max = length(hdf_dates), style = 3)
    progress <- function(n) utils::setTxtProgressBar(pb, n)
    opts <- list(progress = progress)
    foreach::foreach(hdf_date = hdf_dates,
                     .options.snow = opts) %dopar% {
                       mosaic_modis_hdf(hdf_fpnames = list.files(raw_hdf_path,
                                                                 pattern = hdf_date,
                                                                 full.names = T),
                                        dst_fpname = file.path(mosaic_hdf_path, paste0(hdf_date, ".hdf")),
                                        bands_subset = bands_subset,
                                        delete = delete)
                     }
  } else{
    foreach::foreach(hdf_date = hdf_dates) %do% {
      mosaic_modis_hdf(hdf_fpnames = list.files(raw_hdf_path,
                                                pattern = hdf_date,
                                                full.names = T),
                       dst_fpname = file.path(mosaic_hdf_path, paste0(hdf_date, ".hdf")),
                       bands_subset = bands_subset,
                       delete = delete)
    }
  }
  on.exit({
    snow::stopCluster(cl)
    close(pb)
    unlink("./temp", recursive = T)
  })
}
