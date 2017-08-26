#' Mosaic MODIS hdf files.
#' @param hdf_fpnames The names of the hdf files to be mosaiced.
#' @param dst_fpname The full name of the output hdf file. e.g. "C:/out.hdf".
#' @param mrt_path The path of the MRT executable file. If missing, default is "C:/MRT/bin".
#' @param bands_subset A character vector indicating the spectral subset of the input hdf file.
#' @param delete Logical. If TRUE the original hdf files will be deleted after mosaicing.
#' Default is FALSE.
#' @description Modified from the original version in the 'rts' pacakge by Babak Naimi.
#' It internally use MODIS Reproject Tool (MRT) software to mosaic the downloaded images.
#' @note To have the functionality for Mosaic and reprojecting of the images,
#' you need to first install MRT software (\url{https://lpdaac.usgs.gov/tools/modis_reprojection_tool})
#' on your machine, and introduce its' path through the
#' mrt_path argument.
#' @export

mosaic_modis_hdf <- function(hdf_fpnames,
                             dst_fpname,
                             mrt_path,
                             bands_subset,
                             delete = FALSE){
  if (missing(mrt_path)) mrt_path <- "C:/MRT/bin"
  if (length(hdf_fpnames) < 2) stop("mosaic cannot be called for ONE image!")
  if (missing(bands_subset))  bands_subset <- ''
  if (missing(delete)) delete <- FALSE
  mosaic_prm_fpname <- normalizePath(tempfile(pattern = "temp_mosaic",
                                              tmpdir = "./temp",
                                              fileext = ".prm"),
                                     mustWork = F,
                                     winslash = "/")
  mosaic_prm_file = file(mosaic_prm_fpname, open = "wt")
  write(paste(hdf_fpnames[1], sep = ""), mosaic_prm_file)
  for (j in 2:length(hdf_fpnames)) write(hdf_fpnames[j], mosaic_prm_file, append = T)
  close(mosaic_prm_file)
  # generate mosaic:

  if (bands_subset != '') {
    e <- system(paste(mrt_path, '/mrtmosaic -i ', mosaic_prm_fpname, ' -s "',bands_subset,'" -o ', dst_fpname, sep=""))
    if (e != 0) warning ("Mosaic failed! 'bands_subset' may has incorrect structure!")
  } else {
    e <- system(paste(mrt_path, '/mrtmosaic -i ', mosaic_prm_fpname,' -o ',dst_fpname, sep=""))
    if (e != 0) warning ("Mosaic failed!")
  }
  if (delete & e == 0) for (hdf_name in hdf_fpnames) unlink(hdf_name)
  if (e == 0) return (TRUE)
  else return (FALSE)
}
