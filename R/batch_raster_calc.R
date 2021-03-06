#' @importFrom foreach %dopar%
#' @importFrom foreach %do%
#' @import raster
NULL
#' Calculate batch of satellite images using raster.
#' @export
#' @param infiles Character. A directory or a character vector of files (including their path).
#' If a directory, all files matching the pattern will be converted.
#' @param outdir Character. Output directory to save the output files.
#' @param outsuffix Character. The suffix to append to the input filename (minus its extension)
#' to generate the output filename(s).
#' @param pattern Character. If infiles is a directory, this is used to limit the file it is searching for.
#' @param .parallel Logical. Should mask be executed in parallel mode?
#' @param nCores Numeric. The number of clusters to use when \code{.parallel = T}.
#' @param .fun Function to apply to each raster.
#' @param recursive  Logical. If infiles is a directory, should files be searched for recursively?
#' @param verbose Logical. Enable verbose execution? Default is FALSE.
#' @param ... Parameters to pass to \code{\link[raster]{writeRaster}}.
#'
#' @details This function is designed to run customized functions over rasters in batch mode.
#' Files are passed to the function either directly as a character vector of filenames,
#' or by passing it a directory and (typically) a search pattern (e.g. pattern=".tif").
#'
#' If a parallel engine is started and registered with foreach,
#' this program will run in parallel.
#' \code{writeRaster} will execute based on parameters passed to it,
#' and the output file will be named based on the input file (stripped of its extension),
#' with the outsuffix appended to it.
#'
#' @return Either a list of NULLs or
#' a list of RasterBricks depending on whether output_Raster is set to TRUE.
#'
#' @seealso  \code{\link[raster]{writeRaster}}
batch_raster_calc <- function (infiles,
                               outdir,
                               outsuffix = ".tif",
                               pattern = ".tif$",
                               .parallel = T,
                               nCores = 4,
                               .fun,
                               recursive = FALSE,
                               verbose = FALSE,
                               ...)
{
  infile <- NULL
  outfile <- NULL
  if (missing(outdir) || !file.exists(outdir) || !file.info(outdir)$isdir) {
    stop("Please select a valid outdir.")
  }
  if (file.info(infiles)$isdir) {
    if (verbose)
      message("Input is a directory...")
    infiles <- list.files(infiles, pattern = pattern, full.names = TRUE,
                          recursive = recursive)
  }
  checkFiles <- sapply(infiles, function(x) return(file.exists(x)))
  if (!all(checkFiles)) {
    message("The following files are missing:")
    message(infiles[!checkFiles])
    stop()
  }
  .out_file_path <- function(x,
                             .outsuffix = outsuffix,
                             .outdir = outdir) {
    outfilename <- paste(gdalUtils::remove_file_extension(basename(x)), .outsuffix, sep = "")
    outfilepath <- normalizePath(file.path(.outdir, outfilename), mustWork = FALSE)
    return(outfilepath)
  }
  outfiles <- sapply(infiles,
                     FUN = .out_file_path,
                     .outsuffix = outsuffix,
                     .outdir = outdir)
  if(.parallel){
    message("Run in parallel mode:")
    cl <- modisr::create_snow_cluster(nCores)
    pb <- utils::txtProgressBar(max = length(outfiles), style = 3)
    progress <- function(n) utils::setTxtProgressBar(pb, n)
    opts <- list(progress = progress)
    foreach::foreach(infile = infiles,
                     outfile = outfiles,
                     .options.snow = opts) %dopar% {
                       temp.raster <- raster(infile)
                       writeRaster(.fun(temp.raster),
                                   outfile,
                                   ...)
                     }
    snow::stopCluster(cl)
    on.exit(close(pb))
  } else{
    foreach::foreach(infile = infiles,
                     outfile = outfiles,
                     .options.snow = opts) %do% {
                       temp.raster <- raster(infile)
                       writeRaster(.fun(temp.raster),
                                   outfile,
                                   ...)
                     }
  }
  return(1)
}
