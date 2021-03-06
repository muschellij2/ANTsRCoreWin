#' Image Write
#'
#' Write an image object of S4 class \code{antsImage} to a file.
#'
#'
#' @param image Image object of S4 class \code{antsImage} to be written.
#' @param filename Name of the file to write the image to.
#' @return 0 -- Success\cr 1 -- Failure
#' @author Shrinidhi KL
#' @seealso \code{\link{antsImageRead}}
#' @examples
#'
#' fn <- getANTsRData( "r16" )
#' fi <- antsImageRead( fn )
#' antsImageWrite( fi , tempfile( fileext = ".nii.gz" ) )
#' antsImageWrite( fi , tempfile( fileext = ".mha" ) )
#' antsImageWrite( fi , tempfile( fileext = ".nrrd" ) )
#' antsImageWrite( antsImageClone( fi, "unsigned int" ) ,
#'   tempfile( fileext = ".jpg" )  )
#' antsImageWrite( antsImageClone( fi, "float" ) ,
#'   tempfile( fileext = ".tif" )  )
#' antsImageWrite( fi, tempfile( fileext = ".mrc" )  )
#' antsImageWrite( fi, tempfile( fileext = ".hd5" )  )
#' components(fi) = 0L
#' antsImageWrite( fi, tempfile( fileext = ".nii.gz" )  )
#' components(fi) = -1L
#' testthat::expect_error(
#' antsImageWrite( fi, tempfile( fileext = ".nii.gz" )), "nvalid S4"
#' )  
#' testthat::expect_error(
#' antsImageWrite( "hey"), "not of class antsImage"
#' ) 
#'
#' @export antsImageWrite
antsImageWrite <- function(image, filename) {
  if (class(image) != "antsImage") {
    stop("'image' argument provided is not of class antsImage")
  }

  if (length(image@components) == 0 || image@components == 0)
    {
    image@components = as.integer(1)
    }
  filename <- path.expand(filename)
  invisible(.Call("antsImageWrite", image, filename, PACKAGE = "ANTsRCoreWin"))
}
