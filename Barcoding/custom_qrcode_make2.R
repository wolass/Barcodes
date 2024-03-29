custom_qrcode_make <- function (Labels, ErrCorr) 
{
  Xtxt <- gsub("_", "-", Labels)
  if (nchar(Xtxt) <= 1) {
    Xtxt <- paste0("\\s\\s", Xtxt)
    warning("Label is single character or blank. Padding with empty spaces.")
  }
  if(grepl(Xtxt,pattern = "- - -")){
    cat("we got into the right place")
    Xpng <- grid::rasterGrob(matrix(c(1,1),
                                    nrow =21,ncol=21), interpolate = FALSE)
  } else{
    cat("We are in the wrong place")
    Xpng <- grid::rasterGrob(abs(qrcode::qr_code(paste0(Xtxt), 
                                                    ecl = ErrCorr
                                                    # dataOutput = TRUE, 
                                                    # plotQRcode = FALSE, 
                                                    # mask = 3
                                                    ) - 1), 
                             interpolate = FALSE)
    
  }
  return(Xpng)
}
