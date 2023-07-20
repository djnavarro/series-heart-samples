library(Rcpp)
library(cairocore)
library(cairobasic)
library(munsell)

# sequence
#its <- 1:99

# interesting cases
its <- c(1, 4, 6, 8, 11, 19, 20, 25, 26, 32, 36, 37, 38, 50, 52, 54, 
         55, 59, 60, 63, 64, 68, 73, 74, 83, 85, 86, 88, 93, 94, 99)


# function
flame20 <- function(seed, fname = NULL, layers = 3, iter = 3000000, box = 2){
  
  sourceCpp(here::here("source", "ff_p.cpp"))
    
  # fixed
  prefix <- "ff_20"
  filter_y <- c(-box, box)
  filter_x <- c(-box, box)
  adjust <- function(x, a = 1) {
    adjustcolor(x, 1, a, a, a)
  }
  
  set.seed(seed)
  
  
  # make palette ------------------------------------------------------------
  
  map <- munsell:::munsell.map
  hue <- sample(mnsl_hues(), 1)
  clr <- unique(map$name[map$hue == hue])
  clr <- grep("^[^N]", clr, value = TRUE)
  clr_left <- sample(clr, 1)
  clr_right <- complement(clr_left)
  
  msq <- seq_mnsl(clr_left, clr_right, 5, fix = TRUE)
  hsq <- mnsl2hex(msq)
  pfn <- colorRampPalette(hsq)
  pal <- pfn(256)
  
  bg <- sample(pal, 1)
  bg <- adjust(bg, 2)
  
  pal <- adjust(pal, .7)
  pal <- gsub("$FF", "10", pal)
  
  
  # generate image ----------------------------------------------------------
  
  cat("generating...\n")
  
  df <- flame(iter, layers)
  df <- as.data.frame(df)
  names(df) <- c("x","y","c")
  df <- df[-(1:100),]
  
  

  # new palette -------------------------------------------------------------

  pal <- scico::scico(256, palette = "grayC", alpha = .02)  
  bg <- "white"

  # tidy --------------------------------------------------------------------

  
  # filter observations outside the range
  if(!is.null(filter_x)) {
    keep <- df$y > filter_y[1] & df$y < filter_y[2] & 
      df$x > filter_x[1] & df$x < filter_x[2]
    df <- df[keep, ]
    df$c[df$c < -1] <- -1
    df$c[df$c > 1] <- 1
  }
  
  # manually scale the co-ordinates to the image size
  px <- 5000
  xrng <- max(df[,1]) - min(df[,1])
  yrng <- max(df[,2]) - min(df[,2])
  rng <- max(c(xrng, yrng))
  
  xdiff <- max(c(yrng - xrng, 0))/2
  ydiff <- max(c(xrng - yrng, 0))/2
  
  brd <- 0
  df[,1] <- brd + (df[,1] - min(df[,1]) + xdiff) / rng * (px - 2*brd)
  df[,2] <- brd + (df[,2] - min(df[,2]) + ydiff) / rng * (px - 2*brd)
  df[,3] <- rank(df[,3])
  
  # manually create a vector of colours
  col_idx <- as.integer((df[,3] - min(df[,3])) / (max(df[,3]) - min(df[,3])) * 255) + 1L
  col <- pal[col_idx]
  
  if(is.null(fname)) { fname <- paste0(prefix, "_", seed, ".png") }
  fpath <- here::here("image", prefix, fname)
  
  
  
  # render image ------------------------------------------------------------
  
  cat("rendering...\n")
  
  cb <- cairobasic::CairoBasic$new(width = px, height = px, bg = bg, antialias = TRUE)
  cb$add_circles(x = df[,1], y = df[,2], r = 3, fill = col, colour = NA)
  cb$write_png(fpath)
  
}

# render everything...
for(i in its) {cat(i, "\n"); flame20(i); cat("\n")}


