# Recolor image based on gaussian mixed model classification
# Jeff Oliver
# jcoliver@arizona.edu
# 2022-03-29

library(jpeg)
library(mclust)

# Testing approach on very small 4 x 2 "image"
small <- array(data = c(1,0,0,0, # R, column 1
                        1,1,0,1, # R, column 2
                        1,0,1,0, # G, column 1
                        0,0,1,1, # G, column 2
                        1,1,0,0, # B, column 1
                        0,1,1,0),# B, column 2
               dim = c(4, 2, 3))
plot(raster::as.raster(small))
plot(raster::as.raster(small), interpolate = FALSE)

# Convert 3D image array to 2D matrix
small_mat <- matrix(data = small,
                    nrow = prod(dim(small)[1:2]),
                    ncol = dim(small)[3])

# Do GMM, only considering two clusters
small_gmm <- Mclust(data = small_mat, G = 2)

# Extract classification information
small_class <- small_gmm$classification

# Reassign all in cluster 1 to black
small_black <- small
small_black[small_class == 1] <- 0 # all channels 0 for black
plot(raster::as.raster(small_black), interpolate = FALSE)

# Reassign all in cluster 2 to cyan
small_cyan <- small
# Vectors get recycled by dimension. Order for filling cells in a 3D array:
# [, , 1]
#  1  4
#  2  5
#  3  6
# [, , 2]
#  7 10
#  8 11
#  9 12
# [, , 3]
# 13 16
# 14 17
# 15 18
# So, for the array above, if we use a three element vector (-1, -2, -3) to 
# replace values in [3, 1] and [1, 2], we would see:
# [, , 1]
#  1  -2
#  2   5
# -1   6
# [, , 2]
#  7  -1
#  8  11
# -3  12
# [, , 3]
# 13  -3
# 14  17
# -2  18
# It basically fills up the first column, then the second column, and so on of 
# the first index of the third dimension. It then moves to the second index of 
# the third dimension, and finally to the third index of the third dimension.

# Vectors recycled a bit funny, first fill up red channel, then green, then 
# blue.
num_changing <- sum(small_class == 2)
cyan_vec <- c(rep(x = 0, times = num_changing),  # red channel
              rep(x = 1, times = num_changing),  # green channel
              rep(x = 1, times = num_changing))  # blue channel
small_cyan[small_class == 2] <- cyan_vec
plot(raster::as.raster(small_cyan), interpolate = FALSE)

# On a real image
img_url <- "https://soda.la.psu.edu/images/google-logo/image"
img_file <- "image.jpg"
download.file(url = img_url,
              destfile = img_file)
img1 <- readJPEG(img_file)
height <- dim(img1)[1]
width <- dim(img1)[2]
color <- dim(img1)[3]

# Convert 3D image array to 2D matrix
img1_mat <- matrix(data = img1,
                   nrow = prod(dim(img1)[1:2]),
                   ncol = dim(img1)[3])

# Do GMM, only considering three clusters
img1_gmm <- Mclust(data = img1_mat, G = 3)

# Extract classification information
img1_class <- img1_gmm$classification

# Reassign all in cluster 1 to black
img1_black <- img1
img1_black[img1_class == 1] <- c(0, 0, 0)
img1_black[img1_class_mat == 1] <- c(0, 0, 0)
plot(raster::as.raster(img1_black))

# Reassign clusters 1 & 3 to white
img1_white <- img1
img1_white[img1_class %in% c(1, 3)] <- c(1, 1, 1)
plot(raster::as.raster(img1_white))

# Reassign cluster 2 to something other than black or white (in this case, red)
img1_red <- img1
# Create 
num_changing <- sum(img1_class == 2)
red_vec <- c(rep(x = 1, times = num_changing),  # red channel
             rep(x = 0, times = num_changing),  # green channel
             rep(x = 0, times = num_changing))  # blue channel
img1_red[img1_class == 2] <- red_vec
plot(raster::as.raster(img1_red))

# Reassign all clusters: 1 = magenta, 2 = yellow, 3 = cyan
img1_cym <- img1
# Start by counting how many pixels are in each cluster
num_magenta <- sum(img1_class == 1)
num_yellow <- sum(img1_class == 2)
num_cyan <- sum(img1_class == 3)

# Create vectors for each of the three new colors
magenta_vec <- c(rep(x = 1, times = num_magenta),  # red channel
                 rep(x = 0, times = num_magenta),  # green channel
                 rep(x = 1, times = num_magenta))  # blue channel

yellow_vec <- c(rep(x = 1, times = num_yellow),  # red channel
                rep(x = 1, times = num_yellow),  # green channel
                rep(x = 0, times = num_yellow))  # blue channel

cyan_vec <- c(rep(x = 0, times = num_cyan),  # red channel
              rep(x = 1, times = num_cyan),  # green channel
              rep(x = 1, times = num_cyan))  # blue channel
img1_cym[img1_class == 1] <- magenta_vec
img1_cym[img1_class == 2] <- yellow_vec
img1_cym[img1_class == 3] <- cyan_vec
plot(raster::as.raster(img1_cym))

