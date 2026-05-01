library(magick)

# 1) list + sort files
files <- list.files("untitled_consensus_cluster", pattern = "\\.png$", full.names = TRUE)
files <- files[order(files)]

# 2) read
imgs <- image_read(files)

# 3) choose columns (3 makes a 3-wide grid; change to 4, 5, etc.)
ncol <- 3

# 4) pad with blank images so the last row is full
n <- length(imgs)
if (n %% ncol != 0) {
  pad <- ncol - (n %% ncol)
  blank <- image_blank(
    width  = image_info(imgs[1])$width,
    height = image_info(imgs[1])$height,
    color  = "white"
  )
  imgs <- c(imgs, rep(list(blank), pad))
}

# 5) stitch row-by-row, then stack rows
rows <- split(imgs, ceiling(seq_along(imgs) / ncol))
row_imgs <- lapply(rows, function(r) image_append(image_join(r)))  # horizontal
combined <- image_append(image_join(row_imgs), stack = TRUE)       # vertical

# 6) write output
image_write(combined, "consensus_combined.png")
