library(png)
library(grid)
library(gridExtra)

files <- list.files(
  "untitled_consensus_cluster",
  pattern = "^consensus\\d{3}\\.png$",
  full.names = TRUE
)

grobs <- lapply(files, function(f) {
  img <- readPNG(f)
  rasterGrob(
    img,
    interpolate = FALSE   # CRITICAL: prevents blur
  )
})

# Very large canvas to preserve detail
png(
  "consensus_all.png",
  width  = 9000,   # pixels
  height = 9000,
  res    = 300
)

grid.arrange(grobs = grobs, ncol = 3)
dev.off()

