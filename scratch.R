library(tidyverse)

source("scripts/gen_starts.R")
source("scripts/usefulFunctions.R")

## Testing
waves <- 12

data <- gen_starts(
    n = 10000,
    nwaves = waves, # Number of waves
    ri_x = 1, # Random intercept variance for X
    ri_y = 1, # Random intercept variance for Y
    cor_i = .5, # Correlation between intercepts (as correlation)
    x = 1, # AR variance for X
    y = .4, # AR variance for Y
    stab_x = .5, # Stability of X
    stab_y = .4, # Stability of Y
    yx = 0, # Cross lag (Y regressed on X)
    xy = 0, # Cross lag (X regressed on Y)
    cor_xy = 0, # Correlation between X and Y (as correlation)
    xr = .5, # Measurement error for X
    yr = 0 # Measurement error for Y
)

## Reorder data for summarizeR
data <- data[, paste0(c("x", "y"), rep(1:waves, each = 2))]
cors <- summarizeR(cor(data), 2)
plotCors(cors)

cors2 <- cbind(cors, cors)
plotCors(cors2, user=TRUE)


## Test upload

out <- cor(data[, paste0(c("x", "y"), rep(1:waves, each=2))])
summarizeR(out, 2)

write.csv(out, "test.csv", row.names=FALSE)


write.table(out, "test.csv", row.names = FALSE, col.names = FALSE)

temp <- as.matrix(read.csv("test.csv"))
