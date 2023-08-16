library(tidyverse)

source("scripts/gen_starts.R")
source("scripts/usefulFunctions.R")

## Testing
waves <- 10

data <- gen_starts(
    n = 10000,
    nwaves = waves, # Number of waves
    ri_x = 0, # Random intercept variance for X
    ri_y = 0, # Random intercept variance for Y
    cor_i = .5, # Correlation between intercepts (as correlation)
    x = 1, # AR variance for X
    y = 1, # AR variance for Y
    stab_x = .7, # Stability of X
    stab_y = .7, # Stability of Y
    yx = 0, # Cross lag (Y regressed on X)
    xy = 0, # Cross lag (X regressed on Y)
    cor_xy = .5, # Correlation between X and Y (as correlation)
    xr = .5, # Measurement error for X
    yr = .5 # Measurement error for Y
)

## Reorder data for summarizeR
data <- data[, paste0(c("x", "y"), rep(1:waves, each = 2))]
corMat <- cor(data)
cors <- summarizeR(cor(data), 2)
plotCors(cors)

cors2 <- cbind(cors, cors)
plotCors(cors2, user=TRUE)


## Test upload

out <- cor(data[, paste0(c("x", "y"), rep(1:waves, each=2))])
summarizeR(out, 2)

write.csv(out, "test.csv", row.names=FALSE)


write.table(out, "test.csv", row.names = FALSE, col.names = FALSE)

temp <- as.matrix(read.csv("~/temp/ncors.csv"))
summarizeR(temp, 2)



summarizeLags <- function(corMat, nvars=2) {

    averageRs <- matrix(nrow=(nrow(corMat)/nvars-1),
                        ncol=nvars)

    for (k in 1:nvars) {
        for (i in 1:((nrow(corMat)/nvars)-1)) {
            sumR <- 0
            nValid <- 0
            for (j in seq(1, (nrow(corMat)-nvars*i), by=nvars)) {
                rval <- (nvars-k) + ((i * nvars) - 1) + j + 1
                cval <- j + k - 1
                print(c(rval,cval))
                if(!is.na(corMat[rval, cval])) {
                    sumR <- sumR + corMat[rval, cval]
                    nValid <- nValid + 1
                }
                
            }
            averageRs[i,k] <- sumR/(nValid)
        }
    }
    return(averageRs)
}

summarizeLags(corMat)
