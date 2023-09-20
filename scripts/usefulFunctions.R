summarizeR <- function(corMat, nvars=1) {

    averageRs <- matrix(nrow=(nrow(corMat)/nvars-1),
                        ncol=nvars)

    for (k in 1:nvars) {
        for (i in 1:((nrow(corMat)/nvars)-1)) {
            sumR <- 0
            nValid <- 0
            for (j in seq(1, (nrow(corMat)-nvars*i), by=nvars)) {
                if(!is.na(corMat[(j+(i*nvars)+(k-1)), j+(k-1)])) {
                    sumR <- sumR + corMat[(j+(i*nvars)+(k-1)), j+(k-1)]
                    nValid <- nValid + 1
                }
                
            }
            averageRs[i,k] <- sumR/(nValid)
        }
    }
    return(averageRs)
}

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


plotCors <- function(cors, user=FALSE) {
    cors <- as.data.frame(cors)
    names(cors)[1:2] <- c(
        "Generated X",
        "Generated Y"
    )
    if (user == TRUE) {
        names(cors)[3:4] <- c(
            "User X",
            "User Y"
        )
    }

    if (user == TRUE) {
        cors <- cors %>%
            mutate(lag=row_number()) %>%
            pivot_longer(!lag,
                         names_sep = " ",
                         names_to = c("Source", "Variable"))
        minCor <- min(cors$value)
    } else {
        cors <- cors %>%
            mutate(lag=row_number()) %>%
            pivot_longer(!lag,
                         names_to = "Variable")
        minCor <- min(cors$value)
    }

    if (user == TRUE) {
        ggplot(aes(x=lag, y=value, linetype=Source, color=Variable), data=cors) +
            geom_line() +
            geom_point() +
            ylim(min(minCor,0), 1) +
            scale_x_continuous(breaks = 1:nrow(cors))
    } else {
        ggplot(aes(x=lag, y=value, color=Variable), data=cors) +
            geom_line() +
            geom_point() +
            ylim(min(minCor,0), 1) +
            scale_x_continuous(breaks = 1:nrow(cors))
        }
}

arCor <- function(waves, a) {
  cors <- matrix(nrow = (waves - 1), ncol = 1)
  for (i in 1:(waves - 1)) {
    cors[i, 1] <- a^i
  }
  return(cors)
}


