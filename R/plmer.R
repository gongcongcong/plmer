#' @description run lmer in parallel
#' @title lmer
#' @param ncore number of cores to use
#' @param data data.table to perform
#' @param group perform lmer for `data` grouped by the `group` columns
#' @param formula formula for lmer function
#' @param verbose logical
#' @importFrom parallel makeCluster stopCluster parLapply clusterExport
#' @importFrom data.table rbindlist
#' @examples
#' system.time(ret <- plmer(dat[c("A1BG", "AAMP")], "genes", value~(1|tissues) + (1|individual), ncore = 16))
#' @return data.table
#' @export
plmer <- function(dat, group, formula, ncore = 4, verbose = TRUE) {
        # cat("lmer for gene: ", gene, "\n")
        dat_ls <- .split_data(dat, group)
        cl <- makeCluster(ncore)
        on.exit(stopCluster(cl))
        clusterExport(cl, "verbose", environment())
        ret_ls <- parLapply(cl, dat_ls, function (d) {
                fit <- try({
                        lme4::lmer(formula, d, REML = T, verbose = F) |>
                                summary()
                }, silent = T)
                if ("try-error" %in% class(fit)) {
                        if (verbose) message(fit)
                        return(list(var.ind = NA_real_, var.tis = NA_real_))
                }
                .vars <- as.numeric(lme4:::formatVC(fit$varcor)[,3]) ** 2
                .vars.name <- lme4:::formatVC(fit$varcor)[,1]
                .vars.tol <- sum(.vars)
                ret <- list(.vars[[1]]/.vars.tol, .vars[[2]]/.vars.tol)
                names(ret) <- .vars.name[-length(.vars.name)]
                return(ret)
        })
        rbindlist(ret_ls, idcol = TRUE)
}


#' @title _split_data
#' @param data data.table to split
#' @param group `data` to be splitted by the `group` column
#' @return list
.split_data <-
        function(data, group) {
                setkeyv(data, group)
                ret <- split(data, data[[group]])
                names(ret) <- unique(data[[group]])
                ret
        }
