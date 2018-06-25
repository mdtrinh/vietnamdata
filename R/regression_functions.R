## Regression functions

#' Generate flexible permutation matrix for blocked, clustered or simpler
#' designs
#'
#' An alternative version of \code{ri::genperms}. Given user-input vectors of
#' clusters or blocks, generate an exact permutation matrix, or a randomly
#' sampled permutation matrix if the number of actual permutations is too high.
#' Improves upon \code{ri::genperms} by allowing permutation of non-binary
#' vectors.
#'
#' Unlike its counterpart in the \code{ri} package, this function can perform
#' permutation of input vectors that are not binary. It also accepts as
#' arguments for \code{blockvar} and \code{clustvar} other data types than
#' \code{integer}. \code{genperms} is primarily based on the \code{permute}
#' package.
#'
#' @param x a vector to be permuted. Can be continuous
#' @param block a vector of equal length as \code{x}, with unique values
#'   indicating different blocks
#' @param clus a vector of equal length as \code{x}, with unique values
#'   indicating different blocks
#' @param maxiter maximum number of permutations to be included in the
#' permutation matrix. If it is possible to perform exact permutation with
#' fewer permutations, then the exact permutation matrix is produced.
#' @return  A permutation matrix where each row is a permutation of the input
#'   vector \code{x}
#'
#' @references Gerber, Alan S. and Donald P. Green. 2012. Field Experiments:
#'   Design, Analysis, and Interpretation. New York: W.W. Norton.
#' @export
genperms <- function(x, block = NULL, clus = NULL, maxiter = 10000) {
  if(!is.null(block)){
    block <- as.integer(as.factor(block))
  }
  if(!is.null(clus)){
    clus <- as.integer(as.factor(clus))
  }

  if(!is.null(clus)){ #
    clus.trim <- unique(clus)

    if(!is.null(block)){
      block.trim <- block[!duplicated(clus)]

      perm <- permute::shuffleSet(length(clus.trim),
                                  nset = maxiter,
                                  permute::how(block = block.trim))
    } else {
      perm <- permute::shuffleSet(length(clus.trim),
                                  nset = maxiter)
    }

    perm <- apply(perm, 1, function(i) clus.trim[i])
    perm <- apply(perm, 2, function(clusvec) unlist(
      lapply(clusvec, function(clusvar) x[which(clus==clusvar)])))

  } else {
    if(!is.null(block)) {
      perm <- permute::shuffleSet(length(x),
                                  nset = maxiter,
                                  permute::how(block = block))
    } else {
      perm <- permute::shuffleSet(length(x),
                                  nset = maxiter)
    }

    perm <- apply(perm, 1, function(i) x[i])

  }

  return(perm)
}

#' Randomization Inference on Treatment Effect using Linear Regression
#'
#' Estimates of the treatment effect using linear models through \code{lm}, then
#' conducts inference using randomization inference by permuting the treatment
#' vector to obtain the sharp null distribution
#'
#' Estimates of the treatment effects are obtained by OLS regression. When
#' covariates are included, the randomization distribution is obtained by
#' permuting the "partialled-out" treatment vector i.e. the vector of residuals
#' from a regression of treatment on covariates. Internally \code{rireg} makes
#' call to \code{genperms}. The variable whose names are given by
#' \code{blockvar} and \code{clustvar} will be coerced into input vectors for
#' the \code{block} and \code{clus} arguments of the \code{genperms} function.
#'
#' @param data a data frame containing the variables in the model
#' @param outcome a character. Name of the outcome variable.
#' @param treatment a character. Name of the treatment variable.
#' @param covs a character vector. Names of the covariates to be used in the
#'   model.
#' @param perm a matrix containing permutations of the treatment variable (for
#'   \code{rireg} or \code{riSynth}) or the outcome variable (for \code{riwfe}).
#'   When \code{perm} is supplied the call to \code{genperms} will not be made
#'   and the arguments \code{blockvar}, \code{clustvar} and \code{maxiter} will
#'   be ignored
#' @param blockvar an optional character vector. Name of the block variable if
#'   the randomization inference procedure requires block randomization. The
#'   variable named by \code{blockvar} will be used as input for the
#'   \code{genperms} function.
#' @param clustvar an optional character vector. Name of the cluster variable if
#'   the randomization inference procedure requires clustered randomization. The
#'   variable named by \code{clustvar} will be used as input for the
#'   \code{genperms} function.
#' @param maxiter a positive integer. The maximum number of permutations to be
#'   included in the permutation matrix for the randomization distribution. Used
#'   as input for the \code{genperms} function.
#' @return An object of class \code{riFit}
#' @export
rireg <- function(data, outcome, treatment, covs, perm = NULL, blockvar = NULL, clustvar = NULL, maxiter = 10000,
                  covs_control = list(method = "Euclidean",
                                      tol_quantile = .05,
                                      tol_function = function(x) .25*sd(x),
                                      tol_value = NULL)) {

  # run regression once to get point estimate
  full <- lm(as.formula(paste(outcome, paste(c(treatment, covs), collapse = "+"), sep = "~")), data = data)
  beta.actual <- coef(summary(full))[treatment, 1]
  p.actual <- coef(summary(full))[treatment, 4]

  # purge the outcome variable of covariate-based noise
  if(!is.null(covs)){
    purge.y <- lm(as.formula(paste(outcome, paste(covs, collapse = "+"), sep = "~")), data = data)
    y.tilde <- resid(purge.y)
  } else {
    y.tilde <- data[[outcome]]
  }

  # March 16 2018 update
  # stop using old method to control covariate
  # partial out treatment variable
  # if(!is.null(covs)){
  #     purge.treat <- lm(as.formula(paste(treatment, paste(covs, collapse = "+"), sep = "~")), data = data)
  #     treatment.tilde <- resid(purge.treat)
  # } else {
  #     treatment.tilde <- data[[treatment]]
  # }

  # generate permutation of the treatment variable

  # if a permutation is supplied, skip generating permutation
  if(!is.null(perm)){
    if(ncol(perm) != nrow(data) & nrow(perm) != nrow(data)) {
      stop("Dimension of perm does not match nrow(data)!")
    } else if(ncol(perm) == nrow(data)) {
      perm <- t(perm) ## transpose perm s.t. nrow(perm) = nrow(data)
    }
    message("Using user-supplied permutation matrix...")
  } else {
    if(!is.null(blockvar)){
      block <- as.integer(as.factor(data[[blockvar]]))
    } else {
      block <- NULL
    }
    if(!is.null(clustvar)){
      clus <- as.integer(as.factor(data[[clustvar]]))
    } else {
      clus <- NULL
    }
    perm <- ri::genperms(treatment.tilde, block = block, clus = clus)
    message("Using auto-generated permutation matrix...")
  }

  # March 16 2018 update
  # New method to control for covariates
  # filter from universe of permutation vectors only those that
  # are feasible (i.e B'X = Z'X; see Rosenbaum (1984))
  if(!is.null(covs)) {

    # calculate distance based on standardized (no need for demeaning) X
    model_matrix <- cbind(model.matrix(full)[,1,drop=FALSE], apply(model.matrix(full)[,-1], 2, scale, center = FALSE))

    ZtF_treat <- data[[treatment]] %*% model_matrix
    ZtF_all <- t(perm) %*% model_matrix

    # uses exact match i.e B'X = Z'X, or
    # a fuzzy match i.e. B'X and Z'X are close enough
    # in terms of an user-input distance matrix
    # and an user-input tolerance/caliper parameter
    if(covs_control$method == "Exact") {
      eligible_perm <- which(apply(ZtF_all, 1, function(x) identical(x, ZtF_treat[1,]) ))

      message("Determining eligible permutations using exact method")
    } else {
      if(proxy::pr_DB$entry_exists(covs_control$method)) {
        # Mahalanobis distance requires additional vcov
        if(covs_control$method == "Mahalanobis") {
          perm_distance <- as.numeric(proxy::dist(ZtF_treat, ZtF_all, method = covs_control$method, cov = cov(ZtF_all)))
        } else {
          perm_distance <- as.numeric(proxy::dist(ZtF_treat, ZtF_all, method = covs_control$method))
        }

        # determine tolerance/caliper
        if (!is.null(covs_control$tol_quantile)) {
          caliper <- quantile(perm_distance, covs_control$tol_quantile, na.rm = TRUE)
        } else if(!is.null(covs_control$tol_function)) {
          caliper <- covs_control$tol_function(perm_distance)
        } else if (!is.null(covs_control$tol_value)) {
          caliper <- covs_control$tol_value
        } else {
          stop("A non-exact method to determine eligible permutations is selected,
                 but no method to calculate a tolerance threshold is provided!")
        }

        eligible_perm <- which(perm_distance <= caliper)

      } else {
        stop("Method ", covs_control$method, " not available!")
      }

      message("Determining eligible permutations using non-exact method: ", covs_control$method)
    }

    if(length(eligible_perm) == 0) {
      stop("No eligible permutation found!")
    } else if(length(eligible_perm) < nrow(data)) {
      warning("Number of eligible permutations < n, permutation distribution unlikely to be reliable!")
    } else {
      message("Number of eligible permutations: ", length(eligible_perm))
    }

    perm <- as.matrix(perm[, eligible_perm])

  }

  # for each permutation of the vector, calculate treatment effect
  beta <- rep(NA, ncol(perm)) # initiialize vector before the loop
  p <- rep(NA, ncol(perm))    # helps with speed

  for(i in 1:ncol(perm)) { ## for and apply are equally slow

    # Note: think more about whether statistic calculated for
    # permuted sample needs to include covariates (likely yes,
    # but verify!)

    #treatment.perm <- perm[,i]
    #fit <- lm(y.tilde ~ treatment.perm)

    data$treatment.perm <- perm[,i]
    fit <- lm(as.formula(paste(outcome, paste(c("treatment.perm", covs), collapse = "+"), sep = "~")), data = data)


    if(!is.na(coef(fit)["treatment.perm"])){
      beta[i] <- coef(summary(fit))["treatment.perm", 1]
      p[i] <- coef(summary(fit))["treatment.perm", 4]
    } else {
      beta[i] <- NA
      p[i] <- NA
    }
  }

  beta.greater <- mean(beta > beta.actual, na.rm=T)
  beta.smaller <- mean(beta < beta.actual, na.rm=T)
  p.smaller <- mean(p < p.actual, na.rm=T)

  out <- list(beta = beta,
              beta.actual=beta.actual,
              beta.greater=beta.greater,
              beta.smaller=beta.smaller,
              p.actual=p.actual,
              p.smaller=p.smaller)
  class(out) <- "riFit"

  return(out)
}

#' Randomization Inference on Treatment Effect using Weighted Fixed Effects
#' Regression
#'
#' Estimates of the treatment effect using Weighted Fixed Effects Regression
#' through the \code{wfe} function in the \code{wfe} package, then conducts
#' inference using randomization inference by permuting the treatment vector to
#' obtain the sharp null distribution
#'
#' Estimates of the treatment effects are obtained by OLS regression. When
#' covariates are included, the randomization distribution is obtained by
#' permuting the outcome vector. This is equivalent to permuting the treatment
#' vector and all associated covariates. Unlike \code{rireg}, the \code{riwfe}
#' function does not make use of the partialling-out method because the function
#' \code{wfe} from the \code{wfe} package that \code{riwfe} calls does not allow
#' the treatment variable to be omitted. Internally, \code{riwfe} makes call to
#' \code{genperms}. The variable whose names are given by \code{blockvar} and
#' \code{clustvar} will be coerced into input vectors for the \code{block} and
#' \code{clus} arguments of the \code{genperms} function. The arguments
#' \code{unit.index}, \code{time.index}, \code{method}, \code{qoi},
#' \code{estimator}, \code{unbiased.se} are input directly into the call for
#' \code{wfe}.
#'
#' @inheritParams rireg
#' @inheritParams wfe::wfe
#' @return An object of class \code{riFit}
#' @export
riwfe <- function(data, outcome, treatment, covs, perm = NULL, blockvar = NULL, clustvar = NULL, maxiter = 1000,
                  unit.index, time.index, method, qoi = "ate", estimator = NULL, unbiased.se = TRUE,
                  covs_control = list(method = "Euclidean",
                                      tol_quantile = .05,
                                      tol_function = function(x) .25*sd(x),
                                      tol_value = NULL)) {

  # run true regression once to get point estimate
  full <- wfe(as.formula(paste(outcome, paste(c(treatment, covs), collapse = "+"), sep = "~")),
              treat = treatment,
              unit.index = unit.index,
              time.index = time.index,
              method = method,
              qoi = qoi,
              unbiased.se = unbiased.se,
              estimator = estimator,
              data = data)
  beta.actual <- coef(summary(full))[treatment, 1]
  p.actual <- coef(summary(full))[treatment, 4]

  # generate permutation of the *outcome* variable
  # the idea is that permuting the outcome vector is the
  # same as permuting all the rows of the (treat, covs)
  # matrix

  # if a permutation is supplied, skip generating permutation
  if(!is.null(perm)){
    if(ncol(perm) != nrow(data) & nrow(perm) != nrow(data)) {
      stop("Dimension of perm does not match nrow(data)!")
    } else if(ncol(perm) == nrow(data)) {
      perm <- t(perm) ## transpose perm s.t. nrow(perm) = nrow(data)
    }
    message("Using user-supplied permutation matrix...")
  } else {

    if(!is.null(blockvar)){
      block <- as.integer(as.factor(data[[blockvar]]))
    } else {
      block <- NULL
    }
    if(!is.null(clustvar)){
      clus <- as.integer(as.factor(data[[clustvar]]))
    } else {
      clus <- NULL
    }
    perm <- ri::genperms(treatment, block = block, clus = clus)
    message("Using auto-generated permutation matrix...")
  }

  # March 16 2018 update
  # New method to control for covariates
  # filter from universe of permutation vectors only those that
  # are feasible (i.e B'X = Z'X; see Rosenbaum (1984))
  if(!is.null(covs)) {

    # calculate distance based on standardized (no need for demeaning) X
    model_matrix <- model.matrix(as.formula(paste("", paste(c(treatment, covs), collapse = "+"), sep = "~")), data = data)
    model_matrix <- cbind(model_matrix[,1,drop=FALSE], apply(model_matrix[,-1], 2, scale, center = FALSE))

    ZtF_treat <- data[[treatment]] %*% model_matrix
    ZtF_all <- t(perm) %*% model_matrix

    # uses exact match i.e B'X = Z'X, or
    # a fuzzy match i.e. B'X and Z'X are close enough
    # in terms of an user-input distance matrix
    # and an user-input tolerance/caliper parameter
    if(covs_control$method == "Exact") {
      eligible_perm <- which(apply(ZtF_all, 1, function(x) identical(x, ZtF_treat[1,]) ))

      message("Determining eligible permutations using exact method")
    } else {
      if(proxy::pr_DB$entry_exists(covs_control$method)) {
        # Mahalanobis distance requires additional vcov
        if(covs_control$method == "Mahalanobis") {
          perm_distance <- as.numeric(proxy::dist(ZtF_treat, ZtF_all, method = covs_control$method, cov = cov(ZtF_all)))
        } else {
          perm_distance <- as.numeric(proxy::dist(ZtF_treat, ZtF_all, method = covs_control$method))
        }

        # determine tolerance/caliper
        if (!is.null(covs_control$tol_quantile)) {
          caliper <- quantile(perm_distance, covs_control$tol_quantile, na.rm = TRUE)
        } else if(!is.null(covs_control$tol_function)) {
          caliper <- covs_control$tol_function(perm_distance)
        } else if (!is.null(covs_control$tol_value)) {
          caliper <- covs_control$tol_value
        } else {
          stop("A non-exact method to determine eligible permutations is selected,
                 but no method to calculate a tolerance threshold is provided!")
        }

        eligible_perm <- which(perm_distance <= caliper)

      } else {
        stop("Method ", covs_control$method, " not available!")
      }

      message("Determining eligible permutations using non-exact method: ", covs_control$method)
    }

    if(length(eligible_perm) == 0) {
      stop("No eligible permutation found!")
    } else if(length(eligible_perm) < nrow(data)) {
      warning("Number of eligible permutations < n, permutation distribution unlikely to be reliable!")
    } else {
      message("Number of eligible permutations: ", length(eligible_perm))
    }

    perm <- as.matrix(perm[, eligible_perm])

  }

  # for each permutation of the vector, calculate treatment effect
  beta <- rep(NA, ncol(perm)) # initiialize vector before the loop
  p <- rep(NA, ncol(perm))    # helps with speed

  # regress the purged outcome on permutations of treatment variable
  for(i in 1:ncol(perm)) {
    treatment.perm <- perm[,i]
    data[[treatment]] <- treatment.perm

    fit <- wfe(as.formula(paste(outcome, paste(c(treatment, covs), collapse = "+"), sep = "~")),
               treat = treatment,
               unit.index = unit.index,
               time.index = time.index,
               method = method,
               qoi = qoi,
               unbiased.se = unbiased.se,
               estimator = estimator,
               data = data)

    if(!is.na(coef(fit)[treatment])){
      beta[i] <- coef(summary(fit))[treatment, 1]
      p[i] <- coef(summary(fit))[treatment, 4]
    } else {
      beta[i] <- NA
      p[i] <- NA
    }
  }

  beta.greater <- mean(beta > beta.actual, na.rm=T)
  beta.smaller <- mean(beta < beta.actual, na.rm=T)
  p.smaller <- mean(p < p.actual, na.rm=T)

  out <- list(beta = beta,
              beta.actual=beta.actual,
              beta.greater=beta.greater,
              beta.smaller=beta.smaller,
              p.actual=p.actual,
              p.smaller=p.smaller)

  class(out) <- "riFit"

  return(out)
}

#' Plot Point Estimate and Randomization Distribution for Randomization
#' Inference Result
#'
#' A plot showing the point estimate for the treatment effect as a red line, and
#' the randomization distribution of the treatment effect as gray bars.
#'
#' @param x An object of the class \code{riFit} generated by
#'   \code{rireg} or \code{riwfe}.
#' @param title The main title (on top)
#' @param xlab X axis label
#' @param ylab Y axis label
#' @param scale logical; if TRUE the point estimates and the randomization
#' distribution is demeaned and normalized before plotting (default is FALSE)
#' @param xmin a numeric indicating the minimum value of the plot's main axix
#' @param xmax a numeric indicating the maximum value of the plot's main axix
#' @param axe.y logical; if TRUE the y-axis is shown (default is FALSE)
#' @param ... Other arguments passed on to \code{ggplot2::ggplot}. Currently unused
#' @import ggplot2
#' @export
plot.riFit <- function(x, title = NULL, xlab = NULL, ylab=NULL, scale=F, xmin, xmax, axe.y=F, ...){
  beta.null <- data.frame(beta = x$beta)
  beta.actual <- x$beta.actual

  if(missing(xmin) | missing(xmax)) {
    xmin <- min(beta.null$beta) - sd(beta.null$beta)
    xmax <- max(beta.null$beta) + sd(beta.null$beta)
  }
  if(scale){
    beta.actual <- (beta.actual - mean(beta.null$beta))/sd(beta.null$beta)
    beta.null$beta <- (beta.null$beta - mean(beta.null$beta))/sd(beta.null$beta)
  }

  if(axe.y) {
    axes <- ggplot2::theme(axis.text.x=element_blank(),
                           axis.ticks=element_blank(),
                           axis.title.x=element_blank(),
                           axis.title.y=element_blank(),legend.position="none")
  } else {
    axes <- ggplot2::theme(axis.text=element_blank(),
                           axis.ticks=element_blank(),
                           axis.title.x=element_blank(),
                           axis.title.y=element_blank(),legend.position="none")
  }

  ggplot2::ggplot(beta.null, aes(x = beta), ...) +
    ggplot2::stat_bin(aes(y=..count../sum(..count..)), fill = "grey") +
    ggplot2::geom_vline(aes(xintercept = beta.actual), colour="red") +
    ggplot2::geom_vline(aes(xintercept = 0), linetype="dashed", colour="black") +
    ggplot2::ggtitle(title) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab) +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = element_text(hjust = 0.5)) +
    axes +
    ggplot2::coord_flip(xlim = c(xmin,xmax))
}

### Synthetic control functions
### helper functions

#' Synthetic Control ATT
#'
#' Estimates the average treatment effect on the treated using the Synthetic
#' Control Method, then conducts
#' inference using randomization inference by permuting the treatment vector to
#' obtain the sharp null distribution
#'
#' For each unit in the treatment group, \code{Synthatt} creates a synthetic
#' control using weighted averages of the units in the control group, then
#' estimates an unit-specific treatment effect. The Average Treatment Effect on
#' the Treated is calculated by taking the average of all unit-specific
#' treatment effects.
#'
#' Internally, \code{Synthatt} makes use of the \code{synth} and \code{dataprep}
#' functions in the \code{Synth} package. For computers with multiple cores,
#' \code{Synthatt} can perform parallel computation to improve computation
#' speed.
#'
#' @inheritParams rireg
#' @param treatment.year the year or time period at which treatment occurs
#' @param pretreatment.year the years or time periods before treatment occurs. Observations from these
#' years or time periods will be used to create the synthetic contorl
#' @param posttreatment.year the years or time periods after treatment occurs. Observations form these
#' years or time periods will be used to calculate treatment effects.
#' @inheritParams Synth::dataprep
#' @param include.past.Y a logical; if TRUE past values of the outcomes are included as a covariate
#' to produce the synthetic control
#' @param snowfall a logical; if TRUE the function performs parallel computing
#'   using the snowfall package
#' @return An list with two objects: \code{tau.i}, a vector of unit-specific treatment effects, and
#' \code{tau.att} the Average Treatment Effect on the Treated
#' @export
SynthATT <- function(data, outcome, treatment, covs,
                     treatment.year, pretreatment.year, posttreatment.year=NULL,
                     unit.variable, unit.names.variable, time.variable,
                     include.past.Y = TRUE, snowfall=FALSE) {
  # check whether past outcomes is used as predictors
  if(include.past.Y) {
    predictors <- c(outcome, treatment, covs)
  } else {
    predictors <- c(treatment, covs)
  }

  # get treatment and control identifiers
  treatment.identifier <- data[[unit.variable]][which(data[[treatment]] == 1 & data[[time.variable]] == treatment.year)]
  #names(treatment.identifier) <- data[[unit.names.variable]][treatment.identifier]
  control.identifier <- data[[unit.variable]][which(data[[treatment]] == 0 & data[[time.variable]] == treatment.year)]
  #names(control.identifier) <- data[[unit.names.variable]][control.identifier]

  # prepare parallel computing
  if(snowfall){
    requireNamespace(snowfall, quietly = TRUE)
    snowfall::sfInit(parallel = TRUE, cpus = parallel::detectCores())
    snowfall::sfExport(list=c("data", "outcome", "treatment", "covs",
                              "treatment.year", "pretreatment.year", "posttreatment.year",
                              "unit.variable", "unit.names.variable", "time.variable",
                              "include.past.Y", "treatment.identifier", "control.identifier"))
    snowfall::sfLibrary("Synth")

    apply.fun <- snowfall::sfSapply
  }
  else{
    apply.fun <- sapply
  }

  tau <- apply.fun(1:length(treatment.identifier), function (i) {
    # run Synth once to get point estimate
    dataprep.obj <- dataprep(foo = data,
                             predictors = predictors, # can add history of treatment and outcomes
                             predictors.op = "mean",
                             dependent = outcome,
                             unit.variable = unit.variable,
                             unit.names.variable = unit.names.variable,
                             time.variable = time.variable,
                             treatment.identifier = treatment.identifier[i],
                             controls.identifier = control.identifier,
                             time.predictors.prior = pretreatment.year,
                             time.optimize.ssr = pretreatment.year,
                             time.plot=c(pretreatment.year,treatment.year,posttreatment.year))

    synth.out <- synth(dataprep.obj)
    # extract estimates
    y1 <- dataprep.obj$Y1plot
    y0 <- dataprep.obj$Y0plot %*% synth.out$solution.w

    # effect is more like a dif-in-dif, since sometimes the synthetic control does not track too well
    tau <- (mean(y1[as.character(c(treatment.year, posttreatment.year)),]) -
              mean(y1[as.character(c(pretreatment.year)),])) -
      (mean(y0[as.character(c(treatment.year, posttreatment.year)),]) -
         mean(y0[as.character(c(pretreatment.year)),]))

    return(tau)
  })

  if(snowfall){
    # result from snowfall is in list form
    tau <- unlist(tau, recursive = F)
    snowfall::sfStop()
  }
  return(list(tau.i = tau,
              tau.att = mean(tau)))
}

#' Randomization Inference on ATT using Synthetic Control
#'
#' Estimates average treatment effect on the treated using the Synthetic Control
#' Method, then conducts inference using randomization inference by permuting
#' the treatment vector to obtain the sharp null distribution
#'
#' Estimates of treatment effects are obtained by the Synthetic Control Method
#' through the \code{Synthatt} function. The randomization distribution is
#' obtained by permuting the treatment vector. Internally, \code{riSynth} makes
#' call to \code{genperms}. It is not yet possible to perform block or clustered
#' randomization. The arguments \code{treatment.year}, \code{pretreatment.year},
#' \code{posttreatment.year}, \code{unit.variable}, \code{unit.names.variable},
#' \code{time.variable}, \code{include.past.Y} are input directly into the call
#' for \code{Synth::Synth}.
#'
#' Internally, \code{riSynth} makes use of the \code{Synth} function in the
#' \code{Synth} package to calculate estimates of the ATT and the \code{permute}
#' function in the \code{permute} package to create the randomization
#' distribution. For computers with multiple cores, \code{riSynth} can perform
#' parallel computation to improve computation speed.
#'
#' @inheritParams SynthATT
#' @inheritParams rireg
#' @inheritParams genperms
#' @return An object of class "riSynth"
#' @export
riSynth <- function(data, outcome, treatment, covs,
                    treatment.year, pretreatment.year, posttreatment.year=NULL,
                    unit.variable, unit.names.variable, time.variable,
                    include.past.Y = TRUE, snowfall=FALSE, maxiter = 1000) {

  # loop over all treated units
  tau.actual <- SynthATT(data, outcome, treatment, covs,
                         treatment.year, pretreatment.year, posttreatment.year,
                         unit.variable, unit.names.variable, time.variable,
                         include.past.Y, snowfall)

  # generate permutations of treatment variable

  # if a permutation is supplied, skip generating permutation
  if(!is.null(perm)){
    if(ncol(perm) != length(data[[treatment]][which(data[[time.variable]] == treatment.year)]) &
       nrow(perm) != length(data[[treatment]][which(data[[time.variable]] == treatment.year)])) {
      stop("Dimension of perm does not match number of units!")
    } else if(ncol(perm) == length(data[[treatment]][which(data[[time.variable]] == treatment.year)])) {
      perm <- t(perm) ## transpose perm s.t. nrow(perm) = nrow(data)
    }
    message("Using user-supplied permutation matrix...")
  } else {
    perm <- genperms(data[[treatment]][which(data[[time.variable]] == treatment.year)], maxiter)
    message("Using auto-generated permutation matrix...")
  }

  if(snowfall){
    requireNamespace(snowfall, quietly = TRUE)
    snowfall::sfInit(parallel = TRUE, cpus = parallel::detectCores())
    snowfall::sfExport(list=c("data", "outcome", "treatment", "covs",
                              "treatment.year", "pretreatment.year", "posttreatment.year",
                              "unit.variable", "unit.names.variable", "time.variable",
                              "include.past.Y", "SynthATT", "perm"))
    snowfall::sfLibrary("Synth")

    apply.fun <- snowfall::sfLapply
  } else {
    apply.fun <- lapply
  }
  tau <- apply.fun(1:ncol(perm), function (i) {
    treatment.perm <- perm[,i]
    data[[treatment]][which(data[[time.variable]] == treatment.year)] <- treatment.perm

    tau <- tryCatch(SynthATT(data, outcome, treatment, covs,
                             treatment.year, pretreatment.year, posttreatment.year,
                             unit.variable, unit.names.variable, time.variable,
                             include.past.Y, snowfall = snowfall),
                    error = function(e) NULL)
    return(tau)
  })

  if(snowfall){
    # result from snowfall is in list form
    snowfall::sfStop()
  }

  tau.att <- unlist(lapply(tau, function(x) x["tau.att"]), use.names = F)
  tau.i <- do.call(rbind,unlist(lapply(tau, function(x) x["tau.i"]), recursive=F, use.names = F))

  tau.att.greater <- mean(tau.att > tau.actual$tau.att, na.rm=T)
  tau.att.smaller <- mean(tau.att < tau.actual$tau.att, na.rm=T)

  out <- list(tau.att.actual=tau.actual$tau.att,
              tau.i.actual=tau.actual$tau.i,
              tau.att=tau.att,
              tau.i=tau.i,
              tau.att.greater=tau.att.greater,
              tau.att.smaller=tau.att.smaller)
  class(out) <- "riSynth"

  return(out)
}

#' Convert \code{riSynth} object to \code{riFit}
#'
#' Convert a \code{riSynth} to a \code{riFit} object
#'
#' Useful for plotting. Converts all \code{tau.att} elements in the \code{riSynth}
#' object to corresponding \code{beta} elements in \code{riFit} and drops all
#' \code{tau.i} elements.
#'
#' @param riSynth.obj An object of the class \code{riSynth} generated by
#'   \code{riSynth}.
#' @return a \code{riFit} object
riSynthToriFit <- function(riSynth.obj) {
  if(!class(riSynth.obj) == "riSynth") {
    stop("Input is not a riSynth object")
  } else {
    riFit.obj <- list(beta = riSynth.obj$tau.att,
                      beta.actual = riSynth.obj$tau.att.actual,
                      beta.greater = riSynth.obj$tau.att.greater,
                      beta.smaller = riSynth.obj$tau.att.smaller,
                      p.actual = NA,
                      p.smaller = NA)


  }
  return(riFit.obj)
}

#' Plot Point Estimate and Randomization Distribution for Randomization
#' Inference Result from riSynth
#'
#' A plot showing the point estimate for the treatment effect as a red line, and
#' the randomization distribution of the treatment effect as gray bars.
#'
#' \code{plot.riSynth} converts a \code{riSynth} object into a \code{riFit} object
#' and then make a call to \code{plot.riFit}.
#'
#' @inheritParams plot.riFit
#' @param x an \code{riSynth} object
#' @param att logical; currently unused
#' @export
plot.riSynth <- function(x, title = NULL, xlab = NULL, ylab=NULL, scale=F, xmin, xmax, att=T, axe.y=F, ...) {

  x <- riSynthToriFit(x)

  plot.riFit(x, title, xlab, ylab, scale, xmin, xmax, axe.y, ...)
}
