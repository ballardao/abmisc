#' Model predictions
#'
#' Get data for model predictions and uncertainty varied over the range of \code{variable} while holding all other variables at their median values. Can support two-way interactions by specifying a variable name for \code{vary}.
#' @param mod A model whose coefficients and variance-covariance matrix will be used to do the predictions. Currently supports logistic regression (\code{glm} package) and hurdle models (\code{pscl} package).
#' @param df A dataframe containing data to be used in prediction. Should contain values for all variables used in \code{mod}.
#' @param variable A string specifying the variable that will be varied from its minimum to maximum values.
#' @param vary An (optional) string specifying a variable to interact with \code{variable} in doing the predictions. Defaults to \code{NULL}.
#' @param dv A string specifying which of the variables in \code{df} is the dependent variable in \code{mod}.
#' @param type A string specifying the model type (either 'logit' or 'hurdle'). Defaults to 'logit'.
#' @return A dataframe containing values of \code{variable} (as well as the name of the variable) and corresponding predicted values, as well as the values of the boundaries of 95 percent intervals. These values can easily be used to plot the substantive effect of \code{variable} (and \code{vary}, if applicable) on \code{dv}, according to the results of \code{mod}.

predData <- function(mod, df, variable, vary=NULL, dv, type='logit'){

  # Set up dataframe and variable names
  if(type=='logit'){
    ivs <- coef(mod) %>%
      names()
  } else if(type=='hurdle'){
    ivs <- coef(mod) %>%
      names() %>%
      str_extract(., '_.+') %>%
      str_replace(., '_', '') %>%
      unique()
  }
  nameMatch <- intersect(ivs, names(df)) # Column names that match (for subset)
  varIndex <- which(ivs==variable) # Which position in the coefficients is the sliding variable?
  completes <- as.data.frame(df) %>% #Tibbles don't work with this process
    dplyr::select(., c(dv, nameMatch)) %>%
    complete.cases # Pull out a logical for the complete cases for the IVs
  df <- as.data.frame(df) %>%
    filter(., completes) # Filter original df according to completes for IVs
  ivdata <- as.data.frame(df) %>%
    dplyr::select(., nameMatch) # Make ivdata df, by selecting on the matching variables
  ivdata <- cbind(intercept=rep(1, nrow(ivdata)), ivdata) # Add the intercept (first position)

  # Create scenario data
  scen <- NULL
  for(ii in 1:length(ivdata)){
    # Throw error if data won't work in a matrix (or be able to take the median)
    if(!class(ivdata[,ii]) %in% c('numeric', 'integer', 'factor')){
      stop('All variables must be numeric')
    }

    # If it's the variable we're varying, sort, if not, hold at the median value
    if(ii==varIndex){
      var <- sort(ivdata[,ii])
    } else {
      var <- rep(medNA(ivdata[,ii]), nrow(ivdata))
    }

    # Combine into scenario matrix
    scen <- cbind(scen, var)
    colnames(scen)[ii] <- names(ivdata)[ii]
  }

  # Create different scenarios for different values of the varied variable
  if(!is.null(vary)){
    scen1 <- scen0 <- scen
    scen1[,vary] <- rep(1, nrow(scen))
    scen0[,vary] <- rep(0, nrow(scen))
    scen <- rbind(scen1, scen0)
  }

  # Make it a data frame
  scen <- as.data.frame(scen)

  # Do prediction
  if(!is.null(vary)){

    if(type=='logit'){
      pred0 <- predict.glm(mod, newdata = scen[which(scen[,vary]==0), ], se.fit = TRUE, type = 'response' )
      pred1 <- predict.glm(mod, newdata = scen[which(scen[,vary]==1), ], se.fit = TRUE, type = 'response' )

      # Put in (gg)plot-friendly format
      gg0 <- data.frame(fit = pred0$fit, hi95 = pred0$fit + (1.96*pred0$se.fit),
                        lo95 = pred0$fit - (1.96*pred0$se.fit), x = scen[1:d(pred0$fit),varIndex],
                        var = variable, category = 'Minority') %>%
        .[!duplicated.data.frame(.), ]
      gg1 <- data.frame(fit = pred1$fit, hi95 = pred1$fit + (1.96*pred1$se.fit),
                        lo95 = pred1$fit - (1.96*pred1$se.fit), x= scen[1:d(pred1$fit),varIndex],
                        var = variable, category = 'Majority') %>%
        .[!duplicated.data.frame(.), ]

      # Combine
      predData <- rbind(gg0, gg1)
    } else if(type=='hurdle'){
      pred0 <- predict(mod, newdata = scen[which(scen[,vary]==0), ], se=TRUE)
    }
  } else {

    # Do prediction
    pred <- predict.glm(mod, newdata = scen, se.fit = TRUE, type = 'response')

    # Put in (gg)plot-friendly format
    predData <- data.frame(fit = pred$fit, hi95 = pred$fit + (1.96*pred$se.fit),
                           lo95 = pred$fit - (1.96*pred$se.fit), x = scen[1:d(pred$fit),varIndex],
                           var = variable) %>%
      .[!duplicated.data.frame(.), ]
  }
  # Return
  return(predData)
}
