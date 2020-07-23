# Jeremy Roth

expit <- function(x) exp(x) / (1 + exp(x))

AdairLopezPrediction  <- function(data,
                                  model.type,         # one of model1_males, model1_females, model1_both, model2_males, model2_females, model2_both
                                  name.regCDR.per.1000,
                                  name.pct65.as.decimal,
                                  name.under5.mortality.per.1000.livebirths,
                                  name.year,
                                  name.country,
                                  name.under5.completeness.as.decimal=NULL) {
  # variable checks
  if (!is.data.frame(data)) {
    stop("the dataset provided in data must be a data frame")
  }
  if (exists("AL_model_coefficients") == FALSE) {
    stop("object AL_model_coefficients, which should have been shared along with this function, must exist in your R envirionment.")
  }
  if (exists("AL_RE_coefficients") == FALSE) {
    stop("object AL_RE_coefficients, which should have been shared along with this function, must exist in your R envirionment")
  }
  if (!(name.regCDR.per.1000 %in% names(data))) {
    stop(paste("the variable", 
               name.regCDR.per.1000,
               "specified in name.regCDR.per.1000 was not found in the dataset"))
  }
  if (!(name.pct65.as.decimal %in% names(data))) {
    stop(paste("the variable", 
               name.pct65.as.decimal,
               "specified in name.pct65.as.decimal was not found in the dataset"))
  }
  if (!(name.under5.mortality.per.1000.livebirths %in% names(data))) {
    stop(paste("the variable", 
               name.under5.mortality.per.1000.livebirths,
               "specified in name.under5.mortality.per.1000.livebirths was not found in the dataset"))
  }
  if (!(name.year %in% names(data))) {
    stop(paste("the variable", 
               name.year,
               "specified in name.year was not found in the dataset"))
  }
  if (!(name.country %in% names(data))) {
    stop(paste("the variable", 
               name.country,
               "specified in name.country was not found in the dataset"))
  }
  stopifnot(name.regCDR.per.1000 %in% names(data))
  stopifnot(name.pct65.as.decimal %in% names(data))
  name.under5.mortality.per.1000.livebirths
  if (!is.numeric(data[, name.year]) & !is.integer(data[, name.year])) {
    stop(paste("the",
               name.year,
               "variable must be a numeric or an integer"))
  }
  if (all(floor(log10(data[!is.na(data[, name.year]), name.year])) + 1 != 4)) { ## just checking all year are 4-digit numbers
    stop(paste("the non-missing values of the",
               name.year,
               "variable must be 4-digit numbers"))
  }
  if (min(data[!is.na(data[, name.pct65.as.decimal]), name.pct65.as.decimal], na.rm=TRUE) < 0 |
      max(data[!is.na(data[, name.pct65.as.decimal]), name.pct65.as.decimal], na.rm=TRUE) > 1) {
    stop(paste("the non-missing values of the",
               name.pct65.as.decimal,
               "variable must be proportions between 0 and 1"))
  }
  
  data[, "my_regCDR_squared"] <- data[, name.regCDR.per.1000]^2 ## how I'll reference the regCDR^2 variabel going forward even if it's provided under a different name in data
  data[, "my_ln_5q0"] <- log(data[, name.under5.mortality.per.1000.livebirths])
  if (!(model.type %in% c("model1_males", "model1_females", "model1_both",
                          "model2_males", "model2_females", "model2_both"))) {
    stop("the specified model.type must be one of: model1_males, model1_females, model1_both, model2_males, model2_females, model2_both")
  }
  # compute predicted values based on coefficient estimates given in the paper
  data[, "AL_prediction_logit_scale"] <- 
    data[, "my_regCDR_squared"]       * AL_model_coefficients["RegCDRSquared", model.type] +
    data[, name.regCDR.per.1000]      * AL_model_coefficients["RegCDR", model.type] +
    data[, name.pct65.as.decimal]     * AL_model_coefficients["pct65+", model.type] +
    data[, "my_ln_5q0"]               * AL_model_coefficients["ln(5q0)", model.type] +
    data[, name.year]                 * AL_model_coefficients["Year", model.type] +
    AL_model_coefficients["Constant", model.type]
  if (model.type %in% c("model1_males", "model1_females", "model1_both")) {
    if (is.null(name.under5.completeness.as.decimal)) {
      stop(paste("if model 1 is being used, the argument name.under5.completeness.as.decimal needs to be provided"))
    } 
    if (!(name.under5.completeness.as.decimal %in% names(data))) {
      stop("if model 1 is being used, the variable specified in name.under5.completeness.as.decimal must be present in the dataset")
    }
    data[, "AL_prediction_logit_scale"] <- 
      data[, "AL_prediction_logit_scale"]  +
      data[, name.under5.completeness.as.decimal] * AL_model_coefficients["C_5q0", model.type]
  }
  # handling estimates of random intercepts (called RE)
  ## merging in correct RE estimates
  one_RE_estimate <- AL_RE_coefficients[, c("Country", model.type)]
  names(one_RE_estimate) <- c("country_just_for_merge", "RE_estimate")
  data[, "country_just_for_merge"] <- as.character(data[, name.country])
  data <- left_join(x=data,
                  y=one_RE_estimate,
                  by="country_just_for_merge")
  data[, "country_just_for_merge"] <- NULL
  countries_failed_merge_RE <- unique(as.character((data[is.na(data$RE_estimate), 
                                                         name.country])))
  n_countries_failed_merge_RE <- length(countries_failed_merge_RE)
  if (n_countries_failed_merge_RE > 0) {
    message(paste("[message 1/5] \nPlease note that the merging in of random effects coefficient estimates failed for the following",
                 length(countries_failed_merge_RE),
                 "countries in your dataset:"))
    message(paste("[message 2/5]\n",
                paste(countries_failed_merge_RE, collapse=", ")))
    message("[message 3/5]\n As a result, please check if any of those countries are in the following list of countries that did have random effects estimates in the Adair and Lopez (2019) paper:")
    message(paste("[message 4/5] \n",
                paste(AL_RE_coefficients$Country, collapse=", ")))
   message("[message 5/5]\n And, if so, please change the spelling to match the spelling of the country provided in the paper")
  }
  # adding RE estimate for countries where it is available
  data$RE_estimate_no_NA <- ifelse(is.na(data$RE_estimate),
                                   0,
                                   data$RE_estimate)
  data[, "AL_prediction_logit_scale"] <- 
    data[, "AL_prediction_logit_scale"]  + data[, "RE_estimate_no_NA"]
  
  # converting prediction to probability scale
  data[, "AL_predicted_completeness"]<- expit(data[, "AL_prediction_logit_scale"])
  
  # dropping some columns and adding one to indicate whether predicted completeness is based on RE term
  data[, "AL_prediction_logit_scale"] <- NULL
  data[, "RE_estimate_no_NA"] <- NULL
  data[, "AL_prediction_uses_random_effect"] <- ifelse(is.na(data$RE_estimate),
                                                       "No",
                                                       "Yes")
  return("data_with_predicted_completeness"=data)
}
