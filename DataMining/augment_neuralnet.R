#' augment_neuralnet
#' 
#' An augment function for the neuralnet package, which adds fitted values and residuals to the data frame. 
#' Motivated by ChatGPT on 8/10/2025.
#'
#' @param model a model created by neuralnet
#' @param data data frame; the data used to fit the model
#'
#' @returns an augment data frame with the fitted values, residuals, and standard residuals
#' @export
#'
#' @examples
#' library(neuralnet)
#' data(iris)
#' # Fit a neural network model
#' nn_model <- neuralnet(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
#'                        data = iris, hidden = c(5, 3), linear.output = FALSE)  
#' # Use augment_neuralnet to get fitted values and residuals
#' augmented_data <- augment_neuralnet(nn_model, iris)
#' augmented_data
#' @rdname augment_neuralnet
augment_neuralnet <- function(model, data) {
  
  require(neuralnet)
  
  if (!inherits(model, "nn")) {
    stop("Model must be from the neuralnet package (class 'nn').")
  }

  data = model$data
  
  # Predict using the neuralnet model
  preds = predict(model, newdata=data)[,1]
  
  # preds <- predict(model, data[, model$covariate, drop = FALSE])$net.result
  
  # Bind predictions to the original data
  out <- data
  out$.fitted <- as.vector(preds)
  
  # If you have the actual response variable, compute residuals
  resp_var <- model$model.list["response"]$response
  
  if (!all(resp_var %in% names(data))) {
    resp_var = str_extract(resp_var, "[A-Za-z0-9]+")
  }
  
  out$.resid <- data[[resp_var]] - out$.fitted
  out$.std.resid = scale(out$.resid, center = 0, scale = 1)
  
  out
}
 

#' augment_any
#'
#' @param model a supported model object (lm, glm, rpart, neuralnet)
#' @param data data frame; the data used to fit the model
#' @param response character, the name of the response variable (for rpart and neuralnet)
#' @param predictors character, a vector of predictor variable names (for neuralnet)
#'
#' @returns an augment data frame with the fitted values, residuals, and standard residuals.
#' @export
#'
#' @examples
#' m_lm <- lm(mpg ~ wt + hp, data = mtcars)
#' augment_any(m_lm, data=mtcars, response="mpg", predictors=c("wt", "hp"))
#' library(rpart)
#' m_rpart <- rpart(mpg ~ wt + hp, data = mtcars)
#' augment_any(m_rpart, data = mtcars, response = "mpg")
#' 
augment_any <- function(model, data = NULL, response = NULL, predictors = NULL) {
  
  require(randomForest)
  require(rpart)
  require(neuralnet)
  require(broom)
  require(dplyr)
  
  # Default: try to guess data
  if (is.null(data) && !inherits(model, "nn")) {
    data <- tryCatch(model.frame(model), error = function(e) NULL)
  }
  
  if (inherits(model, "lm") || inherits(model, "glm")) {
    out = augment(model, data) 
    return(out)
  }
  
  if (inherits(model, "rpart")) {
    fitted_vals <- predict(model, data)
    out = data %>%
             mutate(.fitted = fitted_vals,
                    .resid = data[[response %||% all.vars(formula(model))[1]]] - .fitted,
                    )
    return(out)
}
  
  if (inherits(model, "neuralnet")) {
    if (is.null(response) || is.null(predictors)) {
      stop("For neuralnet models, please specify `response` and `predictors`.")
    }
    fitted_vals <- as.vector(compute(model, data[, predictors])$net.result)
    out =data %>%
             mutate(.fitted = fitted_vals,
                    .resid = data[[response]] - .fitted,
                    .std.resid = scale(.resid, center = 0, scale = 1)
                    )
    class(out) = "lm"
    return(out)
  }
  
  if (inherits(model, "randomForest")) {

    # Get predictions
    if (model$type == "regression") {
      
      fitted_vals <- predict(model, newdata = data)  # regression, default type="response"
      out <- data
      out$.fitted <- fitted_vals
      out$.resid  <- model$y - fitted_vals
      
    }
    
    else if (model$type == "classification") {
    
      prob_vals <- predict(model, newdata = data, type = "prob") # probabilities
      out <- data
      out$.pred_class <- predict(model, newdata = newdata) # classes
      out <- cbind(out, as.data.frame(prob_vals))
    }
    return(out)
  }

  stop("Model type not supported yet.")
}
