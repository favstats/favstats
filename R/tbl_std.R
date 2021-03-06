#' Create table with standardized estimates
#'
#' This function creates a stargazer table
#' with standardized estimates
#'
#' @param model put in the model (lm object) here.
#' @param ... stargazer options.
#'
#' @return returns a stargazer table.
#' @examples
#' # #fake data
#' var1<-rnorm(100, mean=10, sd=5)
#' var2<-rnorm(100, mean=5, sd=2)
#' var3<-rnorm(100, mean=2, sd=3)
#' var4<-rnorm(100, mean=5, sd=1)
#' df<-data.frame(var1, var2, var3, var4)
#' #model with unstandardized betas
#' model1<-lm(var1~var2+var3+var4, data=df)
#'
#' tbl_std(model1, type = "text")
#' @export

tbl_std <- function(model, ...) {
  #Standardized betas
  model1.beta<-lm.beta(model)
  #print
  #model1.beta$standardized.coefficients[1] <- NA
  #coef(summary(model1.beta))[, "Std. Error"]  <- NA

  stargazer(model, model1.beta,
            coef = list(model$coefficients,
                        model1.beta$standardized.coefficients),
            column.labels = c("b", "b*"), table.placement = "ht!",
            header = F, ...)
}


