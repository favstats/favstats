#' Create table with standardized estimates
#'
#' This function creates a stargazer table 
#' with standardized estimates
#'
#' @param model1 put in the model (lm object) here.
#' @param ... stargazer options.
#'
#' @return 
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


tbl_std <- function(model1, ...) {
  #Standardized betas
  model1.beta<-lm.beta(model1)
  #print
  model1.beta$standardized.coefficients[1] <- NA
  #coef(summary(model1.beta))[, "Std. Error"]  <- NA
  
  stargazer(model1, model1.beta, 
            coef = list(model1$coefficients, 
                        model1.beta$standardized.coefficients), 
            column.labels = c("b", "b*"),
            ...)
}


