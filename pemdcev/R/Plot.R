

Plot <- setClass("Plot", slots = c(.plot = "language"), contains = c("function"))

setMethod("initialize", "Plot",
          function(.Object, ...) {
            expr <- substitute(...)
            f <- function() {
              eval(expr, envir = parent.frame())
            }
            .Object@.plot <- expr
            .Object@.Data <- f
            .Object
          })


# test <- Plot({
#   plot(x = 1:10, y = 1:10)
#   abline(a = 0, b = 2)
#   legend("topleft", legend = "legend")
# })
#
# test
# test()
# test@.Data()
# abline(a = 1, b = 0.5, col = "red")

setMethod("print",
          signature(x = "Plot"),
          function (x, ...)
          {
            print(x@.plot)
          })


# test  # this is show
# print(test)

# Would be an idea to isolate par (pass it as slot and restore after plotting...)


