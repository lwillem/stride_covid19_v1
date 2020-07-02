

#' Density function of Truncated Weibull Distribution
#' @param x Value of the variable after deductible and limit max(0,min(claim,limit)-deductible)
#' @param shape Shape parameter
#' @param scale Scale parameter
#' @param min Left truncation deductible
#' @param max Right truncation limit
#' @examples
#' dtweibull(2.5,2,3)
#' @rdname tweibull
#' @export
dtweibull <- function(x,shape,scale,min=0,max=1e+9){
	ifelse(x==0, pweibull(min,shape,scale),
			ifelse(x>=(max-min-1e-10), 1 - pweibull(max,shape,scale),
			ifelse (x<0, 0,
			ifelse(x<(max-min), dweibull(x+min,shape,scale), 0))))
}

#' Cumulative probability function of Truncated Weibull Distribution
#' @param q Value of the variable after deductible and limit max(0,min(claim,limit)-deductible)
#' @examples
#' ptweibull(2.5,2,3)
#' @rdname tweibull
#' @export
ptweibull <- function(q,shape,scale,min=0,max=1e+9) {
	ifelse(q==0, pweibull(min,shape,scale),
			ifelse(q>=(max-min)-1e-10, 1,
			ifelse (q<0, 0,
			ifelse(q<(max-min), pweibull(q+min,shape,scale), 0))))
}

#' Quantile function of Truncated Weibull Distribution max(0,min(claim,limit)-deductible)
#' @param p Value of the probability
#' @examples
#' qtweibull(0.5,2,3)
#' @rdname tweibull
#' @export
qtweibull <- function(p,shape,scale,min=0,max=1000000000) {
	ifelse(p<=pweibull(min,shape,scale), 0,
			ifelse(p>=pweibull(max,shape,scale), max-min, qweibull(p,shape,scale)-min))
}

#' Random generation of Truncated Weibull Distribution max(0,min(claim,limit)-deductible)
#' @param n Number of samples
#' @examples
#' rtweibull(100,2,3)
#' @rdname tweibull
#' @export
rtweibull <- function(n,shape,scale,min=0,max=1e+9) {qtweibull(runif(n),shape,scale,min,max)}


