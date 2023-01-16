### Define Table Functions
cubicSplines <- function(x, knots, knotHeights, splineCoeffs, boundaryCondition) {
  # Evaluates the value of the cubic splines at x
  #
  # Args:
  #   x: Point at which to evaluate the cubic splines
  #   knots: Knot locations of the splines
  #   knotHeights: Knot heights of the splines
  #   splineCoeffs: Cubic spline coefficients
  #   boundaryCondition: Boundary condition used to implement the splines
  #     0: Natural, 1: Clamped, 2: Not-a-knot, 3: Periodic
  #
  # Returns:
  #   The value of the cubic spline at x

  numKnots <- length(knots)
  y <- NaN
  # Find domain
  index <- -1
	 if (x < knots[1]){  # Extrapolate left
    x <- x - knots[1]
    a <- splineCoeffs[1, ]
    if (boundaryCondition == 0 | boundaryCondition == 1){  # Natural or clamped
      slope <- a[2]
      y <- slope * x + knotHeights[1]
    } else {  # Not-a-knot or periodic
      index <- 1
      y <- splineCoeffs[index, 1] + splineCoeffs[index, 2] * x + splineCoeffs[index, 3] * x^2 + splineCoeffs[index,4] * x^3
    }
  } else if (x > knots[numKnots]) {  # Extrapolate right
    a <- splineCoeffs[numKnots - 1]
    if (boundaryCondition == 0 | boundaryCondition == 1) {  # Natural or clamped
      x <- x - knots[numKnots]
      h <- knots[numKnots] - knots[numKnots - 1]
      slope <- a[2] + 2 * a[3] * h + 3 * a[4] * h^2
      y <- slope * x + knotHeights[numKnots]
    } else {  # Not-a-knot or periodic
      index <- numKnots - 1
      x <- x - knots[index]
      y <- splineCoeffs[index, 1] + splineCoeffs[index, 2] * x + splineCoeffs[index, 3] * x^2 + splineCoeffs[index, 4] * x^3
    }
  } else {  # Interpolate
    index <- 1
    while (x > knots[index + 1] & index < numKnots - 1) {
      index <- index + 1
    }
    x <- x - knots[index]
    y <- splineCoeffs[index, 1] + splineCoeffs[index, 2] * x + splineCoeffs[index, 3] * x^2 + splineCoeffs[index, 4] * x^3
  }
  return(y)
}


lookupTable <- function(data, index, col, lookupMethod, interpolate=NULL, extrapolate=NULL,
                        knots=NULL, knotHeights=NULL, splineCoeffs=NULL, boundaryCondition=NULL) {
  # Returns a value from the lookup table given an index
  #
  # Args:
  #   data: Lookup table data
  #   col: Index of column to lookup
  #   lookupMethod: Method to lookup value (Exact, Interpolate, or Truncate)
  #   interpolate: Interpolation method (Linear or Cubic Splines). Default is null.
  #   extrapolate: Extrapolation options (No, Left Only, Right Only, Both). Default is null.
  #   knots: Knot locations of the splines. Default is null.
  #   knotHeights: Knot heights of the splines. Default is null.
  #   splineCoeffs: Cubic spline coefficients. Default is null.
  #   boundaryCondition: Boundary condition used to implement the splines. Default is null.
  #     0: Natural, 1: Clamped, 2: Not-a-knot, 3: Periodic
  #
  # Returns:
  #   A value from the lookup table that corresponds to a given index

  col <- col + 1  # Index from 1
  numRows <- nrow(data)
  if (col < 2 | col > ncol(data)){
    return(NaN)  # Invalid column
  } else {  # Valid column
    val <- NaN
    if (lookupMethod == "Exact") {
      row <- 0
      found <- F
      while (found == F & row < numRows + 1) {
        row <- row + 1
        if (index == data[row, 1]) {
          found <- T
        }
      }
      if (found) {
        val <- data[row, col]
      }
    } else if(lookupMethod == "Truncate") {
      if (index < data[1, 1]) {  # Below first value - error
        val <- NaN
      } else if (index >= data[numRows, 1]) {  # Above last value
        val <- data[numRows, col]
      } else {  # Between
        row <- 1
        while (data[row, 1] < index) {
          row <- row + 1
        }
        if (index == data[row, 1]) {
          val <- data[row, col]
        } else {
          val <- data[row - 1, col]
        }
      }
    } else if (lookupMethod == "Interpolate") {
      if (interpolate == "Linear") {
        if (index <= data[1, 1]) {  # Below or at first index
          slope <- (data[2, col] - data[1, col]) / (data[2, 1] - data[1, 1])
          val <- data[1, col] - (data[1, 1] - index) * slope
        } else if (index > data[numRows, 1]) {  # Above last index
          slope <- (data[numRows, col] - data[numRows - 1, col]) / (data[numRows, 1] - data[numRows - 1, 1])
          val <- data[numRows, col] + (index - data[numRows, 1]) * slope
        } else {  # Between
          row <- 1
          while (data[row, 1] < index) {
            row <- row + 1
          }
          slope <- (data[row, col] - data[row - 1, col]) / (data[row, 1] - data[row - 1, 1])
          val <- data[row - 1, col] + (index - data[row - 1, 1]) * slope
        }
      } else if (interpolate == "Cubic Splines") {
        val <- cubicSplines(index, knots, knotHeights, splineCoeffs, boundaryCondition)
      }

      # Check extrapolation conditions
      if (extrapolate == "No") {
        if (index <= data[1, 1]) {  # Below or at first index
          val <- data[1, col]
        } else if (index > data[numRows, 1]) {  # Above last index
          val <- data[numRows, col]
        }
      } else if (extrapolate == "Left only") {  # Truncate right
        if (index > data[numRows, 1]) {  # Above last index
          val <- data[numRows, col]
        }
      } else if (extrapolate == "Right only") {  # Truncate left
        if (index <= data[1, 1]) {  # Below or at first index
          val <- data[1, col]
        }
      }
    }
    return(val)
  }
}

calcTableEV <- function(data, col) {
  # Calculates the expected value (EV) of the empirical distribution
  #
  # Args:
  #   data: Empirical distribution table
  #   col: Index of column for EV calculation
  #
  # Returns:
  #   The expected value of the empirical distribuion in the specified column

  col <- col + 1  # Index from 1
  ev <- 0
  for (r in 1:nrow(data)) {
    ev <- ev + data[r, 1] * data[r, col]
  }
  return(ev)
}

