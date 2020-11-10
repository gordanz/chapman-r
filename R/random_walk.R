#' Simulate a single trajectory of a random walk
#'
#' @param T time horizon
#' @param p up probability (defaults to 0.5)
#' @param nozero whether to remove the leading zero
#'
#' @return vector
#'
#' @export
single_trajectory = function(T, p = 0.5, nozero = FALSE) {
  delta = sample(c(-1, 1),
                 size = T,
                 replace = TRUE,
                 prob = c(1 - p, p))
  if (nozero)
    cumsum(delta)
  else
    c(0, cumsum(delta))
}

#' Simulate a data frame of paths of a random walk
#'
#' @param nsim number of trajectories simulated
#' @param T time horizon
#' @param p up probability (defaults to 0.5)
#'
#' @return data.frame
#'
#' @export
#' @examples
#' simulate_walk(nsim=5, T=10, p=0.3)
simulate_walk = function(nsim, T, p = 0.5) {
  data.frame(X0 = 0,
             t(replicate(
               nsim, single_trajectory(T, p, nozero = TRUE)
             )))
}

#' A single trajectory of a random bridge
#'
#' Does not return the initial 0
#'
#' @param T time horizon
#' @param k value at T
#' @param nozero whether to remove the leading zero
#'
#' @return vector
#'
#' @export
single_bridge = function(T, k, nozero = FALSE) {
  deltas = rep(-1, T)
  flip = sample(1:T, size = (T + k) / 2, replace = FALSE)
  deltas[flip] = 1
  if (nozero)
    cumsum(deltas)
  else
    c(0, cumsum(deltas))
}

#' Output all paths of a random walk
#'
#' @param T time horizon
#'
#' @return list
#' @param nozero whether to remove the leading zero
#'
#' @export
all_paths = function (T, nozero = FALSE) {
  Omega = list(2 ^ T)
  index = 1

  for (i in 0:T) {
    choices = utils::combn(T, i, simplify = FALSE)
    for (choice in choices) {
      increments = rep(-1, T)
      increments[choice] = 1
      if (nozero) {
        Omega[[index]] = cumsum(increments)
      } else {
        Omega[[index]] = c(0, cumsum(increments))
      }
      index = index + 1
    }
  }
  return(Omega)
}

#' Computes the first hitting time (>=)
#'
#' @param x vector
#' @param level numeric
#' @param T numeric
#'
#' @return integer
#'
#' @export
first_hitting_time = function(x, level, T = 0) {
  pos = 0
  T = ifelse(T == 0, length(x), min(T, length(x)))
  while (pos < T) {
    pos = pos + 1
    if (x[pos] >= level)
      return(pos)
  }
  return(NaN)
}

#' Reflects trajectory once it hits a level
#'
#' @param x vector
#' @param level numeric
#'
#' @return vector
#'
#' @export
reflect_after_hitting = function(x, level) {
  T = length(x) - 1
  if (T < 1)
    return(x)

  c = ifelse(level >= x[1], 1,-1)
  y = numeric(T + 1)
  y[1] = x[1]
  H = 1
  for (i in 1:T) {
    if (c * x[i] >= c * level)
      H = -1
    y[i + 1] = y[i] + H * (x[i + 1] - x[i])
  }
  return(y)
}


#' Computes the first maximum time
#'
#' @param x vector
#' @param T numeric
#'
#' @return integer
#'
#' @export
#' @examples
#' first_maximum_time(x=c(1,3,2,4,5,6,3,4))
first_maximum_time = function(x, T = 0) {
  if (length(x) == 0)
    stop("x needs to be nonempty.")
  if (T <= 0 || T > length(x))
    T = length(x)
  max_value = x[1]
  max_position = 1
  pos = 1
  while (pos < T) {
    pos = pos + 1
    if (x[pos] > max_value) {
      max_value = x[pos]
      max_position = pos
    }
  }
  return(max_position)
}


#' Computes the last maximum time
#'
#' @param x vector
#' @param T numeric
#'
#' @return integer
#'
#' @export
last_maximum_time = function(x, T = 0) {
  if (length(x) == 0)
    stop("x needs to be nonempty.")
  if (T <= 0 || T > length(x))
    T = length(x)
  max_value = x[1]
  max_position = 1
  pos = 1
  while (pos < T) {
    pos = pos + 1
    if (x[pos] >= max_value) {
      max_value = x[pos]
      max_position = pos
    }
  }
  return(max_position)
}
