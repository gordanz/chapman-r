# simulate the next position of the chain
draw_next = function(m,s) {
  i = match(s, S) # the row number of the state s
  sample(S, prob = P[i, ], size = 1)
}

# simulate a single trajectory of length T
# from the initial state
single_trajectory = function(initial_state) {
  path = numeric(T)
  last = initial_state
  for (n in 1:T) {
    path[n] = draw_next(last)
    last = path[n]
  }
  return(path)
}

# simulate the entire chain
simulate_chain = function(m, i) {
  i = id_of(m, i)
  data.frame(X0 = i,
             t(replicate(
               nsim, single_trajectory(m,i)
             )))
}


simulate_paths = function(m, T=10, nsim=1000) {

}
