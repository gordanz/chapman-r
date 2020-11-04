# MC Examples -------------------------------------------------------------

random_row <- function(n, p) {
  num = rbinom(1, n - 1, p) + 1
  non_zeros = runif(num) %>%
    scale(center = F)
  row = c(non_zeros, rep(0, n - num)) %>%
    sample

  return(row / sum(row))

}

#' Builds a mc with random transitions
#'
#' @param n number of states
#' @param p a parameter governing the total number of edges
#'
#'
#'
#' @return
#' @export
#'
#' @examples
#' m = random_mc(3, p = 0.4)
#' print(transition_matrix(m))
random_mc <- function(n, p = 0.5) {
  P = t(replicate(n, random_row(n, p)))
  m = P %>%
    new_mc_from_matrix(name = "Random") %>%
    set_fancy_layout %>%
    rotate(pca=TRUE) %>%
    set_graphics_parameters(edge.label = NA) %>%
    curve_overlapping_edges()
  m$edges$loop_angle = -pi / 2
  return(m)
}



#' An mc used to test color schemes
#'
#' @param number_of_states
#'
#' @return
#' @export
#'
#' @examples
#' m = color_tester_mc(10)
#' m = set_fancy_edge_colors(m)
#' plot(m)
color_tester_mc <- function(number_of_states) {
  m = new_mc(name = "Color Tester", number_of_states = number_of_states)
  for (i in 1:number_of_states)
    for (j in 1:number_of_states)
      m = m %>% add_edge_by_indices(i,j, prob = (j/number_of_states)^1)
  m = m %>% set_fancy_layout(igraph::layout_in_circle)
  return(m)

}


#' Random walk as a MC
#'
#' @param p the probability of the step to the right
#' @param reflecting whether the states T and -T should be reflecting (otherwise absorbing)
#' @param with_dots whether the states T and -T should be replaced by dots (to look good in lecture notes)
#'
#' @return
#' @export
#'
#' @examples
#' m = random_walk()
#' plot(m)
random_walk <- function(T=3, p=0.5, reflecting = FALSE, with_dots = FALSE) {
  m = new_mc(name="Random Walk")
  if (with_dots)
    reflecting = FALSE

  # states
  for (i in 1:(2*T+1))
    m = m %>% add_state( label = paste(i - T - 1), x=0.7*(i-T-1), y=0 )

  # transitions
  if (reflecting) {
    m = m %>%
      add_edge_by_indices(1, 2, prob=1) %>%
      add_edge_by_indices(length(m), length(m)-1, prob=1)
  } else {
    if (!with_dots) {
      m = m %>%
        add_edge_by_indices(1, 1, prob=1, loop_angle=-pi/2) %>%
        add_edge_by_indices(length(m), length(m), prob=1, loop_angle=-pi/2)
    } else {
      m = m %>%
        add_edge_by_indices(1, 2, prob=p) %>%
        add_edge_by_indices(length(m), length(m)-1, prob= 1-p)
    }
  }

  for (i in 2:(length(m)-1)) {
    m = m %>%
      add_edge_by_indices(i,  i + 1, prob = p) %>%
      add_edge_by_indices(i, i - 1, prob = 1-p)
  }

  # with_dots options for lecture notes
  if (with_dots)
    m = m %>%
      set_state_properties_by_index(1, shape="none") %>%
      set_state_properties_by_index(1, label="...") %>%
      set_state_properties_by_index(length(m), shape="none") %>%
      set_state_properties_by_index(length(m), label="...")

  # layout
  m = m %>%
    stretch(1,1) %>%
    curve_overlapping_edges %>%
    set_fancy_edge_colors %>%
    set_absorbing_state_color

  return(m)
}

#' Deterministically monotone markov chain
#'
#' @return
#' @export
#'
#' @examples
deterministically_monotone = function(T=6) {
  m = new_mc(name="Deterministically Monotone Chain")
  for (i in 1:T) {
    m = m %>% add_state(paste(i-1), x = i-1, y=0)
  }
  m = m %>% add_state("...", x = T, y=0, shape="none")

  for (i in 1:T) {
    m = m %>% add_edge_by_indices(i, i+1, prob=1)
  }

  # layout
  m = m %>%
    shift(-T/2,0) %>%
    stretch(0.5,1)

  return(m)
}



#' Markov chain with two attached cycles
#'
#' @param n1 size of the first cycle
#' @param n2 size of the second cycle
#' @param pA probability of choosing cycle 1
#' @param tail whether there is a short tail
#'
#' @return mc
#' @export
#'
#' @examples
attached_cycles <- function(n1 = 4, n2 = 6, pA = 0.4, tail = FALSE) {

  if (n1 <= 1 | n2 <= 1)
    stop("Cycles need to be at least 2 states in length")

  m=new_mc(name="Attached_Cycles")
  m = m %>% add_state("1")
  for (i in (2:n1))
    m = m %>%  add_state(paste0("A",i))
  for (i in (2:n2))
    m = m %>% add_state(paste0("B",i))

  m = m %>% add_edge_by_labels("1","A2", prob = pA)
  m = m %>% add_edge_by_labels("1","B2", prob = 1-pA)
  m = m %>% add_edge_by_labels(paste0("A",n1),"1")
  m = m %>% add_edge_by_labels(paste0("B",n2),"1")

  if ( n1 > 2 )
    for (i in (2:(n1-1)))
      m = m %>% add_edge_by_labels(paste0("A",i), paste0("A",i+1))

  if ( n2 > 2 )
    for (i in (2:(n2-1)))
      m = m %>% add_edge_by_labels(paste0("B",i), paste0("B",i+1))


  # adding a tail
  if ( tail ) {
    m = m %>% add_state("C2")
    m = m %>% add_edge_by_labels("1","C2", prob = 0.5)
    m = m %>% add_edge_by_labels("C2","C2", loop_angle = -pi/2)

    m = m %>% set_edge_properties_by_labels("1","A2",
                                            prob = pA/2,label=paste(pA/2))
    m = m %>% set_edge_properties_by_labels("1","B2",
                                            prob = 0.5-pA/2, label=paste(0.5-pA/2))
  }

  # # layout
  m = m %>%
    set_fancy_layout %>%
    rotate(pca=TRUE) %>%
    stretch(1.6,1.6) %>%
    shift(0,0) %>%
    set_fancy_edge_colors %>%
    set_absorbing_state_color %>%
    set_graphics_parameters(
      vertex.size = 30
    )

  return(m)
}



#' Gambler's ruin markov chain
#'
#' @param a desired winnings
#' @param p probability of winning 1 $
#'
#' @return
#' @export
#'
#' @examples
gamblers_ruin <- function(a = 5, p = 0.25) {
  if (a <= 2)
    stop("a must be at least 3")
  if (p <= 0 | p >= 1)
    stop("p must be in (0,1)")

  m = new_mc(name = paste0("Gambler's Ruin, a=", a))

  # states
  for (i in 1:(a + 1))
    m = m %>% add_state(paste(i - 1), x = 2 * i - a - 1, y = 0)

  # edges
  for (i in 2:a) {
    m = m %>%
      add_edge_by_indices(i, i - 1, prob = 1 - p) %>%
      add_edge_by_indices(i, i + 1, prob = p)
  }
  m = m %>%
    add_edge_by_labels("0", "0", loop_angle = pi) %>%
    add_edge_by_labels(paste(a), paste(a), loop_angle = 0)

  # Layout
  m = m %>%
    shift(-1.5, 0) %>%
    stretch(0.3, 1) %>%
    curve_overlapping_edges %>%
    set_absorbing_state_color %>%
    set_fancy_edge_colors  %>%
    set_graphics_parameters(vertex.size = 20)

  return(m)
}

#' The regime-swithcin Markov chain
#'
#' @param p12 probability of the transition 1->2
#' @param p21 probability of the transition 2->1
#'
#' @return
#' @export
#'
#' @examples
regime_switching = function(p12 = 0.4, p21 = 0.7) {
  # states
  m = new_mc(name = "Regime Switching") %>%
    add_state("1", x = -0.5, y = 0) %>%
    add_state("2", x = 0.5, y = 0)

  # edges
  m = m %>%
    add_edge_by_indices(1, 1, prob = 1 - p12, loop_angle = pi) %>%
    add_edge_by_indices(1, 2, prob = p12) %>%
    add_edge_by_indices(2, 1, prob = p21) %>%
    add_edge_by_indices(2, 2, prob = 1 - p21)

  # layout
  m = m %>%
    curve_overlapping_edges %>%
    set_fancy_edge_colors(nbin = 4)

  return(m)

}




# professor <- function(p_morning=0.05, p_afternoon = 0.20) {
#
#   # constants
#   q_morning = 1-p_morning
#   q_afternoon = 1-p_afternoon
#   p_morning_label = "p_m"
#   q_morning_label = "q_m"
#   p_afternoon_label = "p_a"
#   q_afternoon_label = "q_a"
#
#   # states
#   m = new_mc(name = "Professor") %>%
#     add_state("0,5,H", x = -5, y = -1) %>%
#     add_state("1,4,H", x = -3, y = -1) %>%
#     add_state("2,3,H", x = -1, y = -1) %>%
#     add_state("3,2,H", x =  1, y = -1) %>%
#     add_state("4,1,H", x =  3, y = -1) %>%
#     add_state("5,0,H", x =  5, y = -1) %>%
#     add_state("0,5,W", x = -5, y = 1) %>%
#     add_state("1,4,W", x = -3, y = 1) %>%
#     add_state("2,3,W", x = -1, y = 1) %>%
#     add_state("3,2,W", x =  1, y = 1) %>%
#     add_state("4,1,W", x =  3, y = 1) %>%
#     add_state("5,0,W", x =  5, y = 1) %>%
#     add_state("Wet",   x = -6, y = 0)
#
#   m = m %>%
#     add_edge_by_labels("0,5,H", "Wet", p_morning)
#
#   m$layout = m$layout %>%
#     stretch(0.5,0.5) %>%
#     set_graphics_parameters(
#     vertex.size = 35
#   )
#
#   return(m)
# }

#' The tennis chain
#'
#' @param p probability of winning for player 1 in a single rally
#'
#' @return
#' @export
#'
#' @examples
tennis <- function(p=0.4) {

  ps = matrix(
    c("0-0","15-0",
      "0-15","15-15",
      "15-0","30-0",
      "0-30","15-30",
      "15-15","30-15",
      "30-0","40-0",
      "0-40","15-40",
      "15-30","30-30",
      "30-15","40-15",
      "40-0","P1",
      "15-40","30-40",
      "30-30","40-30",
      "40-15","P1",
      "40-30","P1",
      "30-40","40-40",
      "40-40","A-40",
      "40-A","40-40",
      "A-40","P1"), ncol=2, byrow=TRUE)

  qs = matrix(
    c("0-0","0-15",
      "0-15","0-30",
      "15-0","15-15",
      "0-30","0-40",
      "15-15","15-30",
      "30-0","30-15",
      "0-40","P2",
      "15-30","15-40",
      "30-15","30-30",
      "40-0","40-15",
      "15-40","P2",
      "30-30","30-40",
      "40-15","40-30",
      "40-30","40-40",
      "30-40","P2",
      "40-40","40-A",
      "40-A","P2",
      "A-40","40-40"), ncol=2, byrow=TRUE)

  # states
  state_labels = unique(c(ps,qs))
  m = new_mc(name = "Tennis") %>%
    add_states(state_labels)

  # edges
  for (i in 1:dim(qs)[1])
    m = m %>% add_edge_by_labels(qs[i,1], qs[i,2], prob = 1-p)


  for (i in 1:dim(ps)[1])
    m = m %>% add_edge_by_labels(ps[i,1], ps[i,2], prob = p)


  m = m %>%
    add_edge_by_labels("P1","P1",loop_angle=-45) %>%
    add_edge_by_labels("P2","P2",loop_angle=45)

  # layout
  m = m %>%
    set_fancy_layout %>%
    curve_overlapping_edges %>%
    set_fancy_edge_colors %>%
    set_absorbing_state_color %>%
    autoscale %>%
    rotate(-1.362 * pi) %>%
    stretch(1.3,-1) %>%
    stretch(0.8, 0.8) %>%
    shift(0,0)
  return(m)
}

#' Chain from the problem "deck22"
#'
#' @return
#' @export
#'
#' @examples
deck22 = function() {

  # States
  m = new_mc(name = "Deck-2-2") %>%
    add_states(c("22", "21b", "12r", "11r", "11b", "02r",
             "01r", "10b", "01b", "00"))

  # Edges
  m = m  %>%
    add_edge_by_labels("22", "21b", prob = 1 / 2) %>%
    add_edge_by_labels("22", "12r", prob = 1 / 2) %>%
    add_edge_by_labels("21b", "21b", prob = 1 / 3, loop_angle = -pi / 2) %>%
    add_edge_by_labels("21b", "11r", prob = 2 / 3) %>%
    add_edge_by_labels("12r", "11b", prob = 2 / 3) %>%
    add_edge_by_labels("12r", "02r", prob = 1 / 3) %>%
    add_edge_by_labels("11r", "01r", prob = 1 / 2) %>%
    add_edge_by_labels("11r", "10b", prob = 1 / 2) %>%
    add_edge_by_labels("11b", "11b", prob = 1 / 2, loop_angle = pi / 2) %>%
    add_edge_by_labels("11b", "01r", prob = 1 / 2) %>%
    add_edge_by_labels("02r", "01b", prob = 1) %>%
    add_edge_by_labels("01r", "00", prob = 1) %>%
    add_edge_by_labels("10b", "00", prob = 1) %>%
    add_edge_by_labels("01b", "01b", prob = 1, loop_angle = -pi / 2) %>%
    add_edge_by_labels("00", "00", prob = 1)

  # Layout
  m = m %>%
    set_fancy_layout %>%
    set_absorbing_state_color %>%
    set_fancy_edge_colors %>%
    rotate(pi/3) %>%
    set_graphics_parameters(
    vertex.size = 30
  )

  return(m)
}
