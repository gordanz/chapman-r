# MC Examples -------------------------------------------------------------

color_tester <- function(n) {
  P = matrix( (1:n)/n, ncol=n, nrow=n, byrow=F)
  m = markov_chain(P) %>%
    set_auto_layout(igraph::layout_in_circle)
  return(m)
}

#' Random walk
#'
#' @param T the random walk goes from -T to T
#' @param p the probability of the step to the right
#' @param reflecting whether the states T and -T should be reflecting (otherwise absorbing)
#' @param with_dots whether the states T and -T should be replaced by dots (to look good in lecture notes)
#'
#' @return a markov chain object
#' @export
#'
#' @examples
#' m = random_walk()
#' plot(m)
random_walk <- function(T=3, p=0.5, reflecting = FALSE, with_dots = FALSE) {

  m = markov_chain()

  if (with_dots)
    reflecting = FALSE

  # states
  for (i in 1:(2*T+1))
    m = m %>% add_state(paste(i - T - 1), x=0.7*(i-T-1)-0.15, y=0 )

  # transitions
  if (reflecting) {
    m = m %>%
      add_edge(1, 2, prob=1) %>%
      add_edge(length(m), length(m)-1, prob=1)
  } else {
    if (!with_dots) {
      m = m %>%
        add_edge(1, 1, prob=1, loop_angle=-pi/2) %>%
        add_edge(length(m), length(m), prob=1, loop_angle=-pi/2)
    } else {
      m = m %>%
        add_edge(1, 2, prob=p) %>%
        add_edge(length(m), length(m)-1, prob= 1-p)
    }
  }

  for (i in 2:(length(m)-1)) {
    m = m %>%
      add_edge(i,  i + 1, prob = p) %>%
      add_edge(i, i - 1, prob = 1-p)
  }

  # with_dots options for lecture notes
  if (with_dots)
    m = m %>%
      set_state(c(1,length(m)), shape="none", label="...")

  # layout
  m = m %>%
    stretch(1,1) %>%
    curve_overlapping_edges %>%
    set_auto_edge_colors %>%
    set_absorbing_state_color

  return(m)
}

#' Deterministically monotone markov chain
#'
#' @param T the position of the state cut-off ("...")
#' @return a markov_chain object
#' @export
deterministically_monotone = function(T=6) {
  m = markov_chain()
  for (i in 1:T) {
    m = m %>%
      add_state(paste(i-1), x = i-1, y=0)
  }
  m = m %>%
    add_state("...", x = T, y=0, shape="none")

  for (i in 1:T) {
    m = m %>%
      add_edge(i, i+1)
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
#' @return a markov_chain object
#' @export
attached_cycles <- function(n1 = 4, n2 = 6, pA = 0.4, tail = FALSE) {

  if (n1 <= 1 | n2 <= 1)
    stop("Cycles need to be at least 2 states in length")

  m = markov_chain(1)

  for (i in (2:n1))
    m = m %>%  add_state(paste0("A",i))
  for (i in (2:n2))
    m = m %>% add_state(paste0("B",i))

  m = m %>% add_edge("1","A2", prob = pA) %>%
    add_edge("1","B2", prob = 1-pA) %>%
    add_edge(paste0("A",n1),"1") %>%
    add_edge(paste0("B",n2),"1")

  if ( n1 > 2 )
    for (i in (2:(n1-1)))
      m = m %>% add_edge(paste0("A",i), paste0("A",i+1))

  if ( n2 > 2 )
    for (i in (2:(n2-1)))
      m = m %>% add_edge(paste0("B",i), paste0("B",i+1))

  # adding a tail
  if (tail) {
    m = m %>%
      add_state("C2") %>%
      add_edge("1", "C2", prob = 0.5) %>%
      add_edge("C2", "C2", loop_angle = -pi/2) %>%
      set_edge("1", "A2", prob = pA/2, label = paste(pA/2)) %>%
      set_edge("1", "B2", prob = 0.5 - pA / 2, label = paste(0.5 - pA/2))
  }

  # # layout
  m = m %>%
    set_auto_layout %>%
    rotate(pca=TRUE) %>%
    stretch(1.6,1.6) %>%
    shift(0,0) %>%
    set_auto_edge_colors %>%
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
#' @return a markov_chain object
#' @export
gamblers_ruin <- function(a = 5, p = 0.25) {
  if (a <= 2)
    stop("a must be at least 3")
  if (p <= 0 | p >= 1)
    stop("p must be in (0,1)")

  m = markov_chain()

  # states
  for (i in 1:(a + 1))
    m = m %>% add_state(paste(i - 1), x = 2 * i - a - 1, y = 0)

  # edges
  for (i in 2:a) {
    m = m %>%
      add_edge(i, i - 1, prob = 1 - p) %>%
      add_edge(i, i + 1, prob = p)
  }
  m = m %>%
    add_edge("0", "0", loop_angle = pi) %>%
    add_edge(paste(a), paste(a), loop_angle = 0)

  # Layout
  m = m %>%
    shift(-1.5, 0) %>%
    stretch(0.3, 1) %>%
    curve_overlapping_edges %>%
    set_absorbing_state_color %>%
    set_auto_edge_colors

  return(m)
}

#' The regime-switching Markov chain
#'
#' @param p12 probability of the transition 1->2
#' @param p21 probability of the transition 2->1
#'
#' @return a markov_chain object
#' @export
regime_switching = function(p12 = 0.4, p21 = 0.7) {
  # states
  m = markov_chain(name = "Regime Switching") %>%
    add_state("1", x = -0.5, y = 0) %>%
    add_state("2", x = 0.5, y = 0)

  # edges
  m = m %>%
    add_edge(1, 1, prob = 1 - p12, loop_angle = pi) %>%
    add_edge(1, 2, prob = p12) %>%
    add_edge(2, 1, prob = p21) %>%
    add_edge(2, 2, prob = 1 - p21)

  # layout
  m = m %>%
    curve_overlapping_edges %>%
    set_auto_edge_colors(nbins = 4)

  return(m)

}




#' A Markov Chain for the problem with the professor and umbrellas
#'
#' @param p_morning probability of getting wet in the morning
#' @param p_afternoon probability of getting wet in the afternoon
#'
#' @return a markov_chain object
#' @export
#'
professor <- function( p_h=0.05, p_o = 0.20, split_wet=FALSE) {

  # constants
  q_h = 1-p_h
  q_o = 1-p_o
  p_h_label = paste(p_h)
  q_h_label = paste(1-p_h)
  p_o_label = paste(p_o)
  q_o_label = paste(1-p_o)

  # states
  m = markov_chain() %>%
    add_state("h0-4") %>%
    add_state("h1-3") %>%
    add_state("h2-2") %>%
    add_state("h3-1") %>%
    add_state("h4-0") %>%
    add_state("0-4o") %>%
    add_state("1-3o") %>%
    add_state("2-2o") %>%
    add_state("3-1o") %>%
    add_state("4-0o")
  if (split_wet) {
    m = m %>%
      add_state("Wet-o") %>%
      add_state("Wet-h")
  } else {
    m = m %>%
      add_state("Wet")
  }


  m = m %>%
    add_edge("h0-4", "0-4o", q_h) %>%
    add_edge("h1-3", "0-4o", p_h) %>%
    add_edge("h1-3", "1-3o", q_h) %>%
    add_edge("h2-2", "1-3o", p_h) %>%
    add_edge("h2-2", "2-2o", q_h) %>%
    add_edge("h3-1", "2-2o", p_h) %>%
    add_edge("h3-1", "3-1o", q_h) %>%
    add_edge("h4-0", "3-1o", p_h) %>%
    add_edge("h4-0", "4-0o", q_h) %>%
    add_edge("0-4o", "h0-4", q_o) %>%
    add_edge("0-4o", "h1-3", p_o) %>%
    add_edge("1-3o", "h1-3", q_o) %>%
    add_edge("1-3o", "h2-2", p_o) %>%
    add_edge("2-2o", "h2-2", q_o) %>%
    add_edge("2-2o", "h3-1", p_o) %>%
    add_edge("3-1o", "h3-1", q_o) %>%
    add_edge("3-1o", "h4-0", p_o) %>%
    add_edge("4-0o", "h4-0", q_o)

  if (split_wet) {
    m = m %>%
      add_edge("h0-4", "Wet-h", p_h) %>%
      add_edge("4-0o", "Wet-o", p_o) %>%
      add_edge("Wet-h","Wet-h", loop_angle = pi/4) %>%
      add_edge("Wet-o","Wet-o", loop_angle = pi/4)
  } else {
    m = m %>%
      add_edge("h0-4", "Wet", p_h) %>%
      add_edge("4-0o", "Wet", p_o) %>%
      add_edge("Wet","Wet", loop_angle = pi/4)
  }




  m = m %>%
    set_auto_layout %>%
    rotate(pca=TRUE) %>%
    set_auto_edge_colors(nbin=4) %>%
    curve_overlapping_edges %>%
    set_graphics_parameters(
      vertex.shape = "rectangle"
      )


  return(m)
}

#' The tennis chain
#'
#' @param p probability of winning for player 1 in a single rally
#'
#' @return a markov_chain object
#' @export
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
  m = markov_chain(name = "Tennis") %>%
    add_state(state_labels)

  # edges
  for (i in 1:dim(qs)[1])
    m = m %>% add_edge(qs[i,1], qs[i,2], prob = 1-p)

  for (i in 1:dim(ps)[1])
    m = m %>% add_edge(ps[i,1], ps[i,2], prob = p)

  m = m %>%
    add_edge("P1","P1",loop_angle=-45) %>%
    add_edge("P2","P2",loop_angle=45)

  # layout
  m = m %>%
    set_auto_layout %>%
    curve_overlapping_edges %>%
    set_auto_edge_colors %>%
    set_absorbing_state_color %>%
    set_graphics_parameters(vertex.label.cex = 0.7,
                            vertex.size = 32) %>%
    autoscale %>%
    rotate(-1.362 * pi) %>%
    stretch(1.3,1) %>%
    stretch(1, -1)
  return(m)
}

#' Chain from the problem "deck22"
#'
#' @return a markov_chain object
#' @export

deck22 = function() {

  # States
  m = markov_chain() %>%
    add_state(c("22", "21b", "12r", "11r", "11b", "02r",
             "01r", "10b", "01b", "00"))

  # Edges
  m = m  %>%
    add_edge("22", "21b", prob = 1 / 2) %>%
    add_edge("22", "12r", prob = 1 / 2) %>%
    add_edge("21b", "21b", prob = 1 / 3, loop_angle = -pi / 2) %>%
    add_edge("21b", "11r", prob = 2 / 3) %>%
    add_edge("12r", "11b", prob = 2 / 3) %>%
    add_edge("12r", "02r", prob = 1 / 3) %>%
    add_edge("11r", "01r", prob = 1 / 2) %>%
    add_edge("11r", "10b", prob = 1 / 2) %>%
    add_edge("11b", "11b", prob = 1 / 2, loop_angle = pi / 2) %>%
    add_edge("11b", "01r", prob = 1 / 2) %>%
    add_edge("02r", "01b", prob = 1) %>%
    add_edge("01r", "00", prob = 1) %>%
    add_edge("10b", "00", prob = 1, loop_angle = -pi/2) %>%
    add_edge("01b", "01b", prob = 1, loop_angle = pi / 2) %>%
    add_edge("00", "00", prob = 1)

  # Layout
  m = m %>%
    set_auto_layout %>%
    set_absorbing_state_color %>%
    set_auto_edge_colors %>%
    rotate(pi/3)
  return(m)
}

#' A Markov chain that tracks the pattern HTH
#' in a series of coin tosses
#'
#' @return a markov_chain object
#' @export
pattern_HTH = function() {
  m = markov_chain() %>%
    add_state("0",x=0, y=0) %>%
    add_state("H",x=1, y=0) %>%
    add_state("HT",x=2, y=0) %>%
    add_state("HTH",x=3, y=0) %>%
    add_edge("0","0",1/2, loop_angle=-pi/2) %>%
    add_edge("0","H",1/2) %>%
    add_edge("H","H",1/2, loop_angle=-pi/2) %>%
    add_edge("H","HT",1/2) %>%
    add_edge("HT","0",1/2, curve = 0.6) %>%
    add_edge("HT","HTH",1/2) %>%
    add_edge("HTH","HTH",1, loop_angle=-pi/2) %>%
    curve_overlapping_edges(0.5) %>%
    set_absorbing_state_color %>%
    set_auto_edge_colors
  return(m)
}

#' A Markov chain that tracks the pattern HHH
#' in a series of coin tosses
#'
#' @return a markov_chain object
#' @export
pattern_HHH = function() {
  m = markov_chain() %>%
    add_state("0",x=0, y=0) %>%
    add_state("H",x=1, y=0) %>%
    add_state("HH",x=2, y=0) %>%
    add_state("HHH",x=3, y=0) %>%
    add_edge("0","0",1/2, loop_angle=-pi/2) %>%
    add_edge("0","H", 1/2) %>%
    add_edge("H","0", 1/2) %>%
    add_edge("H","HH", 1/2) %>%
    add_edge("HH","0", 1/2, curve = 0.6) %>%
    add_edge("HH","HHH", 1/2) %>%
    add_edge("HHH","HHH",1, loop_angle=-pi/2) %>%
    set_absorbing_state_color %>%
    curve_overlapping_edges(-0.3) %>%
    set_auto_edge_colors
  return(m)
}

#' Markov chain for the problem with computers breaking down
#'
#' @param p the break-down probability
#'
#' @return a markov_chain object
#' @export
facility = function(p=0.4) {
  markov_chain() %>%
    add_state("0-0-1-1", x=0, y=0) %>%
    add_state("2-0-0-0", x=1, y=0) %>%
    add_state("1-1-0-0", x=1.5, y=0.5) %>%
    add_state("0-1-0-1", x=0, y=1) %>%
    add_state("1-0-1-0", x=1, y=1) %>%
    add_edge("0-0-1-1","0-1-0-1", label="") %>%
    add_edge("2-0-0-0","0-0-1-1", label="p^2", prob = p**2) %>%
    add_edge("2-0-0-0","2-0-0-0", label="(1-p)^2", prob = (1-p)**2) %>%
    add_edge("2-0-0-0","1-0-1-0",label="2p(1-p)", prob = 2* p*(1-p)) %>%
    add_edge("1-1-0-0","2-0-0-0", label="1-p", prob = (1-p)) %>%
    add_edge("1-1-0-0","1-0-1-0",label="p", prob = p) %>%
    add_edge("1-0-1-0","1-1-0-0",label="1-p", prob = (1-p)) %>%
    add_edge("1-0-1-0","0-1-0-1",label="p", prob = p) %>%
    add_edge("0-1-0-1","1-0-1-0", label="", prob = 1) %>%
    curve_overlapping_edges(0.3)
  }

stov = function(s) {
  if (s == "0") {
    return(character(0))
    } else {
      return(unlist(strsplit(s,"")))
    }
}
vtos = function(v) {
  if (length(v) == 0) {
    return("0")
    } else {
      return(paste(v, collapse = ""))
    }
}

is_prefix = function(test, pattern) {
  all( test == pattern[1:length(test)])
}

largest_first = function(test, pattern) {
  while (length(test) > 0) {
    if (is_prefix(test, pattern)){
      return(test)
      } else {
        test = test[-1]
      }
  }
  return(test)
}

#' A pattern-waiting markov chain for an arbitrary pattern
#'
#' @param pattern_string a string of "H" and "T"
#' @param p probability of "H"
#'
#' @return a markov_chain object
#' @export
pattern = function (pattern_string, p=1/2) {
  if (pattern_string == "") {
    stop("Need a nonempty string.")
  }
  pat = stov(pattern_string)
  if (any( ! pat %in% c("H","T"))) {
    stop("Needs a string of Hs and Ts only")
  }
  n = length(pat)
  m = markov_chain() %>%
    add_state("0", x=0, y=0)

  for (i in 1:n){
    m = add_state(m, vtos(pat[1:i]), x=i, y=0 )
  }

  for (i in 1:n) {
    pat_now = stov( m$states$label[i] )
    pat_now_H = c(pat_now,"H")
    pat_now_T = c(pat_now, "T")
    next_H = vtos(largest_first(pat_now_H, pat))
    next_T = vtos(largest_first(pat_now_T, pat))
    m = m %>%
      add_edge(i, next_H, prob = p, loop_angle=-pi/2) %>%
      add_edge(i, next_T, prob = 1-p, loop_angle = -pi/2)
  }
  m = m %>%
    add_edge(n+1,n+1,prob=1, loop_angle = -pi/2)

  m = m %>%
    set_auto_layout(algorithm = igraph::layout_in_circle) %>%
    set_auto_edge_colors() %>%
    set_auto_state_colors() %>%
    set_graphics_parameters(vertex.size = 20+10*(n-1), vertex.label.cex=0.8)


  return(m)

}

