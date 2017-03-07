#SOURCE CODE FOR ALL PROJECTION FUNCTIONS USED FOR CP6024
#ASSIGNMENT 2 

#Simple Linear
smp_linear <- function(pop_base, pop_launch, years, proj_years) {
  rate <- (pop_launch - pop_base)/years
  pp <- (rate*proj_years)+pop_launch
  
  return(data.frame(population = pp, rate = rate))
}

#Simple Geometric
smp_geom <- function(pop_base, pop_launch, years, proj_years) {
  rate <- log(pop_launch/pop_base)*(1/years)
  pp <- pop_launch * exp(rate*proj_years)
  
  return(data.frame(population = pp, rate = rate))
}

#Geometric
geometric <- function(pop_base, pop_launch, years, proj_years) {
  rate <- ((pop_launch/pop_base)^(1/years))-1
  pp <- pop_launch*(1+rate)^proj_years
  
  return(data.frame(population = pp, rate = rate))
}

#Line Fit Projections (Linear, Quadratic, Cubic)
#Linear Fit
line_proj <- function(a_pop, pop_launch, years, proj_years) {
  t  <- years
  line_mod <- lm(a_pop ~ t)
  
  s <- data.frame(summary(line_mod)$coefficients, offset = pop_launch)
  
  rate = s$Estimate[2]
  pp <- s$Estimate[1] + s$Estimate[2]*proj_years

  return(data.frame(population = pp, rate = rate))
}

#Quadratic Fit
quad_proj <- function(a_pop, pop_launch, years, proj_years){
  t  <- years
  t2 <- t^2
  line_mod <- lm(a_pop ~ t+t2)
  
  s <- data.frame(summary(line_mod)$coefficients, offset = pop_launch)
  
  rate = NA
  pp <- s$Estimate[1] + s$Estimate[2]*proj_years + s$Estimate[3]*proj_years^2
  
  return(data.frame(population = pp, rate = rate))  
}

#Cubic Fit
cube_proj <- function(a_pop, pop_launch, years, proj_years){
  t  <- years
  t2 <- t^2
  t3 <- t^3
  line_mod <- lm(a_pop ~ t+t2+t3)
  
  s <- data.frame(summary(line_mod)$coefficients, offset = pop_launch)
  
  rate = NA
  pp <- s$Estimate[1] + s$Estimate[2]*proj_years + s$Estimate[3]*proj_years^2 + s$Estimate[4]
  
  return(data.frame(population = pp, rate = rate))  
}
#Simple Exponential 
smp_exp <- function(pop_base, pop_launch, years, proj_years) {
  rate <- log(pop_launch/pop_base)*(1/years)
  pp <- pop_launch * exp(rate*proj_years)
  
  return(data.frame(population = pp, rate = rate))
}
#Modified Exp
mod_exp <- function(pop_base, pop_launch, years, proj_years, pop_time) {
    rate <- log(pop_launch/pop_base)*(1/years)
    pp <- (1.5*pop_launch)+pop_launch*rate^(proj_years)
    
    return(data.frame(population = pp, rate = rate))
}
#Logistic
logistic <- function(pop_base, pop_launch, years, proj_years) {
  a <- 1.5*pop_launch
  b <- years
  rate <- (log((1.5*pop_base)/pop_launch)-1)/b
  pp <- a/(1+exp(-rate*years))
  
  return(data.frame(population = pp, rate = rate))
}
#Constant Share
  c_share <- function(pop_j_t, pop_j_l, pop_i_l) {
  pp <- (pop_i_l/pop_j_l)*pop_j_t
  
  return(data.frame(population = pp, rate = (pop_i_l/pop_j_l)))
}
#Shift Share
s_share <- function(pop_j_b, pop_j_l, pop_j_t, pop_i_b, pop_i_l, years, proj_years) {
  a <- (pop_i_l/pop_j_l)
  b <- (pop_i_l/pop_j_l)-(pop_i_b/pop_j_b)
  y <- 1/years
  z <- proj_years
  pp <- (a + ((b*y)*z))*pop_j_t
  
  return(data.frame(population = pp, rate = a))
}
#Growth Share
g_share <- function(pop_j_b, pop_j_l, pop_j_t, pop_i_b, pop_i_l) {
  a <- pop_i_l-pop_i_b
  b <- pop_j_l-pop_j_b
  c <- pop_j_t-pop_j_l
  pp <- pop_i_l +((a/b)*c)
  
  print(b)
  
  return(data.frame(population = pp,rate = (a/b)))
}