#LINEAR PROJECTIONS
  #Linear
  linear_proj <- function (x, y, t_l, p_d) {
    lin.mod <- lm(y ~ x)
    #lin.pre <- predict(lin.mod, newdata = data.frame(x = seq(t_l+1,t_l+p_d)))
    return(lin.mod)
  }
  
  #Quadratic
  quad_proj <- function (x, y, t_l, p_d) {
    qua.mod <- lm(formula = y ~ poly(x,2))
    #qua.pre <- predict(qua.mod, newdata = data.frame(x = seq(t_l,t_l+p_d)))    
    return(qua.mod)
  }
  
  #Cubic
  cube_proj <- function(x, y, t_l, p_d) {
    cub.mod <- lm(formula = y ~ poly(x,3))
    #cub.pre <- predict(cub.mod, newdata = data.frame(x = seq(t_l,t_l+p_d)))
    return(cub.mod)
  }
  
  #Simple Exponential
  s_exp <- function(x, y, t_l, p_d) {
    s_e.0 <- lm(log(y[2:length(y)]/log(y[1])) ~ x[2:length(x)])
    start <- list(a=exp(coef(s_e.0)[1]), b=coef(s_e.0)[2], c = y[length(y)])
    
    s_e.mod <- nls2(y ~ c * exp( b * x ), data = data.frame(y), start = start, algorithm = "brute-force")
    #s_e.pre <- predict(s_e.mod, newdata = data.frame(x = seq(1,p_d)))    
    return(s_e.mod)
  }
  
#NON-LINEAR PROJECTIONS  
  #Simple Geometric
  s_g <- function(x, y, t_l, p_d) {
    s_ge.0 <- lm((log(y[2:length(y)]-(y[1])) ~ x[2:length(x)]))
    start <- list(a=exp(coef(s_ge.0)[1]), b=coef(s_ge.0)[2], c = y[length(y)]*1.5)
    
    s_ge.mod <- nls2(y ~ a * (1 + b)^x + c, data = data.frame(y), start = start, algorithm = "brute-force")
    #s_ge.pre <- predict(s_ge.mod, newdata = data.frame(x = seq(1,p_d))) 
    return(s_ge.mod)
  }
  
  #Modified Exponential
  m_e <- function(x, y, t_l, p_d) {
    m_e.0 <- lm(log(y[2:length(y)]/log(y[1])) ~ x[2:length(x)])
    start <- list(a=exp(coef(m_e.0)[1]), b=coef(m_e.0)[2], c = y[length(y)]*1.5)
    
    m_e.mod <- nls2(y ~ c+(a * exp( b * x )), data = data.frame(y), start = start, algorithm = "brute-force")
    #m_e.pre <- predict(m_e.mod, newdata = data.frame(x = seq(1,p_d)))   
    return(m_e.mod)
  }
  
  #Logistic
  lo <- function(x, y, t_l, p_d) {
    c.0 = 1/(1.5*y[length(y)])
    lo.0 <- lm(log((1/y) - c.0) ~ x)
    start <- data.frame(a = exp(coef(lo.0)[1]), b = exp(coef(lo.0)[2]), c = c.0)
    
    lo.mod <- nls2(y ~ 1/(c+a*(b^x)), data.frame(y), start = start, algorithm = "brute-force")
    #lo.pre <- predict(lo.mod, newdata = data.frame(x = seq(1,p_d))) 
    return(lo.mod)
  }

#RATIO FORCASTS  
  #Constant Share
  c_s <- function(a, a_j, b) {
    c_sp <- (a$pop[length(a$pop)]/a_j$pop[length(a_j$pop)])*b
    return(c_sp)
  }
  
  #Shift Share
  s_s <- function (a, a_j, b, p_d){
    s_l <- a$pop[length(a$pop)]/a_j$pop[length(a_j$pop)] #launch share
    d_s <- (a$pop[length(a$pop)]/a_j$pop[length(a_j$pop)])-(a$pop[1]/a_j$pop[1]) #difference of share
    i_t <- 1/a$year[length(a$year)] #1/time
    
    s_sp <- (s_l+(d_s*i_t*p_d))*b
    return(s_sp)
  }
  
  #Share of Growth
  s_gr <- function (a, a_j, b){
    d_i <- a$pop[length(a$pop)] - a$pop[1]
    d_j <- a_j$pop[length(a_j$pop)] - a_j$pop[1]
    d_t <- b - a_j$pop[length(a_j$pop)]
    
    s_gp <- a$pop[length(a$pop)] + (d_i/d_j)*(d_t)
    return(s_gp)
  }