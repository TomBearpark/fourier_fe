library(dplyr); library(tidyr)

# Time series utility functions -------------------------------------------

gen_ts <- function(df, newvar, beta = 0.8){
  df[newvar]   <- NA
  df[1,newvar] <- 0
  N <- length(df[[newvar]])
  for(ii in 2:N) df[ii, newvar] <- beta * df[ii - 1, newvar] + rnorm(1) 
  df %>% 
    group_by(year) %>% 
    mutate(!!newvar := .data[[newvar]] + rnorm(n = 1, sd = 2)) %>% 
    ungroup()
}


# Mr Mullers Functions ----------------------------------------------------


Psi_j <- function(s, j) sqrt(2) * cos(j * pi * s)

get_plot_freqs <- function(maxQ, s) {
  map_dfr(1:maxQ, function(jj)
    tibble(s = s)  %>% 
      mutate(j = !!jj, psi = Psi_j(j = jj, s = s), 
             plot_psi = psi + (15 - !!10*jj))
  )
}

add_fitted_vals <- function(df, maxQ, var = "growthWDI", plot_freqs = NULL){
  
  s <- seq(0,1,length.out = dim(df)[1])

  if(is.null(plot_freqs)) plot_freqs <- get_plot_freqs(s = s, maxQ = maxQ) 
  
  reg_df <- pivot_wider(select(plot_freqs, -plot_psi), 
                        names_from = "j", names_prefix = "psi", 
                        values_from = "psi")
  
  reg_df <- bind_cols(df, reg_df)
  m <- lm(as.formula(paste0(var, "~ ", 
                            paste("psi", 1:maxQ, collapse = "+", sep = ""))),
          data = reg_df)
  
  bind_cols(df, yhat = m$fitted.values, residual = m$residuals, Q = maxQ)
  
}