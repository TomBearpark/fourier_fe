Psi_j <- function(s, j) sqrt(2) * cos(j * pi * s)

get_plot_freqs <- function(maxQ, s) {
  map_dfr(0:maxQ, function(jj)
    tibble(s = s)  %>% 
      mutate(j = !!jj, psi = Psi_j(j = jj, s = s), 
             plot_psi = psi + (15 - !!10*jj))
  )
}

add_fitted_vals <- function(df, maxQ, var = "growthWDI"){
  
  s <- seq(0,1,length.out = dim(df)[1])

  plot_freqs <- get_plot_freqs(s = s, maxQ = maxQ) 
  
  reg_df <- pivot_wider(select(plot_freqs, -plot_psi), 
                        names_from = "j", names_prefix = "psi", 
                        values_from = "psi")
  
  reg_df <- bind_cols(df, reg_df)
  m <- lm(as.formula(paste0(var, "~ ", 
                            paste("psi", 0:maxQ, collapse = "+", sep = ""))),
          data = reg_df)
  
  bind_cols(df, yhat = m$fitted.values, residual = m$residuals, Q = maxQ)
  
}