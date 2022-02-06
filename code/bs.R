# bootstrap the 

pacman::p_load('tidyverse', 'fixest', 'lubridate', 'patchwork')
theme_set(theme_bw())
set.seed(123)
if(Sys.info()['user'] == "tombearpark") {
  dir <- "/Users/tombearpark/Documents/GitHub/fourier_fe/code"
  out <- "/Users/tombearpark/Dropbox/Apps/Overleaf/What do Fixed Effects do to your Data?/images"
}else{
  stop("need to set up your paths!")
}
source(file.path(dir, "0_funcs.R"))

dates   <- 
  expand_grid(unit = 1, year = 1950:2010, month = 1:12) %>% 
  mutate(date = make_date(year = year, month = month, day = 1))

# Version 1 ---------------------------------------------------------------

# get_coefs <- function(i, dates){
#   message (i)
#   # Prepare the data 
#   df <- gen_ts(dates, "y", beta = 0.8) %>% 
#     mutate(ly = dplyr::lag(y))
#   df <- df[-1,]
#   
#   # FE reg
#   m1  <- lm(y ~ ly + factor(year), data = df)
#   
#   # Mr Muller reg
#   Q  <- 2 * dim(df)[1] / 12
#   df1 <- add_fitted_vals(df, Q, "y")
#   df2 <- add_fitted_vals(df1, Q, "residual")
#   df2$residual <- df2$residual...11 
#   m2  <- lm(df1$residual ~ -1 + df2$residual)
#   
#   tibble(i = i, 
#          fe = coef(m1)['ly'], 
#          mr_muller = coef(m2)['df2$residual']) 
# }
# 
# 
# dd   <- map_dfr(1:50, get_coefs, dates)
# dd   %>% pivot_longer(cols= -i) %>% ggplot() + geom_density(aes(x = value, color = name))

# Version 2 ---------------------------------------------------------------

# Get the fixed effects twice using two different types 

get_coefs <- function(i, dates){
  message (i)
  # Simulate the data 
  df <- gen_ts(dates, "x", beta = 0.8) %>% 
    mutate(y = 0.7 * x + rnorm(n = dim(dates)[1], sd = 2))
  # Get coefficients from fixed effects regression. 
  m1 <- lm(y  ~ x + factor(year), data = df)

  # Recover a similar estimate using the Fourier transforms 
  Q  <- 2 * dim(df)[1] / 12
  df1 <- add_fitted_vals(df, Q, "y")
  df2 <- add_fitted_vals(df, Q, "x")
  m2  <- lm(df1$residual ~ -1 + df2$residual)
  # Organise outputs 
  tibble(i = i, 
         fe = coef(m1)['x'], 
         mr_muller = coef(m2)['df2$residual'])
}


dd <- map_dfr(1:100, get_coefs, dates)
dd %>% pivot_longer(cols= -i) %>% ggplot() + geom_density(aes(x = value, color = name))
ggsave(paste0(out, "/ts_fe_mm_equivalence.png"), height = 4, width = 7)
