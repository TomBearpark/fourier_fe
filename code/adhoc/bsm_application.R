library(tidyverse)
library(fixest)

user <- "TOM"
if(user == "TOM") db <- "/Users/tombearpark/Dropbox/CILads_DB/"

data <- paste0(db, "/papers/gdp_temp/BurkeHsiangMiguel2015_Replication/data/")
df   <- read_csv(paste0(data, '/input/GrowthClimateDataset.csv')) %>% 
  filter(!is.na(growthWDI), !is.na(UDel_temp_popweight)) %>% 
  mutate(temp    = UDel_temp_popweight, 
                    temp2   = temp * temp, 
                    precip  = UDel_precip_popweight/1000, 
                    precip2 = precip*precip, 
                    time2 = time^2, time3 = time^3, time4 = time^4)

reg <- feols(data = df , 
           growthWDI ~ 1|  year + countryname + countryname[year] + countryname[year^2], 
             panel.id = c('year', 'countryname'), vcov= "twoway")

df <- bind_cols(df, resid = reg$residuals)

country <- "USA"
plot_df <- df %>% filter(iso == country)

s1 <- spectrum(plot_df$growthWDI, log = "no", plot = FALSE)
s2 <- spectrum(plot_df$resid,     log = "no", plot = FALSE)

tibble(country = country, low_f_removed = mean(s1$spec[c(1,2)]) > mean(s2$spec[c(1,2)]))

tibble(freq = s1$freq, period = 1 / freq, 
       growthWDI = s1$spec, filtered = s2$spec) %>%
  pivot_longer(cols = -c(freq, period)) %>% 
  ggplot() + 
  geom_line(aes(x = freq, y = (2*value), color = name)) + 
  ggtitle(country)


check_spec <- function(df, country){
  plot_df <- df %>% filter(iso == country)

  s1 <- spectrum(plot_df$growthWDI, log = "no", plot = FALSE)
  s2 <- spectrum(plot_df$resid,     log = "no", plot = FALSE)
  tibble(country = country, low_f_removed = mean(s1$spec[c(1)]) > mean(s2$spec[c(1)]))
}

d   <- map_dfr(unique(df$iso), check_spec, df = df)
df  <- left_join(df, d, by = c("iso" = "country"))
reg <- feols(data = df , 
             growthWDI ~ temp + temp2 + precip + precip2 |  
               year + countryname + countryname[year] + countryname[year^2], 
             fsplit= ~low_f_removed,
             panel.id = c('year', 'countryname'), vcov= "twoway")
reg



# USA specific  -----------------------------------------------------------


df <- df %>% filter(iso == "USA") %>% 
  select(iso, year, growthWDI, temp, temp2, precip, precip2, time, time2) 

ggplot(df %>% pivot_longer(cols = -c(year, iso))) + 
  geom_line(aes(x = year, y = value, color= iso)) + 
  facet_wrap(~name, scales = "free")

m <- feols(growthWDI ~ time + time2, df)
ts <- df$growthWDI
plot.ts(ts)
ss <- spectrum(ts, log = "no", plot = FALSE)

ss2 <- spectrum(m$residuals, log = "no", plot = FALSE)



# Filtering version -------------------------------------------------------


# 1. Partial out the county and year fixed effects
# 2. Filter each time series to remove variation thats got periodicity of 
    # more than about 3 years. 
# 3. Re-estimate the model 