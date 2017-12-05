df <- read_excel('C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Economy/Tax/General Tax/Working Files/Tax_GDP_Comparison.xlsx', sheet = 5)

df <- df %>% separate(Year, c("Year_prior", "Year_after"), sep = "-") %>%
  mutate(Year_prior = as.numeric(Year_prior)) %>%
  select(-Year_after) %>%
  gather(cat, Million, 2:length(.))

t_breakdown <- ggplot(df, aes(x = Year_prior, y = Million, col = cat)) + geom_area(stat = "bin")
