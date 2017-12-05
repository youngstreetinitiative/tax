#This is a Sandbox

p <- "C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Economy/Tax/General Tax/Working Files/Gov_CollatedSPPBudgetData.xlsx"

test <- readxl::read_xlsx(path = p, sheet = 2)

df <- test %>%
  gather(key = Date, value = `$m`, -`$million`) %>%
  mutate(Date = as.Date(paste0(str_extract(Date, "[0-9]+" ), "-06-01"))) %>%
  `colnames<-`(c("Purpose", "Date", "Millions")) %>%
  mutate(Purpose = as.factor(Purpose))

ggplot(data = df, aes(x = Date, y = Millions)) +
  geom_line(aes(group = Purpose, colour = Purpose)) +
  geom_point(aes(colour = Purpose))


test2 <- readxl::read_xlsx(path = p, sheet = 3)

df2 <- test2 %>%
  gather(key = Date, value = `$m`, -`$million`) %>%
  mutate(Date = as.Date(paste0(str_extract(Date, "[0-9]+" ), "-06-01"))) %>%
  `colnames<-`(c("Purpose", "Date", "Millions")) %>%
  mutate(Purpose = as.factor(Purpose)) %>%
  filter(Purpose != "Total payments for specific purposes")

ggplot(data = df2, aes(x = Date, y = Millions)) +
  geom_line(aes(group = Purpose, colour = Purpose)) +
  geom_point(aes(colour = Purpose))
