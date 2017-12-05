
gstplot <- function(){
  library(ggplot2)

  p <- "C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Economy/Tax/General Tax/Working Files/ABS_5512.0_Tax_CostsRevenue_Comparison.xlsx"
  p2 <- "C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Economy/Tax/General Tax/Working Files/R images/"
  state_list <- c("NT", "ACT", "TAS", "SA", "WA", "QLD", "VIC", "NSW")
  state_list_1 <- c("WA", "QLD", "VIC", "NSW")
  state_list_2 <- c("NT", "ACT", "TAS", "SA")

  raw_read <- readxl::read_xlsx(path = p, sheet = 4)

  df <- raw_read %>%
    mutate(State = factor(State, levels = state_list, ordered = TRUE)) %>%
    gather(key = Money_Flow, value = Amount, -Year, -State, -CPI, -All_Grants, -Estimated_Pop, -GST_Grants, -GST_Relativities, -Estimated_GST_Raised, -Total_Net_financial_Assets) %>%
    mutate(Amount_Real = Amount * (max(CPI)/CPI),
           All_Grants_Real = All_Grants * (max(CPI)/CPI),
           Grant_Prop_Real = All_Grants_Real/Amount_Real,
           Grant_per_Head = (All_Grants_Real * 1000000)/Estimated_Pop,
           GST_Grants_Real = GST_Grants * (max(CPI)/CPI),
           GST_per_Head = (GST_Grants_Real * 1000000)/Estimated_Pop,
           GST_Raised_Real = Estimated_GST_Raised * (max(CPI)/CPI),
           GST_Raised_per_Head = (GST_Raised_Real * 1000000)/Estimated_Pop,
           GST_Diff_per_head = GST_per_Head - GST_Raised_per_Head)

  for (i in 1:length(state_list_1)) {
    print(i)
    z <- df %>% filter(State == state_list_1[i])
    print(z)

    path <- paste("C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Economy/Tax/General Tax/Working Files/R images/",
                  state_list_1[i],
                  " Revenue, Expenses and Grants",
                  ".jpeg",
                  sep = "")

    print(ggplot(data = z, aes(x = Year, y = Amount_Real)) +
      geom_point(aes(colour = Money_Flow)) +
      geom_line(aes(group = Money_Flow, colour = Money_Flow)) +
      geom_bar(data = z %>% filter(Money_Flow == "Total_GFS_Revenue"),
               aes(x = Year, y = All_Grants_Real), fill = "#003366", stat = "identity") +
      ggtitle(paste(state_list_1[i], "Revenue, Expenses and Grants")) +
      xlab("Financial Year") +
      ylab("$m") +
      ylim(0, 80000) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      scale_color_brewer(palette = "Paired", labels = c("Expenses", "Revenue")))

    ggsave(filename = path)

  }

  for (i in 1:length(state_list_2)) {
    print(i)
    z <- df %>% filter(State == state_list_2[i])
    print(z)

    path <- paste("C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Economy/Tax/General Tax/Working Files/R images/",
                  state_list_2[i],
                  " Revenue, Expenses and Grants",
                  ".jpeg",
                  sep = "")

    print(ggplot(data = z, aes(x = Year, y = Amount_Real)) +
            geom_point(aes(colour = Money_Flow)) +
            geom_line(aes(group = Money_Flow, colour = Money_Flow)) +
            geom_bar(data = z %>% filter(Money_Flow == "Total_GFS_Revenue"),
                     aes(x = Year, y = All_Grants_Real), fill = "#003366", stat = "identity") +
            ggtitle(paste(state_list_2[i], "Revenue, Expenses and Grants")) +
            xlab("Financial Year") +
            ylab("$m") +
            ylim(0, 20000) +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
            scale_color_brewer(palette = "Paired", labels = c("Expenses", "Revenue")))

    ggsave(filename = path)

  }

  df2 <- df %>%
    filter(Money_Flow == "Total_GFS_Revenue")

  ggplot(data = df2, aes(x = Year, y = Grant_Prop_Real)) +
    geom_point(aes(colour = State)) +
    geom_line(aes(group = State, colour = State)) +
    xlab("Financial Year") +
    ylab("Proportion of Total State Revenue") +
    ggtitle("All Grants as a Proportion of Total State Revenue") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_color_brewer(palette = "Paired")

  ggsave(filename = paste(p2, "State Grants Proportion of Total Revenue.jpeg", sep = ""))

  ggplot(data = df, aes(x = Year, y = Grant_per_Head)) +
    geom_point(aes(colour = State)) +
    geom_line(aes(group = State, colour = State)) +
    xlab("Financial Year") +
    ylab("$") +
    ggtitle("Federal Grants Per Head of State Population") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_color_brewer(palette = "Paired")

  ggsave(filename = paste(p2, "State Grants per Head.jpeg", sep = ""))

  ggplot(data = df, aes(x = Year, y = GST_per_Head)) +
    geom_point(aes(colour = State)) +
    geom_line(aes(group = State, colour = State)) +
    xlab("Financial Year") +
    ylab("$") +
    ggtitle("GST Granted Per Head of State Population") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_color_brewer(palette = "Paired")

  ggsave(filename = paste(p2, "State GST Revenue per Head.jpeg", sep = ""))

  df3 <- df %>%
    filter(Year == "2009-10" | Year == "2009-10" | Year == "2010-11" | Year == "2011-12" | Year == "2012-13" | Year == "2013-14" | Year == "2014-15" | Year == "2015-16")

  ggplot(data = df3, aes(x = Year, y = GST_Raised_per_Head)) +
    geom_point(aes(colour = State)) +
    geom_line(aes(group = State, colour = State)) +
    xlab("Financial Year") +
    ylab("$") +
    #ylim(1750, 2900) +
    ggtitle("GST Raised Per Head of State Population") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_color_brewer(palette = "Paired")

  ggsave(filename = paste(p2, "State GST Revenue Raised per Head.jpeg", sep = ""))

  df4 <- df %>%
    filter(Year == "2009-10" | Year == "2009-10" | Year == "2010-11" | Year == "2011-12" | Year == "2012-13" | Year == "2013-14" | Year == "2014-15" | Year == "2015-16",
           State != "NT")

  ggplot(data = df4, aes(x = Year, y = GST_Diff_per_head)) +
    geom_point(aes(colour = State)) +
    geom_line(aes(group = State, colour = State)) +
    xlab("Financial Year") +
    ylab("$") +
    #ylim(1750, 2900) +
    ggtitle("Net GST Distribution Per Head of State Population") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_color_manual(values = RColorBrewer::brewer.pal(8, "Paired")[2:8])

  ggsave(filename = paste(p2, "State GST Revenue Diff per Head.jpeg", sep = ""))

  ggplot(df, aes(x = Year, y = Estimated_Pop)) +
    geom_bar(aes(fill = State), stat = "identity", position = "fill") +
    xlab("Financial Year") +
    ylab("Proportion of Population") +
    ggtitle("State Populations as a Proportion of Total Australian Population") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_fill_brewer(palette = "Paired")

  ggsave(filename = paste(p2, "Estimated Population.jpeg", sep = ""))

  df5 <- df %>%
    filter(Year == "2009-10" | Year == "2009-10" | Year == "2010-11" | Year == "2011-12" | Year == "2012-13" | Year == "2013-14" | Year == "2014-15" | Year == "2015-16")

  ggplot(data = df5, aes(x = Year, y = GST_Raised_Real)) +
    geom_line(aes(group = State, colour = State)) +
    geom_point(aes(colour = State)) +
    xlab("Financial Year") +
    ylab("GST Raised in $m") +
    ggtitle("Real GST Raised Per State") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_color_brewer(palette = "Paired")

  ggsave(filename = paste(p2, "Total State GST Raised.jpeg", sep = ""))

  raw_read2 <- readxl::read_xlsx(path = p, sheet = 7)

  df_GRA <- raw_read2 %>%
    filter(Year == "2000-01" | Year == "2001-02" | Year == "2002-03" |
             Year == "2003-04" | Year == "2004-05" | Year == "2005-06" |
             Year == "2006-07" | Year == "2007-08" | Year == "2008-09" |
             Year == "2009-10" | Year == "2009-10" | Year == "2010-11" |
             Year == "2011-12" | Year == "2012-13" | Year == "2013-14" |
             Year == "2014-15" | Year == "2015-16", State != "ALL") %>%
    mutate(State = factor(State, levels = state_list, ordered = TRUE),
           GST_Prop_of_GRA = GST_Grants/Total_General_Revenue_Assistance,
           GRA_Prop_of_Grants = Total_General_Revenue_Assistance/All_Grants)

  ggplot(data = df_GRA, aes(x = Year, y = GST_Prop_of_GRA)) +
    geom_line(aes(group = State, colour = State)) +
    geom_point(aes(colour = State)) +
    xlab("Financial Year") +
    ylab("Proportion of Total GRA") +
    ggtitle("GST as a Proportion of Total GRA Per State") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_color_brewer(palette = "Paired")

  ggsave(filename = paste(p2, "GST as Prop of Total GRA.jpeg", sep = ""))

  ggplot(data = df_GRA, aes(x = Year, y = GRA_Prop_of_Grants)) +
    geom_line(aes(group = State, colour = State)) +
    geom_point(aes(colour = State)) +
    xlab("Financial Year") +
    ylab("Proportion of Total Grants") +
    ggtitle("GRA as a Proportion of Total Grants Per State") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_color_brewer(palette = "Paired")

  ggsave(filename = paste(p2, "GRA as Prop of Total Grants.jpeg", sep = ""))

  ggplot(data = df, aes(x = Year, y = All_Grants_Real)) +
    geom_line(aes(group = State, colour = State)) +
    geom_point(aes(colour = State)) +
    xlab("Financial Year") +
    ylab("$m") +
    ggtitle("Total Grants to States (Real)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_color_brewer(palette = "Paired")

  ggsave(filename = paste(p2, "Total Real Grants to States.jpeg", sep = ""))

  df_historical <- readxl::read_xlsx("C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Economy/Tax/General Tax/Data/CGC_2017_Fiscal Equalisation General Revenue Grants From 1910.xlsx", sheet = 2)


  df_historical_clean <- df_historical %>%
    filter(is.na(X__1) == FALSE, X__1 != "(a)") %>%
    select(-Total) %>%
    `colnames<-`(., c("Year", "NSW", "VIC", "QLD", "WA", "SA", "TAS", "ACT", "NT")) %>%
    gather(key = State, value = `$m`, -Year) %>%
    mutate(State = factor(State, levels = state_list, ordered = TRUE),
           `$m` = as.numeric(`$m`)) %>%
    #filter(State == "NSW") %>%
    mutate(`$m` = `$m`/1000)

  ggplot(data = df_historical_clean, aes(x = Year, y = `$m`)) +
    geom_line(aes(group = State, colour = State)) +
    geom_point(aes(colour = State)) +
    #geom_dl(aes(label = State, colour = State), method = list(dl.trans(x = x - 1, y = y + 1), "last.points")) +
    ylab("$m") +
    xlab("Financial Year") +
    ggtitle("Nominal Total GRA Grants Distributed to States") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_colour_brewer(palette = "Paired")

  ggsave(filename = paste(p2, "Nominal Total GRA Grants Distributed to States.jpeg", sep = ""))
}



