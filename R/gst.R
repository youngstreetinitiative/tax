
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
    gather(key = Money_Flow, value = Amount, -Date, -State, -CPI, -All_Grants, -Estimated_Pop, -GST_Grants, -GST_Relativities, -Estimated_GST_Raised, -Total_Net_financial_Assets) %>%
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

    print(ggplot(data = z, aes(x = Date, y = Amount_Real)) +
      geom_point(aes(colour = Money_Flow)) +
      geom_line(aes(group = Money_Flow, colour = Money_Flow)) +
      geom_bar(data = z %>% filter(Money_Flow == "Total_GFS_Revenue"),
               aes(x = Date, y = All_Grants_Real), fill = "#003366", stat = "identity") +
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

    print(ggplot(data = z, aes(x = Date, y = Amount_Real)) +
            geom_point(aes(colour = Money_Flow)) +
            geom_line(aes(group = Money_Flow, colour = Money_Flow)) +
            geom_bar(data = z %>% filter(Money_Flow == "Total_GFS_Revenue"),
                     aes(x = Date, y = All_Grants_Real), fill = "#003366", stat = "identity") +
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

  ggplot(data = df2, aes(x = Date, y = Grant_Prop_Real)) +
    geom_point(aes(colour = State)) +
    geom_line(aes(group = State, colour = State)) +
    xlab("Financial Year") +
    ylab("Proportion of Total State Revenue") +
    ggtitle("All Grants as a Proportion of Total State Revenue") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_color_brewer(palette = "Paired")

  ggsave(filename = paste(p2, "State Grants Proportion of Total Revenue.jpeg", sep = ""))

  ggplot(data = df, aes(x = Date, y = Grant_per_Head)) +
    geom_point(aes(colour = State)) +
    geom_line(aes(group = State, colour = State)) +
    xlab("Financial Year") +
    ylab("$") +
    ggtitle("Federal Grants Per Head of State Population") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_color_brewer(palette = "Paired")

  ggsave(filename = paste(p2, "State Grants per Head.jpeg", sep = ""))

  ggplot(data = df, aes(x = Date, y = GST_per_Head)) +
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
    filter(Date == "2009-10" | Date == "2009-10" | Date == "2010-11" | Date == "2011-12" | Date == "2012-13" | Date == "2013-14" | Date == "2014-15" | Date == "2015-16")

  ggplot(data = df3, aes(x = Date, y = GST_Raised_per_Head)) +
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
    filter(Date == "2009-10" | Date == "2009-10" | Date == "2010-11" | Date == "2011-12" | Date == "2012-13" | Date == "2013-14" | Date == "2014-15" | Date == "2015-16",
           State != "NT")

  ggplot(data = df4, aes(x = Date, y = GST_Diff_per_head)) +
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

  ggplot(df, aes(x = Date, y = Estimated_Pop)) +
    geom_bar(aes(fill = State), stat = "identity", position = "fill") +
    xlab("Financial Year") +
    ylab("Proportion of Population") +
    ggtitle("State Populations as a Proportion of Total Australian Population") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_fill_brewer(palette = "Paired")

  ggsave(filename = paste(p2, "Estimated Population.jpeg", sep = ""))

  df5 <- df %>%
    filter(Date == "2009-10" | Date == "2009-10" | Date == "2010-11" | Date == "2011-12" | Date == "2012-13" | Date == "2013-14" | Date == "2014-15" | Date == "2015-16")

  ggplot(data = df5, aes(x = Date, y = GST_Raised_Real)) +
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
    filter(Date == "2000-01" | Date == "2001-02" | Date == "2002-03" |
             Date == "2003-04" | Date == "2004-05" | Date == "2005-06" |
             Date == "2006-07" | Date == "2007-08" | Date == "2008-09" |
             Date == "2009-10" | Date == "2009-10" | Date == "2010-11" |
             Date == "2011-12" | Date == "2012-13" | Date == "2013-14" |
             Date == "2014-15" | Date == "2015-16", State != "ALL") %>%
    mutate(State = factor(State, levels = state_list, ordered = TRUE),
           GST_Prop_of_GRA = GST_Grants/Total_General_Revenue_Assistance,
           GRA_Prop_of_Grants = Total_General_Revenue_Assistance/All_Grants)

  ggplot(data = df_GRA, aes(x = Date, y = GST_Prop_of_GRA)) +
    geom_line(aes(group = State, colour = State)) +
    geom_point(aes(colour = State)) +
    xlab("Financial Year") +
    ylab("Proportion of Total GRA") +
    ggtitle("GST as a Proportion of Total GRA Per State") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_color_brewer(palette = "Paired")

  ggsave(filename = paste(p2, "GST as Prop of Total GRA.jpeg", sep = ""))

  ggplot(data = df_GRA, aes(x = Date, y = GRA_Prop_of_Grants)) +
    geom_line(aes(group = State, colour = State)) +
    geom_point(aes(colour = State)) +
    xlab("Financial Year") +
    ylab("Proportion of Total Grants") +
    ggtitle("GRA as a Proportion of Total Grants Per State") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_color_brewer(palette = "Paired")

  ggsave(filename = paste(p2, "GRA as Prop of Total Grants.jpeg", sep = ""))

  ggplot(data = df, aes(x = Date, y = All_Grants_Real)) +
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
    `colnames<-`(., c("Date", "NSW", "VIC", "QLD", "WA", "SA", "TAS", "ACT", "NT")) %>%
    gather(key = State, value = `$m`, -Date) %>%
    mutate(Date = as.Date(paste0(str_extract(Date, "[0-9]+" ), "-06-01")),
           State = factor(State, levels = state_list, ordered = TRUE),
           `$m` = as.numeric(`$m`)) %>%
    #filter(State == "NSW") %>%
    mutate(`$m` = `$m`/1000)

  ggplot(data = df_historical_clean, aes(x = Date, y = `$m`)) +
    geom_line(aes(group = State, colour = State)) +
    geom_point(aes(colour = State)) +
    #geom_dl(aes(label = State, colour = State), method = list(dl.trans(x = x - 1, y = y + 1), "last.points")) +
    ylab("$m") +
    xlab("Financial Year") +
    ggtitle("Nominal Total GRA Grants Distributed to States") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_colour_brewer(palette = "Paired") +
    scale_x_date(date_breaks = "5 years",
                 date_labels = "%Y")

  ggsave(filename = paste(p2, "Nominal Total GRA Grants Distributed to States.jpeg", sep = ""))

  GRA_hist <- readxl::read_xlsx(path = p, sheet = 8)

  GRA_hist_clean <- GRA_hist %>%
    mutate(Date = as.Date(paste0(str_extract(Date, "[0-9]+"), "-6-01")), `$m` = `$'000`/1000)

  ggplot(data = GRA_hist_clean, aes(x = Date, y = `$m`)) +
    geom_line(aes(group = Payment, colour = Payment), size = 0.8) +
    geom_point(aes(colour = Payment)) +
    xlab("Financial Year") +
    ylab("GRA Payment in $m") +
    ggtitle("Historcal General Revenue Assistance to the States") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    #scale_colour_brewer(palette = "Paired") +
    scale_x_date(limits = as.Date(c('1978-06-01', '2018-06-01')),
                 date_breaks = "5 years",
                 date_labels = "%Y")

    ggsave(filename = paste(p2, "Historical GRA Payments to the States.jpeg", sep = ""), width = 35, height = 20, units = "cm")

}



