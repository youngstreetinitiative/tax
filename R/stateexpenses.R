state_expenses <- function(){

  library(ggplot2)

  p <- "C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Economy/Tax/General Tax/Working Files/ABS_5512.0_Tax_CostsRevenue_Comparison.xlsx"
  p2 <- "C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Economy/Tax/General Tax/Working Files/R images/"
  state_list <- c("NT", "ACT", "TAS", "SA", "WA", "QLD", "VIC", "NSW")

  df <- readxl::read_xlsx(path = p, sheet = 6)


  df_total <- df %>%
    mutate(State = factor(State, levels = state_list, ordered = TRUE),
           Total_Real = `Total Expenses` * (max(CPI)/CPI),
           Total_Health_Real = `Total health` * (max(CPI)/CPI),
           Total_Edu_Real = `Total education` * (max(CPI)/CPI),
           Total_Social_Real = `Total social security and welfare` * (max(CPI)/CPI),
           Total_Housing_Real = `Total housing and community amenities` * (max(CPI)/CPI),
           Total_Rec_Real = `Total recreation and culture` * (max(CPI)/CPI),
           Total_Energy_Real = `Fuel and energy` * (max(CPI)/CPI),
           Total_per_cap = (Total_Real * 1000000)/Estimated_Pop,
           Total_Health_per_cap = (Total_Health_Real * 1000000)/Estimated_Pop,
           Total_Edu_per_cap = (Total_Edu_Real * 1000000)/Estimated_Pop,
           Total_Social_per_cap = (Total_Social_Real * 1000000)/Estimated_Pop,
           Total_Housing_per_cap = (Total_Housing_Real * 1000000)/Estimated_Pop,
           Total_Rec_per_cap = (Total_Rec_Real * 1000000)/Estimated_Pop,
           Total_Energy_per_cap = (Total_Energy_Real * 1000000)/Estimated_Pop,
           Date = as.Date(paste0(str_extract(Date, "[0-9]+"), "-6-01")))

  df_total_sansnt <- df %>%
    mutate(State = factor(State, levels = state_list, ordered = TRUE),
           Total_Real = `Total Expenses` * (max(CPI)/CPI),
           Total_Health_Real = `Total health` * (max(CPI)/CPI),
           Total_Edu_Real = `Total education` * (max(CPI)/CPI),
           Total_Social_Real = `Total social security and welfare` * (max(CPI)/CPI),
           Total_Housing_Real = `Total housing and community amenities` * (max(CPI)/CPI),
           Total_Rec_Real = `Total recreation and culture` * (max(CPI)/CPI),
           Total_Energy_Real = `Fuel and energy` * (max(CPI)/CPI),
           Total_per_cap = (Total_Real * 1000000)/Estimated_Pop,
           Total_Health_per_cap = (Total_Health_Real * 1000000)/Estimated_Pop,
           Total_Edu_per_cap = (Total_Edu_Real * 1000000)/Estimated_Pop,
           Total_Social_per_cap = (Total_Social_Real * 1000000)/Estimated_Pop,
           Total_Housing_per_cap = (Total_Housing_Real * 1000000)/Estimated_Pop,
           Total_Rec_per_cap = (Total_Rec_Real * 1000000)/Estimated_Pop,
           Total_Energy_per_cap = (Total_Energy_Real * 1000000)/Estimated_Pop,
           Date = as.Date(paste0(str_extract(Date, "[0-9]+"), "-6-01"))) %>%
    filter(State != "NT")

  frame_list <- list(df_total, df_total_sansnt)

  for (i in 1:length(frame_list)) {

    if (i == 1) {
      name <- "All States "
    }else{
      name <- "Without NT "
    }

    if (i == 1) {
      col_range <- 1:8
    }else{
      col_range <- 2:8
    }

    ggplot(data = data.frame(frame_list[i]), aes(x = Date, y = Total_per_cap)) +
      geom_line(aes(group = State, colour = State)) +
      geom_point(aes(colour = State)) +
      xlab("Financial Year") +
      ylab("$") +
      ggtitle("Total Expenses Per Capita") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      scale_color_manual(values = RColorBrewer::brewer.pal(8, "Paired")[col_range])

    ggsave(filename = paste(p2, name, "Total Expenses Per Capita.jpeg", sep = ""))

    ggplot(data = data.frame(frame_list[i]), aes(x = Date, y = Total_Health_per_cap)) +
      geom_line(aes(group = State, colour = State)) +
      geom_point(aes(colour = State)) +
      xlab("Financial Year") +
      ylab("$") +
      ggtitle("Total Health Expenses Per Capita") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      scale_color_manual(values = RColorBrewer::brewer.pal(8, "Paired")[col_range])

    ggsave(filename = paste(p2, name, "Total Health Expenses Per Capita.jpeg", sep = ""))

    ggplot(data = data.frame(frame_list[i]), aes(x = Date, y = Total_Edu_per_cap)) +
      geom_line(aes(group = State, colour = State)) +
      geom_point(aes(colour = State)) +
      xlab("Financial Year") +
      ylab("$") +
      ggtitle("Total Education Expenses Per Capita") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      scale_color_manual(values = RColorBrewer::brewer.pal(8, "Paired")[col_range])

    ggsave(filename = paste(p2, name, "Total Education Expenses Per Capita.jpeg", sep = ""))

    ggplot(data = data.frame(frame_list[i]), aes(x = Date, y = Total_Social_per_cap)) +
      geom_line(aes(group = State, colour = State)) +
      geom_point(aes(colour = State)) +
      xlab("Financial Year") +
      ylab("$") +
      ggtitle("Total Social Expenses Per Capita") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      scale_color_manual(values = RColorBrewer::brewer.pal(8, "Paired")[col_range])

    ggsave(filename = paste(p2, name, "Total Social Expenses Per Capita.jpeg", sep = ""))

    ggplot(data = data.frame(frame_list[i]), aes(x = Date, y = Total_Housing_per_cap)) +
      geom_line(aes(group = State, colour = State)) +
      geom_point(aes(colour = State)) +
      xlab("Financial Year") +
      ylab("$") +
      ggtitle("Total Housing Expenses Per Capita") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      scale_color_manual(values = RColorBrewer::brewer.pal(8, "Paired")[col_range])

    ggsave(filename = paste(p2, name, "Total Housing Expenses Per Capita.jpeg", sep = ""))

    ggplot(data = data.frame(frame_list[i]), aes(x = Date, y = Total_Rec_per_cap)) +
      geom_line(aes(group = State, colour = State)) +
      geom_point(aes(colour = State)) +
      xlab("Financial Year") +
      ylab("$") +
      ggtitle("Total Recreation Expenses Per Capita") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      scale_color_manual(values = RColorBrewer::brewer.pal(8, "Paired")[col_range])

    ggsave(filename = paste(p2, name, "Total Recreation Expenses Per Capita.jpeg", sep = ""))

    ggplot(data = data.frame(frame_list[i]), aes(x = Date, y = Total_Energy_per_cap)) +
      geom_line(aes(group = State, colour = State)) +
      geom_point(aes(colour = State)) +
      xlab("Financial Year") +
      ylab("$") +
      ggtitle("Total Energy Expenses Per Capita") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      scale_color_manual(values = RColorBrewer::brewer.pal(8, "Paired")[col_range])

    ggsave(filename = paste(p2, name, "Total Energy Expenses Per Capita.jpeg", sep = ""))

  }
}
