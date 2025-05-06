#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinydashboard)         #https://rstudio.github.io/shinydashboard/get_started.html
library(dplyr)
library(ggplot2)
library(knitr)                  
library(kableExtra)
library(visNetwork)              #https://cran.r-project.org/web/packages/visNetwork/vignettes/Introduction-to-visNetwork.html  
library(igraph)                  #https://r.igraph.org/articles/igraph.html
library(ggrepel)                 #https://ggrepel.slowkow.com

ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(title = "Enron Emails"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      selectInput("analysis_choice", "Select Analysis",
                  choices = c("Global Mail Flow", "Mail Flow by domain", "Mail Flow by status", "Mail Flow by group",
                              "Mail Flow Between Roles")),
      menuItem("Tables", tabName = "tables", icon = icon("table")),
      menuItem("Network", tabName = "network", icon = icon("project-diagram")),
      conditionalPanel(
        condition = "input.sidebarItemExpanded === 'Network'",
        checkboxInput("show_communities", "Show Communities", value = TRUE)
      ),
      menuItem("Keywords", tabName = "keywords", icon = icon("search")),
      helpText(HTML("<div style='white-space: normal; margin-top: 40px; color: #b0b0b0; font-size: 12px;'>Marie-Caroline Bertheau's shiny app for Enron analysis</div>"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                box(width = 12, solidHeader = TRUE, status = "primary", title = "Main Analysis Plot",
                    plotOutput("myplot", height = "700px"))
              )
      ),
      tabItem(tabName = "tables",
              fluidRow(
                box(title = "Summary of external flow", width = 6, solidHeader = TRUE, status = "info",
                    htmlOutput("summary_counts_table")),
                box(title = "Summary of internal flow", width = 6, solidHeader = TRUE, status = "info",
                    htmlOutput("summary_internal_enron_table"))
              ),
              fluidRow(
                box(title = "Top 10 Senders", width = 6, solidHeader = TRUE, status = "info",
                    htmlOutput("top_senders_table")),
                box(title = "Top 10 Receivers", width = 6, solidHeader = TRUE, status = "info",
                    htmlOutput("top_receivers_table"))
              ),
              fluidRow(
                box(title = "Top Tension-Related Content Keywords", width = 6, solidHeader = TRUE, status = "warning",
                    htmlOutput("tension_keywords_table")),
                box(title = "Top Tension-Related Keywords in Subjects", width = 6, solidHeader = TRUE, status = "warning",
                    htmlOutput("tension_subjects_table"))
              )
      ),
      tabItem(tabName = "network",
              fluidRow(
                box(title = "Interactive Email Network Graph", width = 12, solidHeader = TRUE, status = "info",
                    visNetworkOutput("network_vis", height = "600px"))
              )
      ),
      tabItem(tabName = "keywords",
              fluidRow(
                box(width = 3, solidHeader = TRUE, status = "success", title = "Keyword Selectors",
                    actionButton("select_all", "Select All Words", icon = icon("check-double")),
                    actionButton("clear_all", "Clear All Words", icon = icon("times-circle")),
                    checkboxGroupInput("selected_subject_keywords", "Subject Keywords", choices = unique(keywordsubjects_counts$keyword)),
                    checkboxGroupInput("selected_content_keywords", "Content Keywords", choices = unique(keywordcontent_counts$keyword))
                ),
                box(width = 9, solidHeader = TRUE, status = "success", title = "Combined Keyword Plot",
                    plotOutput("combined_keyword_plot", height = "700px"))
              )
      )
      
    )
  )
)

#Server Part

server <- function(input, output, session) {
  
  output$plot_title <- renderText({ "Main Analysis Plot" })
  
#Dashboard
  
  output$myplot <- renderPlot({
    req(input$analysis_choice)
    
    if (input$analysis_choice == "Global Mail Flow") {
      message_filtered <- message %>%
        mutate(date = as.Date(date),
               month = as.Date(format(date, "%Y-%m-01"))) %>%
        filter(month >= as.Date("1999-01-01") & month <= as.Date("2002-12-31"))
      
      mail_volume <- message_filtered %>%
        group_by(month) %>%
        summarise(n_mails = n()) %>%
        arrange(month)
      
      events <- data.frame(
        date = as.Date(c("2000-11-01", "2001-04-01", "2001-10-01", "2002-01-01")),
        label = c("California Crisis", "New CEO", "SEC Investigation", "Enron Bankruptcy")
      )
      
      ggplot(mail_volume, aes(x = month, y = n_mails)) +
        geom_line(color = "cyan", linewidth = 1.2) +
        geom_point(color = "blue", size = 2) +
        geom_vline(data = events, aes(xintercept = date),
                   color = "red2", linetype = "dashed", linewidth = 1.2) +
        geom_text_repel(data = events,
                        aes(x = date - 15, y = 0, label = label),
                        inherit.aes = FALSE,
                        angle = 90, hjust = 1.2,
                        direction = "y",
                        nudge_x = -10,
                        segment.color = NA,
                        size = 3.5,
                        fontface = "italic",
                        color = "red2") +
        scale_x_date(date_breaks = "3 months", date_labels = "%b %Y",
                     limits = as.Date(c("1999-01-01", "2002-12-31"))) +
        labs(title = "Number of Emails Sent Per Month",
             x = "Month", y = "Number of Emails") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
    } else if (input$analysis_choice == "Mail Flow by domain") {
      senders <- message %>%
        filter(!is.na(sender), sender != "") %>%
        mutate(domain = ifelse(grepl("@enron.com$", sender, ignore.case = TRUE), "Enron", "Extern")) %>%
        count(domain, name = "nb") %>%
        mutate(pct = round(100 * nb / sum(nb), 1),
               label = paste0(pct, "%"))
      
      receivers <- recipientinfo %>%
        filter(!is.na(rvalue), rvalue != "") %>%
        mutate(domain = ifelse(grepl("@enron.com$", rvalue, ignore.case = TRUE), "Enron", "Extern")) %>%
        count(domain, name = "nb") %>%
        mutate(pct = round(100 * nb / sum(nb), 1),
               label = paste0(pct, "%"))
      
      colors <- c("Enron" = "darkcyan", "Extern" = "darkgoldenrod1")
      
      
      par(mar = c(2, 2, 2, 2))
      par(mfrow = c(1, 2), oma = c(0, 0, 2, 0))
      par(xpd = TRUE)
      
      
      pie(senders$nb,
          labels = senders$label,
          col = colors[senders$domain])
      mtext("Senders", side = 1, font = 3)
      
      
      pie(receivers$nb,
          labels = receivers$label,
          col = colors[receivers$domain])
      mtext("Receivers", side = 1, font = 3)
      
      legend("topright",
             legend = names(colors),
             fill = colors,
             title = "Email domains",
             bty = "n")
      
      title(main = "Email Domain Distribution", outer = TRUE, cex.main = 1.6)
      
    } else if (input$analysis_choice == "Mail Flow by status") {
      
      senders_summary <- message %>%
        group_by(sender) %>%
        summarise(n = n()) %>%
        inner_join(employeelist, by = c("sender" = "Email_id")) %>%
        filter(!is.na(status)) %>%
        group_by(status) %>%
        summarise(n_messages = sum(n)) %>%
        mutate(type = "Sent")
      
      receivers_summary <- recipientinfo %>%
        group_by(rvalue) %>%
        summarise(n = n()) %>%
        inner_join(employeelist, by = c("rvalue" = "Email_id")) %>%
        filter(!is.na(status)) %>%
        group_by(status) %>%
        summarise(n_messages = sum(n)) %>%
        mutate(type = "Received")
      
      external_senders <- message %>%
        filter(!is.na(sender), !grepl("@enron.com$", sender, ignore.case = TRUE)) %>%
        count(sender) %>%
        summarise(n_messages = sum(n)) %>%
        mutate(status = "Externe", type = "Sent")
      
      external_receivers <- recipientinfo %>%
        filter(!is.na(rvalue), !grepl("@enron.com$", rvalue, ignore.case = TRUE)) %>%
        count(rvalue) %>%
        summarise(n_messages = sum(n)) %>%
        mutate(status = "Externe", type = "Received")
      
      combined_summary <- bind_rows(senders_summary, receivers_summary, external_senders, external_receivers)
      
    
      ggplot(combined_summary, aes(x = status, y = n_messages, group = type, color = type)) +
        geom_line(linewidth = 1.2) +
        geom_point(size = 2) +
        scale_color_manual(values = c("Sent" = "#1f78b4", "Received" = "#e31a1c")) +
        labs(title = "E-Mail Volume by Status",
             x = "Status", y = "Number of E-mails",
             color = NULL) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
    } else if (input$analysis_choice == "Mail Flow Between Roles") {
      senders_status <- message %>%
        select(mid, sender) %>%
        left_join(employeelist %>% select(Email_id, status), by = c("sender" = "Email_id")) %>%
        mutate(sender_status = case_when(
          !is.na(status) ~ status,
          !is.na(sender) & !grepl("@enron.com$", sender, ignore.case = TRUE) ~ "Externe",
          TRUE ~ NA_character_
        )) %>%
        select(mid, sender_status)
      
      receivers_status <- recipientinfo %>%
        select(mid, rvalue) %>%
        left_join(employeelist %>% select(Email_id, status), by = c("rvalue" = "Email_id")) %>%
        mutate(receiver_status = case_when(
          !is.na(status) ~ status,
          !is.na(rvalue) & !grepl("@enron.com$", rvalue, ignore.case = TRUE) ~ "Externe",
          TRUE ~ NA_character_
        )) %>%
        select(mid, receiver_status)
      
      statut_links <- left_join(receivers_status, senders_status, by = "mid") %>%
        filter(!is.na(sender_status), !is.na(receiver_status))
      
      status_counts <- statut_links %>%
        count(sender_status, receiver_status, name = "n_mails")
      
      
      ggplot(status_counts, aes(x = receiver_status, y = sender_status)) +
        geom_point(aes(size = n_mails, color = n_mails), alpha = 0.8) +
        geom_text(aes(label = ifelse(n_mails > 1000, n_mails, "")), vjust = -1, size = 3) +
        scale_size(range = c(2, 15), name = "Number of mails") +
        scale_color_gradientn(
          colours = c("#deebf7", "#9ecae1", "#3182bd", "#08306b"),
          trans = "log10",
          name = "Number of mails (log scale)"
        ) +
        labs(title = "Email Flow Between Roles (Log Color Scale)",
             x = "Recipient Status",
             y = "Sender Status") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              plot.title = element_text(face = "bold", size = 14))
      
    } else if (input$analysis_choice == "Mail Flow by group") {
      
      combined_activity <- bind_rows(sent_data, received_data) %>%
        filter(month_date >= as.Date("1999-01-01") & month_date <= as.Date("2002-12-31")) %>%
        mutate(group = factor(group, levels = c("Leader", "Employees", "N/A Enron status", "Extern")))
      
      ggplot(combined_activity, aes(x = month_date, y = count, color = type)) +
        annotate("rect", xmin = highlight_start, xmax = highlight_end,
                 ymin = 0, ymax = Inf, fill = "#66b2ff", alpha = 0.25) +
        geom_line(linewidth = 0.8) +
        geom_point(size = 1.2) +
        geom_vline(data = events, aes(xintercept = date),
                   color = "yellow", linetype = "dashed", linewidth = 1.2) +
        ggrepel::geom_text_repel(data = events,
                                 aes(x = date, y = 18000, label = label),
                                 inherit.aes = FALSE,
                                 angle = 90, hjust = 1.2,
                                 direction = "y",
                                 nudge_x = -10,
                                 segment.color = NA,
                                 size = 3.5,
                                 fontface = "italic",
                                 color = "black") +
        facet_wrap(~ group, scales = "fixed", ncol = 1) +
        scale_color_manual(values = c("Sent" = "#2c7fb8", "Received" = "#f03b20")) +
        scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
        labs(title = "Evolution of Emails Sent and Received by Group",
             subtitle = "Shaded area (blue) shows the collapse period (Q4 2001)",
             x = "Month", y = "Number of Emails", color = NULL) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              strip.text = element_text(face = "bold", size = 12),
              plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
              plot.subtitle = element_text(size = 11, hjust = 1.5),
              legend.position = "bottom")
    }
  })
  
  #Tables Part 
  
    # Bloc 1 : External Flow
    output$summary_counts_table <- renderUI({
      nb_na_statut <- employeelist %>% filter(status == "N/A") %>% nrow()
      nb_sender_externe <- message %>%
        filter(!is.na(sender), sender != "", !grepl("@enron.com$", sender, ignore.case = TRUE)) %>%
        distinct(sender) %>% nrow()
      nb_receiver_externe <- recipientinfo %>%
        filter(!is.na(rvalue), rvalue != "", !grepl("@enron.com$", rvalue, ignore.case = TRUE)) %>%
        distinct(rvalue) %>% nrow()
      
      df_summary <- data.frame(
        Description = c("N/A status", "Extern senders", "Extern receivers"),
        Valeur = c(nb_na_statut, nb_sender_externe, nb_receiver_externe)
      )
      
      df_summary %>%
        kable(format = "html",
              align = c("l", "r"),
              col.names = c("Description", "Valeur"),
              caption = "<b style='color:black;'>Summary of Sender and Receiver Counts</b>") %>%
        kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                      full_width = FALSE, font_size = 12, position = "left") %>%
        row_spec(0, bold = TRUE, background = "blue", color = "white") %>%
        HTML()
    })
    
    
    # Bloc 2 : Internal Flow
    output$summary_internal_enron_table <- renderUI({
      stats_senders_enron <- message %>%
        filter(!is.na(sender), grepl("@enron.com$", sender, ignore.case = TRUE)) %>%
        count(sender, name = "n_mails") %>%
        summarise(Min = min(n_mails), Max = max(n_mails)) %>%
        mutate(Type = "Enron's sender")
      
      stats_receivers_enron <- recipientinfo %>%
        filter(!is.na(rvalue), grepl("@enron.com$", rvalue, ignore.case = TRUE)) %>%
        count(rvalue, name = "n_mails") %>%
        summarise(Min = min(n_mails), Max = max(n_mails)) %>%
        mutate(Type = "Enron's receiver")
      
      resume_enron_df <- bind_rows(stats_senders_enron, stats_receivers_enron) %>%
        select(Type, Min, Max)
      
      resume_enron_df %>%
        kable(format = "html", digits = 2,
              caption = "<b style='color:black;'>Summary of Internal Enron Email Activity</b>") %>%
        kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                      full_width = FALSE, font_size = 12, position = "left") %>%
        row_spec(0, bold = TRUE, background = "blue", color = "white") %>%
        HTML()
    })
    
    
    # Bloc 3 : Top Senders
    output$top_senders_table <- renderUI({
      top_senders <- message %>%
        filter(!is.na(sender)) %>%
        count(sender, sort = TRUE) %>%
        slice_head(n = 10)
      
      top_senders %>%
        kable(format = "html",
              col.names = c("Email Address", "Number of Sent Emails"),
              caption = "<b style='color:black;'>Top 10 Email Senders</b>") %>%
        kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                      full_width = FALSE, font_size = 12, position = "left") %>%
        row_spec(0, bold = TRUE, background = "blue", color = "white") %>%
        HTML()
    })
    
    
    # Bloc 4 : Top Receivers
    output$top_receivers_table <- renderUI({
      top_receivers <- recipientinfo %>%
        filter(!is.na(rvalue)) %>%
        count(rvalue, sort = TRUE) %>%
        slice_head(n = 10)
      
      top_receivers %>%
        kable(format = "html",
              col.names = c("Email Address", "Number of Received Emails"),
              caption = "<b style='color:black;'>Top 10 Email Receivers</b>") %>%
        kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                      full_width = FALSE, font_size = 12, position = "left") %>%
        row_spec(0, bold = TRUE, background = "blue", color = "white") %>%
        HTML()
    })
    
    
    # Bloc 5 : Top Subject Keywords
    output$tension_subjects_table <- renderUI({
      clean_words <- message %>%
        filter(!is.na(subject)) %>%
        pull(subject) %>%
        tolower() %>%
        gsub("[^a-z\\s]", " ", .) %>%
        strsplit("\\s+") %>%
        unlist() %>%
        .[nchar(.) > 3]
      
      TOP150_content <- as.data.frame(table(clean_words)) %>%
        arrange(desc(Freq)) %>%
        slice_head(n = 150) %>%
        rename(word = clean_words) %>%
        mutate(rank = row_number())
      
      tension_subjectskeywords <- c("credit", "california", "trading", "deal", "market", "isda", "ferc",
                                    "contract", "notification", "legal", "risk", "announcement", "committee",
                                    "financial", "expense", "important", "urgent", "board")
      
      tension_subject_df <- TOP150_content %>%
        filter(word %in% tension_subjectskeywords) %>%
        arrange(rank)
      
      tension_subject_df %>%
        kable("html",
              caption = "<b style='color:black;'>Top Tension-Related Keywords in Email Subjects</b>",
              col.names = c("Word", "Frequency", "Rank")) %>%
        kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                      full_width = FALSE, font_size = 12, position = "left") %>%
        column_spec(1, bold = TRUE) %>%
        row_spec(0, bold = TRUE, color = "white", background = "blue") %>%
        HTML()
    })
    
    
    # Bloc 6 : Top Content Keywords
    output$tension_keywords_table <- renderUI({
      clean_words <- referenceinfo %>%
        filter(!is.na(reference)) %>%
        pull(reference) %>%
        tolower() %>%
        gsub("[^a-z\\s]", " ", .) %>%
        strsplit("\\s+") %>%
        unlist() %>%
        .[nchar(.) > 3]
      
      TOP150_content <- as.data.frame(table(clean_words)) %>%
        arrange(desc(Freq)) %>%
        slice_head(n = 150) %>%
        rename(word = clean_words) %>%
        mutate(rank = row_number())
      
      tension_contentkeywords <- c("contract", "agreement", "market", "price", "energy", "california", "credit",
                                   "confidential", "trading", "group", "issues", "power", "call", "meeting",
                                   "houston", "questions", "review", "legal", "help")
      
      tension_content_df <- TOP150_content %>%
        filter(word %in% tension_contentkeywords) %>%
        arrange(rank)
      
      tension_content_df %>%
        kable("html",
              caption = "<b style='color:black;'>Top Tension-Related Keywords in Email Content</b>",
              col.names = c("Word", "Frequency", "Rank")) %>%
        kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                      full_width = FALSE, font_size = 12, position = "left") %>%
        column_spec(1, bold = TRUE) %>%
        row_spec(0, bold = TRUE, color = "white", background = "#2c7fb8") %>%
        HTML()
    })
    
  
  
  # Interactive Network Graph (visNetwork)
  output$network_vis <- renderVisNetwork({
    req(exists("edges_small"))
    g <- graph_from_data_frame(edges_small, directed = TRUE)
    
    
    if (input$show_communities) {
      comm <- cluster_walktrap(g)  
      communities <- membership(comm)
      colors <- rainbow(length(unique(communities)))[communities]
      group_labels <- paste0("Community ", unique(communities))
    } else {
      communities <- rep(1, vcount(g))
      colors <- "lightblue"
      group_labels <- "All"
    }
    
    nodes <- data.frame(id = V(g)$name,
                        label = V(g)$name,
                        group = as.factor(communities),
                        color = colors)
    
    edges <- data.frame(from = edges_small$sender,
                        to = edges_small$rvalue,
                        value = edges_small$weight)
    
    visNetwork(nodes, edges) %>%
      visNodes(size = 15) %>%
      visEdges(arrows = "to", smooth = FALSE) %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visLayout(randomSeed = 123) %>%
      visLegend(addNodes = data.frame(label = group_labels,
                                      shape = "dot",
                                      color = rainbow(length(unique(communities)))),
                useGroups = FALSE, position = "right", main = "Detected Communities")
  })
  
  
  
  
  # Keywords 
  
  observeEvent(input$select_all, {
    updateCheckboxGroupInput(session, "selected_subject_keywords", selected = unique(keywordsubjects_counts$keyword))
    updateCheckboxGroupInput(session, "selected_content_keywords", selected = unique(keywordcontent_counts$keyword))
  })
  
  observeEvent(input$clear_all, {
    updateCheckboxGroupInput(session, "selected_subject_keywords", selected = character(0))
    updateCheckboxGroupInput(session, "selected_content_keywords", selected = character(0))
  })
  
  output$combined_keyword_plot <- renderPlot({
    if (length(c(input$selected_subject_keywords, input$selected_content_keywords)) == 0) {
      return(NULL)
    }
    
    data_subject <- keywordsubjects_counts %>% filter(keyword %in% input$selected_subject_keywords) %>% mutate(type = "Subject")
    data_content <- keywordcontent_counts %>% filter(keyword %in% input$selected_content_keywords) %>% mutate(type = "Content")
    
    combined_data <- bind_rows(data_subject, data_content)
    
    max_y <- max(combined_data$n, na.rm = TRUE)
    
    ggplot(combined_data, aes(x = month_date, y = n, color = keyword)) +
      annotate("rect", xmin = as.Date("2001-10-01"), xmax = as.Date("2002-01-01"),
               ymin = 0, ymax = Inf, fill = "#66b2ff", alpha = 0.2) +
      geom_line(linewidth = 1) +
      geom_point(size = 1.5) +
      geom_vline(data = events, aes(xintercept = date), color = "red2", linetype = "dashed", linewidth = 1) +
      geom_text(data = events, aes(x = date - 10, y = max_y, label = label),
                angle = 90, vjust = -0.2, hjust = 0, size = 5, color = "black", inherit.aes = FALSE) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.3))) +
      scale_x_date(limits = as.Date(c("1998-12-01", "2002-08-01")),
                   date_labels = "%b\n%Y", date_breaks = "6 months") +
      labs(title = "Trend of Selected Tension-Related Keywords",
           subtitle = "Subjects and Content combined",
           x = "Date", y = "Number of Mentions", color = "Keyword") +
      theme_minimal(base_size = 10) +
      theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
            axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "bottom")
    
    
  }) 
}


    shinyApp(ui, server)



