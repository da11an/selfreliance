# Self Reliance Worksheet App
# Adaptation of https://www.churchofjesuschrist.org/bc/content/shared/content/english/pdf/welfare/PD60007387_000_SelfReliancePlan_Member_Web_Interactive.pdf
# Updated: 2020-12-15

library(shiny)
library(DT)
library(ggplot2)
library(dplyr)
library(magrittr)
library(ggthemes)
library(RColorBrewer)
library(shinythemes)

# functions ----
dfInputs <- function(df, row = NULL, labels = NULL, session){
    
    df_chr <- deparse(substitute(df))
    df_name <- df_chr
    # first check if reactive and deal with that
    if (grepl("\\$", df_chr)){
        df_name <- strsplit(df_chr, "\\$") %>% `[[`(1) %>% tail(1)
    }
    if (grepl("\\[", df_chr)){
        df_name <- df_chr %>% gsub("\\[", "", .) %>% gsub("\\]", "", .) %>% 
            strsplit(., '"') %>% `[[`(1) %>% tail(1) %>% 
            strsplit(., "'") %>% `[[`(1) %>% tail(1)
    }
    if (grepl("\\(", df_chr)){
        df_name <- df_chr %>% gsub("()", "", .)
    }
    input_ids <- paste0(df_name, "_col_", seq(names(df)))
    colheaders <- names(df)
    if (!is.null(labels)) { # replace table column names with user-friendly display names for modal
        for (i in 1:max(length(colheaders), length(labels))){
            colheaders[i] <- labels[i]
        }
    }
    colclasses <- sapply(df, "class")
    row <- tail(row, 1) # only keep latest selection if multiple
    if (!is.null(row) & row != 0){
        
        df_inputs <- lapply(seq_along(colheaders), function(i) {
            if (colclasses[i] %in% c("numeric", "integer")){
                numericInput(input_ids[i], colheaders[i], value = as.numeric(df[row, i]))
            } else if (colclasses[i] == "Date"){
                dateInput(input_ids[i], colheaders[i], value = as.Date(as.numeric(df[row, i]), origin = "1970-01-01"))
            } else { # (colclasses[i] == "character"){
                textInput(input_ids[i], colheaders[i], value = df[row, i])
            }
        })
        df_inputs[[length(df_inputs) + 1]] <- actionButton(paste0(df_name, "_editrow"), "Enter")
        df_inputs[[length(df_inputs) + 1]] <- actionButton(paste0(df_name, "_deleterow"), "Delete")
        df_inputs[[length(df_inputs) + 1]] <- modalButton("Cancel")
        
    } else {
        
        df_inputs <- lapply(seq_along(colheaders), function(i) {
            if (colclasses[i] %in% c("numeric", "integer")){
                numericInput(input_ids[i], colheaders[i], value = NULL)
            } else if (colclasses[i] == "Date"){
                dateInput(input_ids[i], colheaders[i], value = NULL)
            } else { # if (colclasses[i] == "character"){
                textInput(input_ids[i], colheaders[i], value = NULL)
            }
        })
        df_inputs[[length(df_inputs) + 1]] <- actionButton(paste0(df_name, "_addrow"), "Enter")
        df_inputs[[length(df_inputs) + 1]] <- modalButton("Cancel")
    }
    
    return(df_inputs)
}

dfEdit <- function(df, input, action = NULL, session){
    
    # this will access input values created by dfInputs if set
    df_chr <- deparse(substitute(df))
    df_name <- df_chr
    # first check if reactive and deal with that
    if (grepl("\\$", df_chr)){
        df_name <- strsplit(df_chr, "\\$") %>% `[[`(1) %>% tail(1)
    }
    if (grepl("\\[", df_chr)){
        df_name <- df_chr %>% gsub("\\[", "", .) %>% gsub("\\]", "", .) %>% 
            strsplit(., '"') %>% `[[`(1) %>% tail(1) %>% 
            strsplit(., "'") %>% `[[`(1) %>% tail(1)
    }
    if (grepl("\\(", df_chr)){
        df_name <- df_chr %>% gsub("()", "", .)
    }
    
    input_ids <- paste0(df_name, "_col_", seq(names(df)))
    row_number <- eval(parse(text = paste0("input$", df_name, "_rows_selected")))
    editrow <- row_number
    newrow <- nrow(df) + 1
    
    if (is.null(action) | action == "Modify"){
        for (i in seq_along(names(df))){
            df[editrow, i] <- eval(parse(text = paste0("input$", input_ids[i])))
        }
    } else if (action == "Add"){
        for (i in seq_along(names(df))){
            df[newrow, i] <- eval(parse(text = paste0("input$", input_ids[i])))
        }
    } else if (action == "Delete"){
        df <- df[-editrow,]
    }
    
    return(df)
}

financeTable <- function(df){
    df %<>% tibble::add_row(Description = "Total", 
                            Current = sum(df$Current),
                            Future = sum(df$Future))
    datatable(
        df,
        selection = 'single',
        options = list(dom = 't', 
                       pageLength = 1000,
                       columnDefs = list(list(targets = 0, visible = FALSE)))) %>%
        formatCurrency(c('Current', 'Future')) %>%
        formatStyle(
            'Description',
            target = "row",
            fontWeight = styleEqual("Total", "bold")
        )
}

planTable <- function(df_plan){
    datatable(
        df_plan,
        rownames = FALSE,
        options = list(dom = 't', pageLength = 1000)
    )
}

financeBalance <- function(df_income, df_expenses) {
    spend <- df_expenses$Current %>% sum
    make <- df_income$Current %>% sum
    net <- round(make - spend, 2)
    
    spend_future <- df_expenses$Future %>% sum
    make_future <- df_income$Future %>% sum
    net_future <- round(make_future - spend_future, 2)
    return(c(net, net_future))
}

financePlot <- function(df_income, df_expenses, screen_dim = NULL) {
    pixels_per_letter <- 6.7
    vertical_pixels <- 100
    vertical_letters <- vertical_pixels / pixels_per_letter
    min_pixel_width <- 20
    double_pixels <- 60
    
    # create long budget table
    df <- bind_rows(
        df_income %>% mutate(Budget = "Income"),
        df_expenses %>% mutate(Budget = "Expense")) %>%
        mutate(Description = trimws(Description)) %>%
        tidyr::pivot_longer(cols = c(Current, Future), 
                            names_to = "Time", 
                            values_to = "Amount")
    
    # merge identically named description items within Budget/Time group
    df %<>%
        group_by(Budget, Time, Description) %>%
        summarize(Amount = sum(Amount)) %>%
        ungroup
    
    # introduce 'id' which will become a factor for ordering and coloring
    df %<>% 
        left_join(
            df %>%
                filter(Time == 'Current') %>%
                arrange(Budget, -Amount) %>%
                mutate(id = row_number()) %>%
                select(id, Description)
        )
    
    # compute totals for each bar (grouped by Time & Budget)
    # compute text label locations
    df %<>%
        group_by(Time, Budget) %>%
        arrange(id) %>%
        mutate(Amount_max = sum(Amount)) %>% # getting length of each bar
        ungroup %>% 
        mutate(Amount_max = max(Amount_max)) #getting length of longest bar

    # convert Amount to pixels (estimated)
    df %<>%
        mutate(
            plot_width_pixels = screen_dim[1] - 100, # take off 100 for labels, etc
            horizontal_pixels = Amount / Amount_max * plot_width_pixels)
    
    # group items too tiny to label
    df %<>%
        group_by(Time, Budget) %>%
        mutate(grouping = if_else(horizontal_pixels < min_pixel_width,
                                  "tiny", Description)) %>%
        ungroup %>%
        group_by(Time, Budget, grouping) %>%
        summarize(
            Amount = sum(Amount),
            id = sum(id),
            horizontal_pixels = sum(horizontal_pixels),
            full_label = paste(Description, collapse = ", "),
            Description = case_when(
                length(Description)==1 ~ paste(Description, collapse = ""),
                length(Description)==2 ~ paste(strtrim(Description, 7), collapse = ","),
                length(Description)==3 ~ paste(strtrim(Description, 4), collapse = ","),
                length(Description)==4 ~ paste(strtrim(Description, 3), collapse = ","),
                TRUE                   ~ paste0(strtrim(Description[1], 4), ". & ", length(Description)-1, " more")
            )
        ) %>% 
        ungroup %>%
        select(-grouping)
    
    # refresh totals for each bar (grouped by Time & Budget)
    # refresh text label locations
    df %<>%
        group_by(Time, Budget) %>%
        arrange(id) %>%
        mutate(
            Amount_hi = cumsum(Amount),
            Amount_lo = Amount_hi - Amount) %>%
        ungroup %>% 
        mutate(y_pos_text = (Amount_hi + Amount_lo)/2) %>%
        ungroup 
    
    # compute labels/orientation based on plot space available
    df %<>%
        group_by(Time) %>%
        mutate(
            Description = case_when( # trim to fit if necessary
                horizontal_pixels > (nchar(Description) * pixels_per_letter) ~ 
                    paste0(Description, "\n$", Amount),
                horizontal_pixels > vertical_pixels ~ 
                    paste0(
                        strtrim(Description, round(horizontal_pixels / pixels_per_letter, 0)),
                        "\n$", Amount),
                vertical_pixels > (nchar(Description) * pixels_per_letter) &
                    horizontal_pixels > double_pixels ~ 
                    paste0(Description, "\n$", Amount),
                vertical_pixels > (nchar(Description) * pixels_per_letter) ~ Description,
                horizontal_pixels > double_pixels ~
                    paste0(strtrim(Description, round(vertical_letters, 0)), "\n$", Amount),
                TRUE ~ strtrim(Description, round(vertical_letters, 0))
            ),
            orientation = case_when( # set orientation
                horizontal_pixels > (nchar(Description) * pixels_per_letter) ~ 0,
                horizontal_pixels > vertical_pixels ~ 0,
                vertical_pixels > (nchar(Description) * pixels_per_letter) ~ -90,
                TRUE ~ -90                             
            )
        ) %>% 
        ungroup
    
    # set id order by making it a factor
    df$id <- factor(nrow(df) - df$id, 
                    levels = sort(unique(nrow(df) - df$id)))
        
    # showModal( # for debugging purposes -- see data going into plot
    #     modalDialog(
    #         renderDT({
    #             df
    #         }),
    #         easyClose = TRUE,
    #         size = 'l'
    #     )
    # )

    ggplot(df, aes(x = Budget, y = Amount, label = Description, fill = id)) +
        geom_col(col = "white") +
        geom_text(aes(y = y_pos_text, angle = orientation)) +
        facet_grid(rows = vars(Time)) +
        coord_flip() +
        theme_hc(base_size = 14, base_family = 'verdana') +
        xlab("Budget Item") +
        # scale_fill_continuous() + 
        scale_fill_manual(values = colorRampPalette(
            RColorBrewer::brewer.pal(
                min(12, unique(length(df$Description))), "Pastel1"))(
                    length(unique(df$Description)))) +
        theme(legend.position = "none") +
        ggtitle(label = "My Budget",
                subtitle = "Amount ($)") +
        ylab("Amount ($)")
}

verbose_balance <- function(income, expenses) {
    net <- financeBalance(income, expenses)
    
    if (net[1] == 0 & net[2] == 0) {# balance now, balance future
        "My income and expenses are in perfect balance. 
        An increase in income or decrease in
        expenses would allow me to save money for unexpected life
        events. Any reduction in income or additional
        expenses might affect my ability to be self-reliant."
    } else if (net[1] == 0 & net[2] < 0) {# balance now, negative future
        paste0("My finances are in balance now, but I see my balance
               going negative in the future. An increase in income or decrease in
               expenses covering $", abs(net[2]), " would be enough balance my budget
               under my future scenario and be more self-reliant.")
    } else if (net[1] == 0 & net[2] > 0) {# balance now, positive future
        paste0("My finances are in balance. In the future
               I expect to have a surplus of $", net[2], "/month. This would allow
               me to save for unexpected life events and become more self-reliant.")
    } else if (net[1] < 0 & net[2] == 0) {# negative now, balance future
        paste0("My finances leave a deficit of $", abs(net[1]), 
               " per month. I expect changes that will allow me to balance my budget
               in the future and become more self-reliant.")
    } else if (net[1] > 0 & net[2] == 0) {# positive now, balance future
        paste0("My current finances provide a surplus of $", net[1], 
               " per month although this surplus is expected to drop to $", net[2],
               "in the future. Saving now will help me weather a tight budget
               in the future.")
    } else if (net[1] < 0 & net[2] < 0) {# negative now, negative future
        paste0("My current finances leave a deficit of $", abs(net[1]), 
               " per month ($", abs(net[2]), " per month deficit in the future). Changes in my
               income or spending will be required to become self-reliant.")
    } else if (net[1] < 0 & net[2] > 0) {# negative now, positive future
        paste0("My monthly expenses are $", abs(net[1]), " more than my income. 
               However, I expect to have surplus of $", net[2],
               " per month in the future. Achieving these changes will support me
               in becoming financially self-reliant.")
    } else if (net[1] > 0 & net[2] < 0) {# positive now, negative future
        paste0("My current finances would allow me to save up to $", net[1], 
               " per month. However, I expect my to have deficit of $", abs(net[2]),
               " per month in the future. Saving now may help, but in the long
               run I will need more income or lower expenses to remain financially
               self-reliant.")
    } else if (net[1] > 0 & net[2] > 0) {# positive now, positive future
        paste0("My current finances provide a surplus of $", net[1], 
               " per month ($", net[2], " per month in the future). Saving some or
               all of this will make me financially self-reliant in hard times.")
    } else {
        "My budget is indescribable."
    }
    # if (net[1] == 0) {
    #     current_msg <- "My current spending and income are in balance." 
    #     # I should consider a plan to income my income or decrease my spending
    #     # so that I have the opportunity to become more financially prepared."
    # } else if (net[1] < 0) {
    #     current_msg <- paste0("My current spending is $", abs(net[1]), " more than 
    #         my income. I don't have enough income to cover my expenses.")
    # } else if (net[1] > 0) {
    #     current_msg <- paste0("My current income is $", abs(net[1]), " more 
    #         than my current spending.")
    # }
    # 
    # if (net[2] == 0) {
    #     future_msg <- "I anticipate that in the near future my spending and income will be in balance."
    # } else if (net[2] < 0) {
    #     future_msg <- paste0("I anticipate that in the near future my spending will be $", abs(net[2]), " more than 
    #         my income. I need a plan to address this shortfall.")
    # } else if (net[2] > 0) {
    #     future_msg <- paste0("I anticipate that in the near future my income will be $", abs(net[2]), " more 
    #         than my spending. I may have the opportunity to put money into savings and 
    #         become more financially prepared.")
    # }
    # return(paste(current_msg, future_msg))
}

# Define UI for application that draws a histogram
ui <- function(request) {
    fluidPage(
        theme = shinytheme('paper'),
        # themeSelector(),
        
        
        # screen dimensions ----
        tags$head(tags$script('
                                var dimension = [0, 0];
                                $(document).on("shiny:connected", function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                                $(window).resize(function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                            ')),
        
        # Application title
        titlePanel("Self-Reliance Plan"),
        
        tabsetPanel(
            id = "main_tabs",
            
            # step 1: needs tab ----
            tabPanel(
                title = "1",
                icon = icon("shopping-basket"),
                tags$span(
                    helpText("Use this app to create your self-reliance plan. 
                    Your bishop, branch president, Relief Society president, 
                    elders quorum president, or other leaders can assist you.",
                    tags$a("(original form)", target = "_blank", 
                           href = "https://www.churchofjesuschrist.org/bc/content/shared/content/english/pdf/welfare/PD60007387_000_SelfReliancePlan_Member_Web_Interactive.pdf"))
                ),
                tags$h4("What are my needs?"),
                helpText("Immediate needs may include food, clothing, medical or 
                emotional care, or housing. Longer-term needs may include education 
                or improved employment. Identify your needs in the space below."),
                textAreaInput("needs", NULL)
            ),
            
            # step 2: finances income tab ----
            tabPanel(
                title = '2',
                icon = icon("dollar-sign"),
                tags$h4("What are my income and expenses?"),
                
                tabsetPanel(
                    id = "finances",
                    type = "pills",
                    
                    tabPanel(
                        title = "Income",
                        icon = icon("money-bill"),
                        tags$h4("Monthly Income"),
                        DTOutput("income"),
                        helpText("Tap button to begin. Tap previous entry to edit or delete."),
                        actionButton("income_add", "Record Income", icon = icon("pen"))
                    ),
                    
                    # expenses tab ----
                    tabPanel(
                        title = "Expenses",
                        icon = icon("receipt"),
                        tags$h4("Monthly Expenses"),
                        DTOutput("expenses"),
                        helpText("Tap button to begin. Tap previous entry to edit or delete."),
                        actionButton("expenses_add", "Record Expense", icon = icon("pen"))
                    ),
                    
                    # balance tab ----
                    tabPanel(
                        title = "Summary",
                        icon = icon("chart-bar"),
                        fluidRow(
                            column(
                                width = 12,
                                tags$p(textOutput('balance'))
                            ),
                            column(
                                width = 12,
                                plotOutput('expenseIncomePlot')
                            )
                        )
                    )
                )
            ),
            
            # resources tab ----
            tabPanel(
                title = "3", #Resources",
                icon = icon("user-friends"), #toolbox"),
                tags$h4("What other resources are available?"),
                textAreaInput("resourcesIndividual", "Individual resources and skills"),
                textAreaInput("resourcesFamily", "Help and resources available from family members (parents, children, siblings, others)"),
                textAreaInput("resourcesCommunity", "Relevant community resources")        
            ),
            
            # plan tab ----
            tabPanel(
                title = "4", #Plan",
                icon = icon("list-ol"),
                tags$h4("What is my personal or family plan to become more self-reliant?"),
                tags$p("As part of your plan, consider participating in a self-reliance group."),
                DTOutput("plan"),
                helpText("Tap button to begin. Tap previous entry to edit or delete."),
                actionButton("plan_add", "Add Item", icon = icon("pen"))
            ),
            
            # service tab ----
            tabPanel(
                title = "5", #Service",
                icon = icon("handshake"),
                tags$h4("What work or service will I contribute in return for any assistance I may receive?"),
                textAreaInput("myServiceIdeas", "Ideas to share with the bishop or branch president"),
                textAreaInput("myServicePlan", "After consulting with the bishop or branch president, 
                              describe the work or service assignment you will do"),
                tags$h4("Commitment"),
                textInput("memberSign", "Member Signature (type name to sign)"),
                textInput("spouseSign", "Spouse Signature (type name to sign)"),
                downloadButton("report", "Download Full Report"),
                br(),
                hr(),
                tags$details(
                    "Your session information is captured in the address bar. Simply bookmark
                    this page to save it for later or copy the address into an email to share
                    or save. If your URL is more than 2000 characters it may not load properly in
                    some browsers. If this happens, try a non-Microsoft browser like Firefox, 
                    Safari, or Chrome. Please note that your financial information is never saved by this
                    tool, although the download and link saving (bookmarking) options allow you
                    to keep a record of your work.")    
            )
            
            # commitment tab ----
            # tabPanel(
            #     title = "", #Commitment",
            #     icon = icon("pen"),
            #     tags$p("Type your name to indicate your commitment to following
            #            through on your plan."),
            #     textInput("memberSign", "Member Signature (type name to sign)"),
            #     textInput("spouseSign", "Spouse Signature (type name to sign)"),
            #     downloadButton("report", "Download Full Report"),
            #     br(),
            #     hr(),
            #     helpText(
            #         "Your session information is captured in the address bar. Simply bookmark
            #         this page to save it for later or copy the address into an email to share
            #         or save. If your URL is more than 2000 characters it may not load properly in
            #         some browsers. If this happens, try a non-Microsoft browser like Firefox, 
            #         Safari, or Chrome. Please note that your financial information is never saved by this
            #         tool, although the download and link saving (bookmarking) options allow you
            #         to keep a record of your work.")
            # )
        )
    )
}
# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    # Session restore ----
    
    # Save extra values in state$values when we bookmark
    onBookmark(function(state) {
        state$values$income <- values$income
        state$values$expenses <- values$expenses
        state$values$plan <- values$plan
    })
    
    # Read values from state$values when we restore
    onRestore(function(state, url) {
        values$income <- as_tibble(state$values$income) %>% #comes back as list, make it tibble again 
            mutate(
                Description = as.character(Description),
                Current = as.numeric(Current),
                Future = if_else(is.na(Future), 
                                 as.numeric(Current), 
                                 as.numeric(Future)))
        values$expenses <- as_tibble(state$values$expenses) %>%
            mutate(
                Description = as.character(Description),
                Current = as.numeric(Current),
                Future = if_else(is.na(Future), 
                                 as.numeric(Current), 
                                 as.numeric(Future)))
        values$plan <- as_tibble(state$values$plan) %>%
            mutate(`Resources and skills needed for self-reliance` = 
                       as.character(`Resources and skills needed for self-reliance`),
                   `Steps to be taken` = as.character(`Steps to be taken`),
                   `By when` = as.Date(as.character(`By when`)))
    })
    
    # Exclude buttons from bookmarking
    setBookmarkExclude(
        c(paste0("income_col_", seq(3)),
          paste0("expenses_col_", seq(3)),
          paste0("plan_col_", seq(3)),
          outer(c("income", "expenses", "plan"), 
                c("_editrow", "_addrow", "_deleterow", 
                  "_rows_selected", "_add",
                  "_cell_clicked", "_row_last_clicked", "_rows_all", "_rows_current"), 
                FUN = "paste0")[1:15],
          
          "memberSign", "spouseSign"))
    
    observe({
        # Trigger this observer every time an input changes
        reactiveValuesToList(input)
        session$doBookmark()
    })
    
    onBookmarked(function(url) {
        updateQueryString(url)
    })
    
    # screen dimensions ----
    # output$dimension_display <- renderText({
    #     paste("dims:", input$dimension[1], input$dimension[2], input$dimension[2]/input$dimension[1])
    # }) %>% debounce(2000)
    
    my_screen_width <- reactiveVal(300)
    observeEvent(input$dimension, { #require 2% change in screen width for a graph update
        if ((abs(input$dimension[1] - my_screen_width()) / my_screen_width()) > 0.02) {
            my_screen_width(input$dimension[1])
        }
    })
    
    # initialize tables ----
    values <- reactiveValues()
    
    values[['income']] <- tibble(
        Description = character(0),
        Current     = numeric(0),
        Future      = numeric(0)
    )
    
    values[['expenses']] <- tibble(
        Description = character(0), 
        Current     = numeric(0),
        Future      = numeric(0)
    )
    
    values[['plan']] <- tibble(
        `Resources and skills needed for self-reliance` = character(0),
        `Steps to be taken`                             = character(0),
        `By when`                                       = character(0) %>% as.Date
    )
    
    # 'add' modals (using dfInput) ----
    # income add modal
    observeEvent(input$income_add, {
        showModal(modalDialog(
            dfInputs(values$income, 
                     row = 0,
                     labels = c("Income Description",
                                "Current Monthly Amount ($)",
                                "Future Monthly Amount -- if different ($)")),
            hr(),
            tags$p("Please consider:"),
            tags$ul(
                tags$li("Income from all household members"),
                tags$li("Other financial sources (family, others)"),
                tags$li("Government assistance (financial, food, housing, and so forth)")
            ),
            easyClose = TRUE,
            footer = NULL
        ))
    })
    
    # expense add modal
    observeEvent(input$expenses_add, {
        showModal(modalDialog(
            dfInputs(values$expenses, 
                     row = 0,
                     labels = c("Expense Description",
                                "Current Monthly Amount ($)",
                                "Future Monthly Amount -- if different ($)")),
            hr(),
            tags$p("Please consider:"),
            tags$ul(
                tags$li("Tithes, offerings"),
                tags$li("Debt payments"),
                tags$li("Food, clothing, housing"),
                tags$li("Electricity, fuel, water"),
                tags$li("Education, medical, transportation"),
                tags$li("Entertainment, phone, etc.")
            ),
            tags$p("Enter a future amount if an expense can be reduced, eliminated, or may increase."),
            easyClose = TRUE,
            footer = NULL
        ))
    })
    
    # plan add modal
    observeEvent(input$plan_add, {
        showModal(modalDialog(
            dfInputs(values$plan, row = 0),
            easyClose = TRUE,
            footer = NULL
        ))
    })
    
    # 'edit' modals (using dfInput) ----
    # income edit modal
    income_proxy = dataTableProxy('income')
    observeEvent(input$income_rows_selected, {
        income_proxy %>% selectRows(tail(input$income_rows_selected, 1))
        row_number = input$income_rows_selected
        showModal(modalDialog(
            dfInputs(values$income,
                     row = row_number,
                     labels = c("Income Description",
                                "Current Monthly Amount ($)",
                                "Future Monthly Amount -- if different ($)")),
            easyClose = TRUE,
            footer = NULL
        ))
    })
    
    # expense edit modal
    expenses_proxy = dataTableProxy('expenses')
    observeEvent(input$expenses_rows_selected, {
        
        expenses_proxy %>% selectRows(tail(input$expenses_rows_selected, 1))
        row_number = input$expenses_rows_selected
        showModal(modalDialog(
            dfInputs(values$expenses, 
                     row = row_number,
                     labels = c("Expense Description",
                                "Current Monthly Amount ($)",
                                "Future Monthly Amount -- if different ($)")),
            easyClose = TRUE,
            footer = NULL
        ))
    })
    
    # plan edit modal
    plan_proxy = dataTableProxy('plan')
    observeEvent(input$plan_rows_selected, {
        
        plan_proxy %>% selectRows(tail(input$plan_rows_selected, 1))
        row_number = input$plan_rows_selected
        showModal(modalDialog(
            dfInputs(values$plan, 
                     row = row_number),
            easyClose = TRUE,
            footer = NULL
        ))
    })
    
    # 'add' (using dfEdit) ----
    # add: income table
    observeEvent(input$income_addrow, {
        values[['income']] <- dfEdit(values$income, input, action = "Add")
        values[['income']] %<>% mutate(Future = ifelse(is.na(Future), Current, Future))
        removeModal()
    })
    
    # add: expense table
    observeEvent(input$expenses_addrow, {
        values[['expenses']] <- dfEdit(values$expenses, input, action = "Add")
        values[['expenses']] %<>% mutate(Future = ifelse(is.na(Future), Current, Future))
        removeModal()
    })
    
    # add: plan table
    observeEvent(input$plan_addrow, {
        values[['plan']] <- dfEdit(values$plan, input, action = "Add")
        removeModal()
    })
    
    # 'edit' (using dfEdit) ----
    # edit: income table
    observeEvent(input$income_editrow, {
        values[['income']] <- dfEdit(values$income, input, action = "Modify")
        values[['income']] %<>% mutate(Future = ifelse(is.na(Future), Current, Future))
        removeModal()
    })
    
    # edit: expense table
    observeEvent(input$expenses_editrow, {
        values[['expenses']] <- dfEdit(values$expenses, input, action = "Modify")
        values[['expenses']] %<>% mutate(Future = ifelse(is.na(Future), Current, Future))
        removeModal()
    })
    
    # edit: plan table
    observeEvent(input$plan_editrow, {
        values[['plan']] <- dfEdit(values$plan, input, action = "Modify")
        removeModal()
    })
    
    # 'delete' (using dtEdit) ----
    # delete income row
    observeEvent(input$income_deleterow, {
        values[['income']] <- dfEdit(values$income, input, action = "Delete")
        removeModal()
    })
    
    # delete expense row
    observeEvent(input$expenses_deleterow, {
        values[['expenses']] <- dfEdit(values$expenses, input, action = "Delete")
        removeModal()
    })
    
    # delete plan row
    observeEvent(input$plan_deleterow, {
        values[['plan']] <- dfEdit(values$plan, input, action = "Delete")
        removeModal()
    })
    
    # display tables ----
    # display income table
    output$income = renderDT(
        if (nrow(values[['income']]) > 0){
            financeTable(values[['income']])
        }
    )
    
    # display expense table
    output$expenses = renderDT(
        if (nrow(values[['expenses']]) > 0){
            financeTable(values[['expenses']])
        }
    )    
    
    # display plan table
    output$plan = renderDT(
        if (nrow(values[['plan']]) > 0){
            planTable(values[['plan']])
        }
    )
    
    # balance plots ----
    output$balance <- renderText({
        verbose_balance(values[['income']], values[['expenses']])
    })
    
    # output$expenseDonutPlot <- renderPlot({
    #     financePlot(values[['income']], values[['expenses']])
    # })
    
    output$expenseIncomePlot <- renderPlot({
        financePlot(values[['income']], values[['expenses']], 
                     screen_dim = my_screen_width())
    }, height = 500)
    
    # make report ----
    output$report <- downloadHandler(
        # For PDF output, change this to "report.pdf"
        filename = "report.html",
        content = function(file) {
            # Copy the report file to a temporary directory before processing it, in
            # case we don't have write permissions to the current working dir (which
            # can happen when deployed).
            tempReport <- file.path(tempdir(), "report.Rmd")
            file.copy("report.Rmd", tempReport, overwrite = TRUE)
            
            # Set up parameters to pass to Rmd document
            author <- if_else(
                input$spouseSign == "",
                input$memberSign,
                paste(c(input$memberSign, input$spouseSign), collapse = " and ")
            )
            
            params <- list(
                set_author = author,
                set_member = input$memberSign,
                set_spouse = input$spouseSign,
                set_date = format(Sys.Date(), "%d %b %Y"),
                set_needs = input$needs,
                set_income = financeTable(values[['income']]),
                set_expenses = financeTable(values[['expenses']]),
                set_financePlot = financePlot(values[['income']], 
                                               values[['expenses']],
                                               screen_dim = 800),
                set_financeBalance = verbose_balance(values[['income']], 
                                                     values[['expenses']]),
                set_resourcesIndividual = input$resourcesIndividual,
                set_resourcesFamily = input$resourcesFamily,
                set_resourcesCommunity = input$resourcesCommunity,
                set_myPlan = planTable(values[['plan']]),
                set_serviceIdeas = input$myServiceIdeas,
                set_servicePlan = input$myServicePlan
            )
            
            # Knit the document, passing in the `params` list, and eval it in a
            # child of the global environment (this isolates the code in the document
            # from the code in this app).
            rmarkdown::render(tempReport, output_file = file,
                              params = params,
                              envir = new.env(parent = globalenv())
            )
        }
    )
}

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")
