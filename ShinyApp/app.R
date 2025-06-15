# app.R ---------------------------------------------------------------
library(shiny)
library(jsonlite)
library(dplyr)
library(purrr)
library(visNetwork)
library(DT)
library(tidygraph)
library(ggraph)
library(patchwork)
library(grid)
library(ggplot2)

# ── 1. master colour map ---------------------------------------------
legend_cols <- c(
  Person        = "#1b9e77",
  Vessel        = "#d95f02",
  Organization  = "#7570b3",
  Relationship  = "#e7298a",
  Group         = "#e6ab02",
  Location      = "#66a61e"
)

# ── 2. load + preprocess ---------------------------------------------
graph <- read_json("Data/MC3_graph.json", simplifyVector = TRUE)

events_df <- graph$nodes %>%
  filter(type == "Event", !is.na(timestamp)) %>%
  mutate(date = as.Date(timestamp)) %>%
  filter(date >= as.Date("2040-10-01"), date <= as.Date("2040-10-15"))

min_date <- min(events_df$date)
max_date <- max(events_df$date)

nodes_all <- graph$nodes %>%
  mutate(category = if_else(type == "Relationship", "Relationship", sub_type)) %>%
  filter(category %in% names(legend_cols)) %>%
  mutate(color = legend_cols[category])

edges_all <- graph$edges %>%
  transmute(from   = source,
            to     = target,
            type   = type,      # 'sent', 'received', etc.
            arrows = "to") %>%
  left_join(events_df %>% select(id, date), by = c("from" = "id")) %>%
  rename(date_from = date) %>%
  left_join(events_df %>% select(id, date), by = c("to" = "id")) %>%
  rename(date_to   = date) %>%
  mutate(event_date = coalesce(date_from, date_to)) %>%
  select(-date_from, -date_to)

# ── 3. UI -------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Mini Challenge 3"),
  sidebarLayout(
    sidebarPanel(
      selectInput("classes", "Select Entity/Entities to Display:",
                  choices  = names(legend_cols),
                  selected = c("Person", "Vessel"),
                  multiple = TRUE),
      sliderInput("dateRange", "Event-date window:",
                  min = min_date, max = max_date,
                  value = c(min_date, max_date),
                  timeFormat = "%Y-%m-%d"),
      checkboxInput("isolates", "Keep isolated nodes", FALSE),
      tags$hr(), tags$h5("Legend"),
      ## dynamic legend based on legend_cols -------------------------
      tagList(lapply(names(legend_cols), function(cat) {
        tags$div(style="display:flex; align-items:center; margin-bottom:4px;",
                 tags$span(style=sprintf(
                   "display:inline-block;width:14px;height:14px;border-radius:50%%;\
                    background:%s;margin-right:6px;", legend_cols[cat])),
                 tags$span(cat)
        )
      })),
      width = 3
    ),
    mainPanel(
      tabsetPanel(id = "tabs",
                  
                  tabPanel("Data Table",
                           tabsetPanel(type = "tabs",
                                       tabPanel("Nodes",  DTOutput("nodes_table_dt")),
                                       tabPanel("Edges",  DTOutput("edges_table_dt"))
                           )
                  ),
                  
                  tabPanel("Heatmap & Frequency of Communication",
                           
                           selectInput("focus_id", "Select by id",
                                       choices = "All", selected = "All"),
                           visNetworkOutput("pv_net", height = "700px")
                  ),
                      
                  # visNetwork 
                  tabPanel("Interactive Network",
                           visNetworkOutput("net", height = "800px"), br(),
                           tabsetPanel(type = "tabs",
                                       tabPanel("Nodes", DTOutput("nodes_table")),
                                       tabPanel("Edges", DTOutput("edges_table"))
                           )
                  ),
                  
                  # original daily plot 
                  tabPanel("Daily Communication Graph & Timeline",
                           plotOutput("dailyPlot")
                  )

      )
    )
  )
)

# ── 4. Server ---------------------------------------------------------
server <- function(input, output, session) {
  
  # ---- a. helpers for your existing network / tables / plots ---------
  edges_date <- reactive({
    edges_all %>%
      filter(is.na(event_date) |
               (event_date >= input$dateRange[1] &
                  event_date <= input$dateRange[2]))
  })
  
  nodes_r <- reactive({
    req(input$classes)
    ids <- unique(c(edges_date()$from, edges_date()$to))
    
    n <- nodes_all %>%
      filter(category %in% input$classes) %>%
      mutate(
        color = if_else(category %in% names(legend_cols),
                        legend_cols[category],
                        NA_character_)
      ) %>%
      filter(!is.na(color))  # remove anything that doesn't match the legend
    
    if (!isTRUE(input$isolates)) {
      n <- n %>% filter(id %in% ids)
    }
    n
  })
  
  
  edges_r <- reactive({
    ids <- nodes_r()$id
    edges_date() %>% filter(from %in% ids & to %in% ids)
  })
  
  # --- Data-Table tab ---------------------------------------------------
  output$nodes_table_dt <- renderDT(
    nodes_r() %>% 
      select(id, label, category, color),
    options  = list(pageLength = 10),
    rownames = FALSE
  )
  
  output$edges_table_dt <- renderDT(
    edges_r() %>% 
      select(from, to, type, event_date),
    options  = list(pageLength = 10),
    rownames = FALSE
  )
  
  # --- Visnetwork tab
  output$net <- renderVisNetwork({
    net_nodes <- nodes_r() %>%
      mutate(
        category = if_else(category %in% names(legend_cols), category, NA_character_),
        color = legend_cols[category]
      ) %>%
      filter(!is.na(color)) %>%   # removes nodes without defined colors
      transmute(
        id, label,
        color.background = color,
        color.border     = color,
        color.highlight  = Map(function(c) list(background = c, border = "#000000"), color)
      )
    
    net_edges <- edges_r() %>%
      transmute(from, to, arrows = "to", label = type)
    
    if (nrow(net_nodes) == 0) return(visNetwork(data.frame(), data.frame()))
    
    visNetwork(net_nodes, net_edges) %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visPhysics(stabilization = FALSE)
  })
  
  # ---- tables --------------------------------------------------------
  output$nodes_table <- renderDT(
    nodes_r()  %>% select(id, label, category, color),
    options = list(pageLength = 10), rownames = FALSE)
  output$edges_table <- renderDT(
    edges_r() %>% select(from, to, type, event_date),
    options = list(pageLength = 10), rownames = FALSE)
  
  # ---- your existing daily faceted plot (unchanged) ------------------
  # ... daily_plot() definition unchanged from your current script ...
  daily_plot <- reactive({
    
    keep_cls <- input$classes
    if (length(keep_cls) < 2) return(NULL)       # need at least two classes
    
    ## 1  communication edges inside the date window ---------------------
    e_full <- edges_date() %>%
      filter(type %in% c("sent","received"),
             !is.na(event_date))
    if (nrow(e_full) == 0) return(NULL)
    
    ## 2  build sender → receiver pairs via the comm node ---------------
    es <- e_full %>% filter(type == "sent")      %>%
      transmute(comm = to,   sender = from,  event_date)
    er <- e_full %>% filter(type == "received")  %>%
      transmute(comm = from, receiver = to,  event_date)
    
    pv_edges <- inner_join(es, er,
                           by = c("comm","event_date")) %>%
      transmute(from = sender, to = receiver, event_date)
    
    if (nrow(pv_edges) == 0) return(NULL)
    
    ## 3  look up the categories of each endpoint -----------------------
    cat_lookup <- nodes_all %>% select(id, category)
    
    pv_edges <- pv_edges %>%
      left_join(cat_lookup, by = c("from" = "id")) %>%
      rename(cat_from = category) %>%
      left_join(cat_lookup, by = c("to" = "id")) %>%
      rename(cat_to   = category) %>%
      # keep ONLY selected classes
      filter(cat_from %in% keep_cls,                   
             cat_to   %in% keep_cls)
    
    if (nrow(pv_edges) == 0) return(NULL)
    
    ## 4  node table limited to kept classes ----------------------------
    node_ids <- unique(c(pv_edges$from, pv_edges$to))
    pv_nodes <- nodes_all %>%
      filter(id %in% node_ids, category %in% keep_cls) %>%
      select(id, label, category)
    
    ## 5  one facet per day ---------------------------------------------
    plots <- lapply(sort(unique(pv_edges$event_date)), function(d){
      
      g <- tbl_graph(
        nodes = pv_nodes %>% filter(id %in% c(
          pv_edges$from[pv_edges$event_date == d],
          pv_edges$to  [pv_edges$event_date == d] )),
        edges = pv_edges %>% filter(event_date == d),
        directed = TRUE)
      
      ggraph(g, layout = "fr") +
        geom_edge_link(
          arrow       = grid::arrow(length = grid::unit(2, "mm")),
          edge_colour = "#666666", edge_alpha = 0.5) +
        geom_node_point(aes(color = category), size = 3) +
        geom_node_text(aes(label = label), repel = TRUE, size = 3) +
        scale_color_manual(values = legend_cols, breaks = names(legend_cols)) +
        ggtitle(as.character(d)) +
        theme_void() +
        theme(legend.position = "none",
              plot.title = element_text(size = 10, hjust = 0.5),
              plot.margin     = margin(t = 10, r = 5, b = 20, l = 5))
    })
    
    wrap_plots(plots, ncol = 2)
  })
  
  
  # keep your existing code here
  output$dailyPlot <- renderPlot(
    height = function() {
      ndays <- length(unique(edges_date()$event_date))
      row   <- ceiling(ndays / 2)                # 2 panels per row (ncol = 2)
      max(1, row) * 200                          # ~200 px per row
    },
    res = 96,                                    # keep your resolution
    { daily_plot() }                             # <- the existing expr
  )

  

  #  COMMUNICATION NETWORK  <<<  ---------------

  
  # (1) Rebuild sender → receiver pairs
  pv_pairs_full <- reactive({
    
    e_full <- edges_date() %>%                 # already filtered by date
      filter(type %in% c("sent","received"),   # keep comm edges
             !is.na(event_date))
    if (nrow(e_full) == 0)
      return(tibble(from = character(), to = character()))
    
    es <- e_full %>% filter(type == "sent") %>%
      transmute(comm = to,   sender   = from)
    er <- e_full %>% filter(type == "received") %>%
      transmute(comm = from, receiver = to)
    
    pairs <- inner_join(es, er, by = "comm") %>%  
      transmute(from = sender, to = receiver)
    
    # attach categories for both ends
    pairs <- pairs %>%
      left_join(nodes_all %>% select(id, category),
                by = c("from" = "id")) %>%
      rename(cat_from = category) %>%
      left_join(nodes_all %>% select(id, category),
                by = c("to"   = "id")) %>%
      rename(cat_to   = category)
    
    # ==== NEW: keep *any* combination picked in the sidebar ============
    keep_cls <- input$classes                  # Person / Vessel / Org / …
    pairs %>% filter(cat_from %in% keep_cls,
                     cat_to   %in% keep_cls)
  })
  
  
  # (2) Aggregate counts  ----------------------------------------------
  pv_edges_full <- reactive({
    pv_pairs_full() %>%
      count(from, to,  name = "weight")
  })
  
  # (3) update dropdown choices whenever the data window changes -------
  observe({
    ids <- unique(c(pv_edges_full()$from, pv_edges_full()$to))
    labs <- nodes_all %>% filter(id %in% ids) %>% arrange(label)
    choices <- setNames(c("All", labs$id), c("All", labs$label))
    updateSelectInput(session, "focus_id", choices = choices)
  })
  
  # (4) Filter by focus -------------------------------------------------
  pv_edges <- reactive({
    if (input$focus_id == "All") {
      pv_edges_full()
    } else {
      pv_edges_full() %>% filter(from == input$focus_id | to == input$focus_id)
    }
  })
  
  pv_nodes <- reactive({
    ids <- unique(c(pv_edges()$from, pv_edges()$to))
    nodes_all %>% 
      filter(id %in% ids) %>% 
      transmute(id, label, group = category, color)
  })
  
  # (5) Render ----------------------------------------------------------
  output$pv_net <- renderVisNetwork({
    
    # 0. bail out early if nothing to show
    req(nrow(pv_edges()) > 0)
    
    # 1. node + edge tables ---------------------------------------------
    net_nodes <- pv_nodes()
    
    net_edges <- pv_edges() %>% 
      transmute(
        from, to,
        label  = weight,                        # edge label
        title  = paste("Count:", weight),       # tooltip on hover
        width  = 1 + weight,                    # >>> thicker shaft
        arrows = "to"                           # default arrow head
      )
    
    # 2. build the visNetwork widget ------------------------------------
    visNetwork(net_nodes, net_edges) %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visEdges(smooth = FALSE,
               arrows = list(to = list(enabled = TRUE,
                                       scaleFactor = 0.5))) %>%  # global head size
      visPhysics(stabilization = FALSE)
  })
  # --------------------------------------------------------------------
  
}

shinyApp(ui, server)
