# ===================================================
# MODEL SPECIFICATIONS
# ===================================================

model_specs <- list(
  
  "Auto ARDL" = function(df) {
    
    auto_ardl(
      margin ~ bn + covid,
      data        = df,
      
      # max lag orders:
      # Y up to 4
      # X up to 4
      # covid fixed contemporaneous
      
      max_order   = c(4,4,0),
      
      # FORCE Y lag >= 1
      # first element = Y lag
      
      fixed_order = c(1,-1,0),
      
      selection   = "AIC"
    )$best_model
  },
  
  "ARDL(2,1)" = function(df) {
    
    ardl(
      margin ~ bn + covid,
      data  = df,
      order = c(2,1,0)
    )
  },
  
  "ARDL(2,2)" = function(df) {
    
    ardl(
      margin ~ bn + covid,
      data  = df,
      order = c(2,2,0)
    )
  },
  
  "ARDL(2,3)" = function(df) {
    
    ardl(
      margin ~ bn + covid,
      data  = df,
      order = c(2,3,0)
    )
  },
  
  "ARDL(1,2)" = function(df) {
    
    ardl(
      margin ~ bn + covid,
      data  = df,
      order = c(1,2,0)
    )
  }
)


# ===================================================
# FIT ALL MODELS
# ===================================================

all_models <- purrr::imap(
  
  datasets,
  
  ~ {
    
    df <- as.data.frame(.x)
    
    purrr::imap(
      model_specs,
      ~ .x(df)
    )
  }
)


# ===================================================
# MODEL COMPARISON
# ===================================================

comparison_table <- purrr::imap_dfr(
  
  all_models,
  
  ~ {
    
    purrr::imap_dfr(
      
      .x,
      
      ~ tibble(
        
        dataset = .y,
        
        model = .y,
        
        AIC = AIC(.x),
        
        BIC = BIC(.x)
      )
    )
  }
)

comparison_table

library(gt)

# ===================================================
# MODEL COMPARISON TABLE
# ===================================================

comparison_table <- purrr::imap_dfr(
  
  all_models,
  
  ~ {
    
    dataset_name <- .y
    
    purrr::imap_dfr(
      
      .x,
      
      ~ tibble(
        
        dataset =
          dataset_name,
        
        model =
          .y,
        
        AIC =
          round(
            AIC(.x),
            2
          ),
        
        BIC =
          round(
            BIC(.x),
            2
          )
      )
    )
  }
) |>
  
  group_by(dataset) |>
  
  arrange(
    AIC,
    .by_group = TRUE
  ) |>
  
  mutate(
    
    AIC_rank =
      row_number()
  ) |>
  
  ungroup()

# ===================================================
# NICE OUTPUT
# ===================================================

comparison_table |>
  
  gt(
    groupname_col = "dataset"
  ) |>
  
  cols_label(
    
    model =
      "Model",
    
    AIC =
      "AIC",
    
    BIC =
      "BIC",
    
    AIC_rank =
      "AIC Rank"
  ) |>
  
  tab_header(
    
    title =
      "ARDL Model Comparison",
    
    subtitle =
      "Models Ranked by AIC Within Each Dataset"
  ) |>
  
  fmt_number(
    
    columns =
      c(AIC, BIC),
    
    decimals = 2
  ) |>
  
  data_color(
    
    columns = AIC_rank,
    
    palette = c(
      "#d4f4dd",
      "#fff3bf",
      "#ffd6d6"
    )
  )
