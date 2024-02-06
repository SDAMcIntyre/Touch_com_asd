# inspiration from https://github.com/kassambara/ggcorrplot/blob/master/R/ggcorrplot.R

corr_matrix_df <- function(corr_test_result, var_labels) {
  plot_mat <- corr_test_result$r * as.numeric(corr_test_result$p < 0.05)
  plot_mat[lower.tri(plot_mat)] <- NA
  diag(plot_mat) <- NA
  
  plot_mat %>% 
    as_tibble(rownames = "variable1") %>% 
    pivot_longer(col = !variable1, 
                 names_to = "variable2", values_to = "value") %>% 
    mutate(
      variable1 = factor(
        variable1, levels = rownames(plot_mat), 
        labels = var_labels 
      ),
      variable2 = factor(
        variable2, levels = rownames(plot_mat), 
        labels = var_labels
      ),
      r_label = case_when(
        value != 0 ~ as.character(round(value,2)),
        TRUE ~ ""
      ),
      outline = case_when(
        !is.na(value) ~ "yes",
        TRUE ~ "no"
      )
    )
}

corr_matrix_plot <- function(corr_matrix_df, colourbar_height = 10) {
  corr_matrix_df %>% 
    ggplot(aes( x = variable1, y = variable2)) +
    geom_tile(aes(fill = value, colour = outline)) +
    geom_text(aes(label = r_label), size = 4) +
    scale_fill_gradient2(
      name ='Corr', 
      low="#8E2043", mid = "white", high="#077187",
      na.value = "transparent",
      guide = guide_colorbar(barheight = colourbar_height), limits = c(-1,1)) +
    scale_colour_manual(
      values = c("yes" = "grey", "no" = "transparent"), 
      guide = guide_none()
    ) +
    scale_x_discrete(position = "top") +
    theme_minimal(base_size = 16) +
    theme(axis.text.x=element_text(angle=45, hjust = 0 )) + 
    labs(x = NULL, y = NULL) 
}