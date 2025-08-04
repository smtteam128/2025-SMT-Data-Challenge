#--------------------Note----------------------------------------------------
#' All of the variables should have been loaded in from the previous scripts. This file
#' contains the visualization code and some data wrangling when necessary. Please refer to the
#' ReadMe or previous R scripts to properly wrangle the data
library(ggplot2)
library(ggtext) 
library(forcats)
library(scales)

#Visualization 1:Model 1 Importance ---------------------------------------
naive_importance <- as.data.frame(importance)
naive_importance <- rownames_to_column(naive_importance, var = "feature")
naive_importance <- select(naive_importance, feature, importance = MeanDecreaseAccuracy)

naive_importance <- naive_importance %>%
  mutate(feature = case_when( #TODO: Capitalize
    feature == "distance_to_target" ~ "Distance To Target",
    feature == "hang_time" ~ "Hang Time",
    feature == "r" ~ "Distance from home plate",
    feature == "theta" ~ "Angle from home plate",
    feature == "direction_deg" ~ "Target direction"
  ))
naive_importance$importance <- naive_importance$importance/100


ggplot(naive_importance, aes(x = fct_reorder(feature, importance), 
                             y = importance, 
                             fill = feature)) +
  geom_bar(stat = "identity", 
           color = "gray20") + 
  coord_flip() +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "<span style = 'font-size:25pt;'>**Which factors are most important in the naive catch probability model?** </span> <br>
    <span style = 'font-size:17pt;'>Values over 100 indicate the model performs *worse* than random chance", 
    y = "Importance (%)", 
    x = ""
  ) +
  scale_fill_manual(values = c("#887094", "#B49149", "#66b2b2", "#dfc471", "#bea4cb")) +
  theme_minimal() + 
  theme(
    plot.title = ggtext::element_markdown(family = "serif", 
                                          size = 18,
                                          lineheight = 1.2, 
                                          ),  
    axis.title.y = element_text(size = 17, face = "bold"),  
    axis.text.y = element_text(size = 14, face = "bold"),
    axis.text = element_text(family = "serif", color = "black", size = 13), 
    axis.title.x = element_text(size = 17, family = "serif", face = "bold"),
    legend.position = "none",
    panel.background = element_rect(fill = "gray85", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

#Visual 2: Model evaluation----------------------------------------------
test_probs <- predict(naive_rf, test_df, type = "prob")[,2] 
roc_curve <- roc(test_df$is_out, test_probs)

plot(roc_curve, 
     main = paste("ROC Curve (Area Under Curve (AUC) =", round(auc(roc_curve), 4), ")"),
     col = "blue", lwd = 2)
abline(a = 0, b = 1, lty = 2, col = "red") 




#Visual 3: Importance of speed on catch probability ------------------------
plays_sprints$graph_residual <- abs(plays_sprints$residual*100)
ggplot(plays_sprints, aes(x = naive_catch_prob, y = abs(graph_residual))) +
  geom_smooth() +
  scale_x_continuous(labels = percent_format()) +
  labs(
    title = "<span style = 'font-size:25pt;'>**How much does speed effect catch probability?** </span><br>
    <span style = 'font-size:17pt;'>  The residual represents the percentage difference between the speed-adjusted model and the naive model </span>", 
    y = "Absolute residual between both models (percentage points)", 
    x = "Naive Catch Probability"
  ) +
  theme_minimal() + 
  theme(
    plot.title = element_markdown(family = "serif", size = 18, lineheight = 1.2, 
                                          margin = ggplot2::margin(t = 10, b = 10)),  
    axis.title.y = element_text(size = 17, face = "bold", family = "serif"),  
    axis.text.y = element_text(size = 14),
    axis.text = element_text(family = "serif", color = "black", size = 13), 
    axis.title.x = element_text(size = 17, family = "serif", face = "bold"),
    legend.position = "none",
    panel.background = element_rect(fill = "gray85", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

#Visual 4 - Naive probability vs. Adjusted probability ----------------------
ggplot(players, aes(x = exp_naive_prob, y = exp_adj_prob)) +
  geom_smooth(se = FALSE) +
  geom_point() +
  scale_y_continuous(labels = percent_format()) +
  scale_x_continuous(labels = percent_format()) +
  labs(
    title = "<span style = 'font-size:25pt;'>**Naive catch probability vs. adjusted catch probability** </span><br>
    <span style = 'font-size:17pt;'>  x axis is our original model and the y axis takes player speed into account </span>", 
    y = "Adjusted Catch Probability", 
    x = "Naive Catch Probability"
  ) +
  theme_minimal() + 
  theme(
    plot.title = element_markdown(family = "serif", size = 18, lineheight = 1.2, 
                                  margin = ggplot2::margin(t = 10, b = 10)),  
    axis.title.y = element_text(size = 17, face = "bold", family = "serif"),  
    axis.text.y = element_text(size = 14),
    axis.text = element_text(family = "serif", color = "black", size = 13), 
    axis.title.x = element_text(size = 17, family = "serif", face = "bold"),
    legend.position = "none",
    panel.background = element_rect(fill = "gray85", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

#Visualization 5 - Outs Above Average vs. Total Responsibility Score ----------

#Get the R-squared value
model <- lm(oaa ~ trs, data = player_responsibility_score)
r_squared <- summary(model)$r.squared


ggplot(player_responsibility_score, aes(x = oaa, y = trs)) +
geom_smooth(method = "lm", se = FALSE) +
geom_point() +
  labs(
    title = "<span style = 'font-size:25pt;'>**Comparing True Responsibility Score to Outs Above Average** </span>", 
    y = "True Responsibility Score (Adjusted)", 
    x = "Outs Above Average (Naive)"
  ) +
  annotate("text", 
           x = max(player_responsibility_score$oaa, na.rm = TRUE) * 0.8, 
           y = max(player_responsibility_score$trs, na.rm = TRUE) * 0.9,
           label = paste("R² =", round(r_squared, 3)), 
           size = 4) +
  theme_minimal() + 
  theme(
    plot.title = element_markdown(family = "serif", size = 18, lineheight = 1.2, 
                                  margin = ggplot2::margin(t = 10, b = 10)),  
    axis.title.y = element_text(size = 17, face = "bold", family = "serif"),  
    axis.text.y = element_text(size = 14),
    axis.text = element_text(family = "serif", color = "black", size = 13), 
    axis.title.x = element_text(size = 17, family = "serif", face = "bold"),
    legend.position = "none",
    panel.background = element_rect(fill = "gray85", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

#Visualization 6 - Histogram of player aggressiveness --------------------------------
ggplot(player_responsibility_score, aes(x = agg_index)) +
  geom_histogram(bins = 15, fill = "steelblue", color = "white", boundary = 0) +
  labs(
    title = "<span style='font-size:22pt;'>**Distribution of average aggressiveness index**</span>",
    x = "Aggressiveness index",
    y = "Number of Players"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_markdown(family = "serif", size = 18, lineheight = 1.2, 
                                 ),  
    axis.title.y = element_text(size = 17, face = "bold", family = "serif"),  
    axis.title.x = element_text(size = 17, family = "serif", face = "bold"),
    axis.text.y = element_text(size = 14),
    axis.text = element_text(family = "serif", color = "black", size = 13), 
    legend.position = "none",
    panel.background = element_rect(fill = "gray85", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

#Visualization 7 - Table of most aggressive teams -------------------------
trio_stats_viz <- select(trio_stats, player_1, player_2, player_3, aggressiveness_score)
trio_stats_viz <- trio_stats_viz %>%
  arrange(desc(aggressiveness_score)) %>%
  mutate(aggressiveness_score = round(aggressiveness_score, 3))

library(gt)
library(dplyr)
viz1 <- head(trio_stats_viz, 10)
gt_data1 <- viz1 %>%
  select(player_1, player_2, player_3, aggressiveness_score) %>%
  gt() %>%
  tab_header(
    title = md("**Which Outfield Trios Are Most Aggressive?**"),
    subtitle = md("*High aggressiveness score means multiple players do not like sharing*")
  ) %>%
  cols_label(
    player_1 = "Player 1",
    player_2 = "Player 2",
    player_3 = "Player 3",
    aggressiveness_score = "Aggressiveness Score (Percentile)"
  ) %>%
  cols_align(
    align = "center"
  ) %>%
  data_color(
    columns = c(aggressiveness_score),
    method = "numeric",
    palette = c("#0D0887", "#E16462", "#FCA636", "#F0F921"),
    domain = range(trio_stats_viz$aggressiveness_score, na.rm = TRUE),
    reverse = TRUE
  ) %>%
  opt_table_font(
    font = list(
      google_font(name = "Times New Roman"),
      "Serif"
    )
  )
gt_data1

#Visualization 8 - Passive trios ---------------------------------------
trio_stats_viz <- select(trio_stats, player_1, player_2, player_3, aggressiveness_score)
trio_stats_viz <- trio_stats_viz %>%
  arrange((aggressiveness_score)) %>%
  mutate(aggressiveness_score = round(aggressiveness_score, 3))


library(gt)
library(dplyr)
viz2 <- head(trio_stats_viz, 10)
gt_data2 <- viz2 %>%
  select(player_1, player_2, player_3, aggressiveness_score) %>%
  gt() %>%
  tab_header(
    title = md("**Which Outfield Trios Are Most Passive?**"),
    subtitle = md("*A low aggressiveness score means multiple players are not assertive enough*")
  ) %>%
  cols_label(
    player_1 = "Player 1",
    player_2 = "Player 2",
    player_3 = "Player 3",
    aggressiveness_score = "Aggressiveness Score (Percentile)"
  ) %>%
  cols_align(
    align = "center"
  ) %>%
  data_color(
    columns = c(aggressiveness_score),
    method = "numeric",
    palette = c("#0D0887", "#E16462", "#FCA636", "#F0F921"),
    domain = range(trio_stats_viz$aggressiveness_score, na.rm = TRUE),
    reverse = TRUE
  ) %>%
  opt_table_font(
    font = list(
      google_font(name = "Times New Roman"),
      "Serif"
    )
  )
gt_data2

#Visualization 9 - Volatile -----------------------------------------------------



# Build GT table
trio_stats_viz2 <- trio_stats %>%
  select(p1_agg_index, p2_agg_index, p3_agg_index, aggressiveness_score, volatility_score) %>%
  arrange(desc(volatility_score)) %>%
  head(10) %>%
  mutate(
    p1_agg_index = round(p1_agg_index, 3),
    p2_agg_index = round(p2_agg_index, 3),
    p3_agg_index = round(p3_agg_index, 3),
    volatility_score = round(volatility_score, 3),
    aggressiveness_score = round(aggressiveness_score, 3)
    
  )

# Identify max column per row
max_cols <- pmap_chr(
  trio_stats_viz2[, c("p1_agg_index", "p2_agg_index", "p3_agg_index")],
  ~ c("p1_agg_index", "p2_agg_index", "p3_agg_index")[which.max(c(...))]
)

# Create gt table (without max_col)
gt_data3 <- trio_stats_viz2 %>%
  gt() %>%
  tab_header(
    title = md("**Which Outfield Trios Are Most Volatile?**"),
    subtitle = md("*Mapping the standard deviation of each outfield trio's agressiveness*")
  ) %>%
  cols_label(
    p1_agg_index = "Player 1 Aggressiveness",
    p2_agg_index = "Player 2 Aggressiveness",
    p3_agg_index = "Player 3 Aggressiveness",
    aggressiveness_score = "Agg Score",
    volatility_score = "Difference Score"
  ) %>%
  cols_align(align = "center") %>%
  data_color(
    columns = c(volatility_score),
    method = "numeric",
    palette = c("#440154", "#482576", "#3E4A89", "#31688E", "#26828E", "#1FA187", "#35B779", "#6DCD59", "#B4DD2C", "#D9EF3F"),
    domain = range(trio_stats_viz2$volatility_score, na.rm = TRUE),
    reverse = TRUE
  ) %>%
  opt_table_font(
    font = list(google_font("Times New Roman"), "Serif")
  )

#Make sure the max aggressiveness for each trio is highlighted
for (i in seq_along(max_cols)) {
  gt_data3 <- gt_data3 %>%
    tab_style(
      style = cell_fill(color = "#FFF4B2"),
      locations = cells_body(
        columns = all_of(max_cols[i]),
        rows = i
      )
    )
}

gt_data3
