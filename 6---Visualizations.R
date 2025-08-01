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
                                          margin = margin(t = 10, b = 10)),  
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
