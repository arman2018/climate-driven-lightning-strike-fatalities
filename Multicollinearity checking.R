library(car)
library(ggplot2)

# Check multicollinearity using VIF
lm_model <- lm(deaths ~ max_temp + min_temp + mean_temp + humid + prep, data = data)
vif(lm_model)


# Create a data frame from VIF results
vif_values <- c(max_temp = 2.581024, min_temp = 1.146030, mean_temp = 4.085362, humid = 6.154606, prep = 4.437788)
vif_df <- data.frame(
  Variable = names(vif_values),
  VIF = as.numeric(vif_values)
)

# Create the bar plot
ggplot(vif_df, aes(x = reorder(Variable, VIF), y = VIF, fill = VIF > 5)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_hline(yintercept = 5, linetype = "dashed", color = "red", size = 1) +
  geom_text(aes(label = round(VIF, 2)), vjust = -0.5, size = 5) +
  coord_flip() +  # Horizontal bars
  scale_fill_manual(values = c("FALSE" = "#4C72B0", "TRUE" = "#C44E52")) +
  labs(
    title = "",
    subtitle = "",
    x = "Variable",
    y = "VIF",
    fill = "VIF > 5"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )
