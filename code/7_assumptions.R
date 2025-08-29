
# exchange effect

plot_exchange <- data_experiment_complete %>%
  filter(!is.na(pairId)) %>%
  select(ConnectId, contains("relpower"), game_winner, exchange) %>%
  pivot_longer(
    cols = contains("relpower"),
    names_to = "n_round",
    names_prefix = "relpowerhand_",
    values_to = "relpowerhand"
  ) %>%
  mutate(
    round = as.numeric(gsub("[^0-9]", "", n_round))  # Remove non-numeric chars
  ) %>%
  filter(!is.na(relpowerhand), !is.na(round)) %>%
  ggplot(aes(x = round, y = relpowerhand, group = ConnectId, colour = game_winner)) +
  geom_line(alpha = 0.1) +
  facet_grid(. ~ exchange) +
  scale_colour_manual(values = mypal_disc) +
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  labs(x = "Round", y = "Relative Hand Strength", colour = NULL) 

# Save plot
ggsave(here("figures", "plot_exchange.jpg"), plot_exchange, height = fig_h, width = fig_w)


# playing skills (as deviation from min-max strategy)

custom_labels <- c("1t","2t","3t","1g","2g","3g","4g","5g")

plot_dev_minmax <- data_experiment_complete %>%
  select(ConnectId, contains("diff_"), game_winner,exchange) %>%
  pivot_longer(cols = contains("diff_"), names_to = "stage", names_prefix = "diff_r_", values_to = "diff_minmax") %>%
  ggplot(aes(x = as.numeric(str_extract(stage, "\\d+")), y = diff_minmax, group = game_winner, colour = game_winner, fill = game_winner)) +
  facet_grid( . ~ exchange) +
  geom_smooth(method = "loess") +
  geom_vline(xintercept = 3, linetype = "dashed") +
  scale_x_continuous(breaks = 1:length(custom_labels), labels = custom_labels) + # Set custom x-axis labels
  scale_colour_manual(values = mypal_disc) +
  scale_fill_manual(values = mypal_disc) +
  labs(x = "round", y = "Deviation from min-max", colour=NULL, fill=NULL) 

ggsave(here("figures","plot_dev_minmax.jpg"), plot_dev_minmax, height = fig_h, width = fig_w)


# outcome in training as proxy for skills (prob winning round 1 game)


mymodel <- glm(as.factor(winner_game_round_1) ~ 
                 exchange*training_winner*relpowerhand_total, 
               data=data_experiment_complete, family="binomial")

# new data for predictions
newx <- data_experiment_complete %>% data_grid(exchange,training_winner,relpowerhand_total,.model=mymodel)
newx <- cbind(newx,predict(mymodel,newdata=newx, type="response", se.fit=T)) %>%
  mutate(lwr_95 = fit - 1.96 * se.fit, 
         upr_95 = fit + 1.96 * se.fit,
         lwr_90 = fit - 1.645 * se.fit, 
         upr_90 = fit + 1.645 * se.fit)


plot_wintrain <- newx %>%
  ggplot(aes(x = relpowerhand_total, y = fit, ymin=lwr_95, ymax=upr_95,
             group = training_winner, colour = training_winner, fill = training_winner)) +
  facet_grid( . ~ exchange) +
  geom_ribbon(aes(colour=NULL),alpha=0.5) +
  geom_line(linewidth = 1) +
  scale_colour_manual(values = mypal_disc) +
  scale_fill_manual(values = mypal_disc) +
  labs(x = "Hand strength", y = "P(Win game)", colour=NULL, fill=NULL) 
ggsave(here("figures","plot_wintrain.jpg"), plot_wintrain, height = fig_h, width = fig_w)



# winning by skills and quality of cards


mymodel <- glm(as.factor(game_winner) ~
                 exchange*average_difference_game*relpowerhand_total,
               data=data_experiment_complete, family="binomial")

# new data for predictions

newx <- data_experiment_complete %>%
data_grid(exchange, average_difference_game = seq(0,2,length.out=1000), relpowerhand_total = seq(0.2,0.8,length.out=1000), .model=mymodel)

newx <- cbind(newx,predict(mymodel,newdata=newx, type="response", se.fit=T)) %>%
mutate(lwr_95 = fit - 1.96 * se.fit,
upr_95 = fit + 1.96 * se.fit,
lwr_90 = fit - 1.645 * se.fit,
upr_90 = fit + 1.645 * se.fit)


plot_winhandskills <- newx %>%
as_tibble() %>%
ggplot(aes(x = relpowerhand_total, y = average_difference_game, fill = fit)) +
facet_grid(. ~ exchange) +
geom_raster(interpolate = TRUE) +  # for heatmap display
geom_path(data = filter(newx,  fit > 0.499 & fit < 0.501), aes(group = exchange), color = "black") + # Adding line for fit = 0.5
scale_fill_gradient(low = mypal_disc[1], high = mypal_disc[2]) +  # Continuous color scale
labs(x = "Hand strength", y = "Deviation from minmax", fill = "P(win game)") +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  )

ggsave(here("figures","plot_winhandskills.jpg"), plot_winhandskills, height = fig_h, width = fig_w)

