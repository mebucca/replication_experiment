# Talent most important

depvar <- c("min_payment_UG","max_payment_UG","ultimatum_game_proposer_receiver","dictator_game_receiver")

label <- c("Min acceptable payment UG","Max acceptable payment UG","Proposed amount to send UG", "Amount sent DG")


predictor_labels <- c(
  "Opportunity inequality (RE)",             # exchangeRE
  "High outcome inequality ($5 gap)",        # inequality5
  "Payment",                                 # payment
  "Ultimatum game first",                    # ug_first
  "Bot opponent",                            # bot_opponent
  "Skill gap (training)",                    # average_difference_training
  "Skill gap (game)",                        # average_difference_game
  "Age",                                     # as.numeric(age)
  "Male",                                    # factor(gender)Male
  "Other gender",                            # factor(gender)Other
  "Black",                                   # factor(race)black
  "Latino/Hispanic",                         # factor(race)latino-hispanic
  "Other race",                              # factor(race)other
  "White",                                   # factor(race)white
  "Years of schooling",                      # years_of_schooling
  "Household income",                        # income_midpoint
  "Political ideology",                      # political_ideology
  "Opportunity inequality (continuous)",     # ineq_opp
  "Opportunity (RE) × Outcome inequality"    # exchangeRE:inequality5
)



for (dv in depvar) {
  
  # eval each depentend varible
  data_experiment_complete <- data_experiment_complete %>% mutate(outcome_var = !!sym(dv)) 
  
  # logistic regression model 
  mymodel <- lm(outcome_var ~ 
                   exchange*inequality + 
                   payment + ug_first,
                 data=data_experiment_complete)
  
  
  mymodel_ctrl <- update(mymodel, . ~  . + bot_opponent + average_difference_training + average_difference_game +
                           as.numeric(age) + factor(gender) + factor(race) + years_of_schooling + income_midpoint + political_ideology)
  
  
  mymodel_ctrl_opp <- update(mymodel_ctrl, . ~  . + ineq_opp)
  
  
  # new data for predictions
  newx <- data_experiment_complete %>% data_grid(exchange,inequality,.model=mymodel)
  newx <- cbind(newx,predict(mymodel,newdata=newx, se.fit=T)) %>%
    mutate(lwr_95 = fit - 1.96 * se.fit, 
           upr_95 = fit + 1.96 * se.fit,
           lwr_90 = fit - 1.645 * se.fit, 
           upr_90 = fit + 1.645 * se.fit)
  
  
  #plot
  
  dodge_width <- width # Adjust this value as needed
  myplot <- newx %>%
    ggplot(aes(x = exchange, y = fit, colour = inequality)) +
    geom_point(size = 2.7, position = position_dodge(width = dodge_width)) +
    geom_linerange(aes(ymin = lwr_90, ymax = upr_90), size = 1, position = position_dodge(width = dodge_width), alpha=1) + 
    geom_linerange(aes(ymin = lwr_95, ymax = upr_95), size = 1, position = position_dodge(width = dodge_width), alpha=0.5) +
    scale_colour_manual(values = mypal_disc) +
    theme(legend.position = "bottom") + ylim(0,100) +
    labs(colour = "inequality of opportunity", x = "inequality of outcomes", 
         y = label[match(dv, depvar)],
         colour = NULL) 
  
  # save table and plot
  
  file_table     <- paste0(dv, ".tex")
  file_plot      <- paste0(dv, ".jpg")
  
  
  stargazer(mymodel, mymodel_ctrl, mymodel_ctrl_opp,
            type = "latex",
            out = here("tables", file_table),
            covariate.labels = predictor_labels,
            float.env = "table",  # ensures \begin{table}
            font.size = "tiny",   # adds \tiny
            float = TRUE,
            table.placement = "H",  # adds [H]
            header = FALSE)    
  
  
  ggsave(here("figures",file_plot), myplot, height = fig_h, width = fig_h)

  
}
