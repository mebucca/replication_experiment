# Talent most important

depvar <- c("ahead_fwealth","ahead_peduc","ahead_ambition","ahead_hardwork")

label <- c("Get ahead: family wealth","Get ahead: educated parents","Get ahead: ambition","Get ahead: hard work")

pretty_map <- c(
  "ineq_oppmore unequal"                                    = "Opportunity inequality (unequal)",
  "ineq_outhigh"                                            = "Outcome inequality (high)",
  "as.numeric(age)"                                         = "Age",
  "factor(gender)Male"                                      = "Male",
  "factor(gender)Other gender"                              = "Other gender",
  "factor(race)Asian"                                       = "Asian",
  "factor(race)Black or African"                            = "Black or African",
  "factor(race)Hispanic or Latino/a/x"                      = "Hispanic or Latino/a/x",
  "factor(race)Middle Eastern or North African"             = "Middle Eastern or North African",
  "factor(race)Multiracial / Mixed race"                    = "Multiracial / Mixed race",
  "factor(race)White"                                       = "White",
  "years_of_schooling"                                      = "Years of schooling",
  "income_midpoint"                                         = "Household income (midpoint)",
  "polExtremely Conservative"                               = "Extremely Conservative",
  "polExtremely Liberal"                                    = "Extremely Liberal",
  "polLiberal"                                              = "Liberal",
  "polModerate"                                             = "Moderate",
  "polSlightly Conservative"                                = "Slightly Conservative",
  "polSlightly Liberal"                                     = "Slightly Liberal",
  "ineq_oppmore unequal:ineq_outhigh"                       = "Opportunity inequality (unequal) × Outcome inequality (high)"
)


for (dv in depvar) {
  
  # eval each depentend varible
  data_experiment_complete <- data_experiment_complete %>% mutate(outcome_var = !!sym(dv)) 
  
  # logistic regression model 
  mymodel <- glm(outcome_var ~ ineq_opp*ineq_out, data=data_experiment_complete, family="binomial")
  
  
  mymodel_ctrl <- update(mymodel, . ~  . + as.numeric(age) + factor(gender) + factor(race) + years_of_schooling + income_midpoint + pol)
  
  
  # new data for predictions
  newx <- data_experiment_complete %>% data_grid(ineq_opp,ineq_out,.model=mymodel)
  newx <- cbind(newx,predict(mymodel,newdata=newx, type="response", se.fit=T)) %>%
    mutate(lwr_95 = fit - 1.96 * se.fit, 
           upr_95 = fit + 1.96 * se.fit,
           lwr_90 = fit - 1.645 * se.fit, 
           upr_90 = fit + 1.645 * se.fit)
  
  
  # plot

  ylim_list <- list(
    ahead_fwealth   = c(0.37, 0.90),
    ahead_peduc     = c(0.37, 0.90),
    ahead_ambition  = c(0.37, 0.90),
    ahead_hardwork  = c(0.37, 0.90)
  )
  
  dodge_width <- width  # Adjust this value as needed
  
  myplot <- newx %>%
    ggplot(aes(x = ineq_opp, y = fit, colour = ineq_out, shape = ineq_out)) +
    geom_point(size = 2.7, position = position_dodge(width = dodge_width)) +
    geom_linerange(aes(ymin = lwr_90, ymax = upr_90), linewidth = 1, position = position_dodge(width = dodge_width), alpha=1) +
    geom_linerange(aes(ymin = lwr_95, ymax = upr_95), linewidth = 1, position = position_dodge(width = dodge_width), alpha=0.5) +
    scale_colour_manual(values = mypal_disc) +
    scale_shape_manual(values = c(16, 17)) +
    facet_grid(. ~ "") +
    guides(colour = guide_legend(title = "outcome inequality", title.position = "bottom"),
           shape  = guide_legend(title = "outcome inequality", title.position = "bottom")) +
    ylim(ylim_list[[dv]]) +
    theme(legend.position = "bottom",
          legend.title = element_text(size = rel(0.8)),
          axis.title.x = element_text(size = rel(0.8)),
          strip.text.x = element_blank(),
          strip.background.x = element_blank()) +
    labs(
      colour = NULL,
      x = "opportunity",
      y = label[match(dv, depvar)]
    )
  
  assign(paste0("plot_",dv,"_s2"),myplot)

  
  # save table and plot
  
  file_table <- paste0(dv, "_s2.tex")
  file_plot  <- paste0(dv, "_s2.jpg")
  
  
  cov_order <- unique(c(
    names(coef(mymodel_ctrl))[-1],
    names(coef(mymodel))[-1]
  ))
  cov_order <- cov_order[!grepl("^outcome_var", cov_order)]
  predictor_labels <- unname(ifelse(
    cov_order %in% names(pretty_map),
    pretty_map[cov_order],
    cov_order
  ))

  stargazer(mymodel, mymodel_ctrl,
            type = "latex",
            out = here("study_2/tables", file_table),
            dep.var.labels = "",
            omit = "outcome_var",
            covariate.labels = predictor_labels,
            font.size = "tiny",
            no.space = TRUE,
            table.placement = "H",
            header = FALSE)
  
  
  ggsave(here("study_2/figures",file_plot), myplot, height = fig_h, width = fig_w)
  
}


