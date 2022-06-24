plot_ensemble <- function(truth, tibble_pred, incorrect = FALSE,  tibble_prob = NULL, order = NULL, ...){


  if(tibble_pred %>%
     select(!where(is.factor)) %>%
     ncol() != 0)stop("tibble_pred some columns not factors")

  if(!is.null(tibble_prob)){
    if(!identical(sort(names(tibble_prob)),  sort(names(tibble_pred))))stop("tibble_prob does not match tibble_pred")
    if(max(tibble_prob) > 1)stop("tibble_prob greater than 1")
    if(min(tibble_prob) < 1/length(levels(truth)))stop("tibble_prob less than 1/no levels")
  }

  tib <- tibble_pred %>%
    mutate(truth = truth,
           id = 1 : nrow(tibble_pred))

  if(is.null(order)){accuracy_vector <- tib %>%
    summarise(across(-id,
                     ~ accuracy_fun(truth, .x)))
  }else {
    if(!is.data.frame(order))stop("order is not a data frame")
    accuracy_vector <- order %>% mutate(truth = 1)
    if(!identical(names(tib %>% select(-id)), names(accuracy_vector)))stop("order variable names not same as tibble_pred")
  }


  cols_order <- rank(-accuracy_vector,
                     ties.method = "first")

  tib <- tib[,c(names(sort(cols_order)), "id")]

  ord <- round(sort(as.numeric(accuracy_vector), decreasing = TRUE), 3)

  if (!is.null(tibble_prob)) {
    tib_prob <- tibble_prob %>%
      mutate(truth = truth, id = 1 : nrow(tib))

    tib_prob <- tib_prob[, c(names(sort(cols_order)), "id")]

    # add prefix before joining:
    names(tib) <- paste0("x", names(tib))
    names(tib_prob) <- paste0("y", names(tib_prob))

    tib <- tib  %>%
      rename(id = xid)

    tib_prob <- tib_prob  %>%
      rename(id = yid)

    if (incorrect == TRUE) {
      dstinct <- !duplicated(tib %>%
                               select(-id))
      tib <- tib[dstinct, ]
      tib_prob <- tib_prob[dstinct, ]
    }

    tib <- tib %>%
      inner_join(tib_prob, by = "id") %>%
      select(-id) %>%
      arrange(across(everything())) %>%
      mutate(id = 1:nrow(tib))


    tib <- tib %>%
      mutate(ytruth = rep(1, nrow(tib))) %>%
      pivot_longer(-id,
                   names_to = c(".value", "name"),
                   names_pattern = "(.)(.*)") %>%
      mutate(
        name = as.factor(name),
        name = fct_relevel(name , names(sort(cols_order))),
        name = as.numeric(name)
      ) %>%
      rename(class = x, prob = y)


    p1 <- tib %>%
      ggplot(aes(x = name, y = id)) +
      geom_tile(aes(fill = class, alpha = prob))

  }

  else{
    if (incorrect == TRUE) {
      dstinct <- !duplicated(tib %>% select(-id))
      tib <- tib[dstinct, ]
    }
    tib <- tib %>%
      select(-id) %>%
      arrange(across(everything())) %>% #
      mutate(id = 1:nrow(tib)) %>%
      pivot_longer(-id) %>%
      mutate(
        name = as.factor(name),
        name = fct_relevel(name , names(sort(cols_order))),
        name = as.numeric(name)
      )


    p1 <- tib %>%  ggplot(aes(x = name, y = id)) +
      geom_tile(aes(fill = value))

  }
  p1 +
    theme_void() +
    scale_x_continuous(
      breaks = unique(tib$name),
      labels = names(sort(cols_order)),
      sec.axis = sec_axis( ~ ., breaks = unique(tib$name), labels = ord)
    ) +
    theme(
      axis.title.x = element_blank(),
      plot.margin = margin(0.7, 0, 1.5, 0, "cm"),
      # axis.title.y=element_blank(),
      # axis.ticks.x=element_blank(),
      # axis.ticks.y=element_blank(),
      axis.text.x = element_text(angle = 30, hjust = 0)
    ) +
    xlab("") +
    ylab("") +
    scale_fill_brewer(palette = "Set2")
}