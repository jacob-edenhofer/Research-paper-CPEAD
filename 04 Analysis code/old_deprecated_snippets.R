# # Set up loop over variables
# # map(corp_var, ~{
# #   # Rename variable
# #   corp <- .x
# #   corp_var_name <- var_dict[corp]
# #   
# #   # Compute change in corp_var_name by country and half_decade
# #   data_plot <- oecd_merged[, .(reference_area, time_period, half_decade, corp_value = get(corp))]
# #   
# #   # Calculate the delta
# #   data_plot <- data_plot[, corp_delta := corp_value - shift(corp_value, type = "lag"), by = reference_area]
# #   
# #   # Calculate the half-decade average change
# #   data_plot <- data_plot[, corp_delta_hc := mean(corp_delta, na.rm = TRUE), by = .(reference_area, half_decade)]
# #   
# #   # Select unique values for plotting
# #   data_plot <- unique(data_plot[, .(reference_area, half_decade, corp_delta_hc)])
# #   
# #   # Remove any NA values
# #   data_plot <- data_plot[!is.na(corp_delta_hc)]
# #   
# #   # Order half-decades vector 
# #   data_plot$half_decade <- factor(data_plot$half_decade, 
# #                                   levels = c("1990-1994", "1995-1999", "2000-2004", "2005-2009", "2010-2014", 
# #                                              "2015-2019", "2020-2022"))
# #   
# #   # Plot
# #   p <- data_plot %>%
# #     ggplot(aes(y = half_decade, x = reorder(reference_area, corp_delta_hc), fill = corp_delta_hc)) +
# #     geom_tile() +
# #     geom_text(aes(label = round(corp_delta_hc, 2)), size = 2.5) +
# #     scale_fill_gradient2(low = "blue", mid = "gray95", high = "red", midpoint = 0, na.value = "white") +
# #     # scale_fill_viridis_b(option = "plasma", direction = -1, na.value = "grey80") +
# #     coord_flip() +
# #     labs(y = "Half-decade",
# #          title = str_wrap(paste0("Change in ", corp_var_name, ", by half decade")),
# #          x = ""
# #     ) +
# #     theme_bw() +
# #     theme(axis.text.y = element_text(size = 7), 
# #           strip.background = element_blank(),
# #           axis.text.x = element_text(size = 7),
# #           axis.title.y = element_text(size = 8),
# #           plot.title = element_text(hjust = 0.5, size = 14),
# #           legend.position = "bottom"
# #     ) +
# #     guides(fill = guide_colourbar(title = "Average annual change", barwidth = 15, 
# #                                   title.position = "top", title.hjust = 0.5))
# #   
# #   
# #   # Save plots
# #   save_plot(plot = p, path = paste0(output_dir, "/Figures/Descriptive plots/Corporatism/",
# #                                     str_replace_all(str_to_lower(corp_var_name), "_", ""), ".png"))
# #   
# # })
# 
# # Compute changes in corporatism variable 
# # For each half_decade, compute the change in the corp_var by country and plot it 
# # map(corp_var, ~{
# #   # Rename variable
# #   corp <- .x
# #   corp_var_name <- var_dict[corp]
# #   
# #   # Compute change in corp_var_name by country and half_decade
# #   data_plot <- oecd_merged[, .(reference_area, time_period, half_decade, corp_value = get(corp))]
# #   
# #   # Calculate the delta
# #   data_plot <- data_plot[, corp_delta := corp_value - shift(corp_value, type = "lag"), by = reference_area]
# #   
# #   # Calculate the half-decade average change
# #   data_plot <- data_plot[, corp_delta_hc := mean(corp_delta, na.rm = TRUE), by = .(reference_area, half_decade)]
# #   
# #   # Select unique values for plotting
# #   data_plot <- unique(data_plot[, .(reference_area, half_decade, corp_delta_hc)])
# #   
# #   # Remove any NA values
# #   data_plot <- data_plot[!is.na(corp_delta_hc)]
# #   
# #   # Initialise list of plots 
# #   plot_list <- list()
# #   
# #   # Order half-decades vector 
# #   data_plot$half_decade <- factor(data_plot$half_decade, 
# #                                   levels = c("1990-1994", "1995-1999", "2000-2004", "2005-2009", "2010-2014", 
# #                                              "2015-2019", "2020-2022"))
# #   
# #   # Loop through each half-deacade, save plots, and wrap_plots them together
# #   for(decade in unique(data_plot$half_decade)[!grepl("2020-2022", unique(data_plot$half_decade), fixed = T)]){
# #     # Subset data
# #     data_plot_decade <- data_plot[half_decade == decade]
# #     
# #     # Plot
# #     p <- data_plot_decade %>%
# #       ggplot(aes(y = corp_delta_hc, x = reorder(reference_area, corp_delta_hc))) +
# #       geom_col() +
# #       geom_hline(yintercept = 0, linetype = "dashed", colour = "red", linewidth = 0.7) +
# #       facet_wrap(~half_decade, scales = "free_y") +
# #       coord_flip() +
# #       labs(title = "",
# #            y = str_wrap(paste0("Change in ", corp_var_name), width = 40),
# #            x = ""
# #       ) +
# #       theme_bw() +
# #       theme(axis.text.y = element_text(size = 3), 
# #             strip.background = element_blank(),
# #             axis.text.x = element_text(size = 6),
# #             axis.title.y = element_text(size = 8),
# #             ) 
# #     
# #     # Save to list 
# #     plot_list[[decade]] <- p
# #   }
# #   
# #   # Wrap plots together
# #   comb_plot <- wrap_plots(plot_list, ncol = 2) +
# #     patchwork::plot_annotation(title = str_wrap(paste0("Change in ", corp_var_name, " by country and half-decade")), 
# #                                theme = theme(plot.title = element_text(hjust = 0.5, size = 14))
# #                                )
# #   
# #   # Save plots
# #   save_plot(plot = comb_plot, path = paste0(output_dir, "/Figures/Descriptive plots/Corporatism/", 
# #                                             str_replace_all(str_to_lower(corp_var_name), "_", ""), ".png"))
# #   
# # })
# 
# # Heatmap plots of changes in corporatism variables
# map(corp_var, ~{
#   # Rename variable
#   corp <- .x
#   corp_var_name <- var_dict[corp]
#   
#   # Compute change in corp_var_name by country and half_decade
#   data_plot <- oecd_merged[, .(reference_area, time_period, half_decade, corp_value = get(corp))]
#   
#   # Calculate the delta
#   data_plot <- data_plot[, corp_delta := corp_value - shift(corp_value, type = "lag"), by = reference_area]
#   
#   # Calculate the half-decade average change
#   data_plot <- data_plot[, corp_delta_hc := mean(corp_delta, na.rm = TRUE), by = .(reference_area, half_decade)]
#   
#   # Select unique values for plotting
#   data_plot <- unique(data_plot[, .(reference_area, half_decade, corp_delta_hc)])
#   
#   # Remove any NA values
#   data_plot <- data_plot[!is.na(corp_delta_hc)]
#   
#   # Order half-decades vector 
#   data_plot$half_decade <- factor(data_plot$half_decade, 
#                                   levels = c("1990-1994", "1995-1999", "2000-2004", "2005-2009", "2010-2014", 
#                                              "2015-2019", "2020-2022"))
#   
#   # Plot
#   p <- data_plot %>%
#     ggplot(aes(y = half_decade, x = reorder(reference_area, corp_delta_hc), fill = corp_delta_hc)) +
#     geom_tile() +
#     geom_text(aes(label = round(corp_delta_hc, 2)), size = 2.5) +
#     scale_fill_gradient2(low = "blue", mid = "gray95", high = "red", midpoint = 0, na.value = "white") +
#     # scale_fill_viridis_b(option = "plasma", direction = -1, na.value = "grey80") +
#     coord_flip() +
#     labs(y = "Half-decade",
#          title = str_wrap(paste0("Change in ", corp_var_name, ", by half decade")),
#          x = ""
#     ) +
#     theme_bw() +
#     theme(axis.text.y = element_text(size = 7), 
#           strip.background = element_blank(),
#           axis.text.x = element_text(size = 7),
#           axis.title.y = element_text(size = 8),
#           plot.title = element_text(hjust = 0.5, size = 14),
#           legend.position = "bottom"
#     ) +
#     guides(fill = guide_colourbar(title = "Average annual change", barwidth = 15, 
#                                   title.position = "top", title.hjust = 0.5))
#   
#   
#   # Save plots
#   save_plot(plot = p, path = paste0(output_dir, "/Figures/Descriptive plots/Corporatism/",
#                                     str_replace_all(str_to_lower(corp_var_name), "_", ""), ".png"))
#   
# })
# 
# # Do the same for dv_var
# map(dv_var, ~{
#   # Rename variable
#   dv <- .x
#   dv_var_name <- var_dict[dv]
#   
#   # Compute change in corp_var_name by country and half_decade
#   data_plot <- oecd_merged[, .(reference_area, time_period, climate_actions_and_policies, half_decade, dv_value = get(dv))]
#   
#   # Calculate the delta
#   data_plot <- data_plot[, dv_delta := dv_value - shift(dv_value, type = "lag"), by = .(reference_area, climate_actions_and_policies)]
#   
#   # Calculate the half-decade average change
#   data_plot <- data_plot[, dv_delta_hc := mean(dv_delta, na.rm = TRUE), by = .(reference_area, climate_actions_and_policies, half_decade)]
#   
#   # Select unique values for plotting
#   data_plot <- unique(data_plot[, .(reference_area, climate_actions_and_policies, half_decade, dv_delta_hc)])
#   
#   # Remove any NA values
#   data_plot <- data_plot[!is.na(dv_delta_hc)]
#   
#   # Order half-decades vector 
#   data_plot$half_decade <- factor(data_plot$half_decade, 
#                                   levels = c("1990-1994", "1995-1999", "2000-2004", "2005-2009", "2010-2014", 
#                                              "2015-2019", "2020-2022"))
#   
#   # Plot
#   p <- data_plot %>%
#     ggplot(aes(y = half_decade, x = reorder(reference_area, dv_delta_hc), fill = dv_delta_hc)) +
#     geom_tile() +
#     geom_text(aes(label = round(dv_delta_hc, 1)), size = 2.1) +
#     scale_fill_gradient2(low = "blue", mid = "gray95", high = "red", midpoint = 0, na.value = "white") +
#     # scale_fill_viridis_b(option = "plasma", direction = -1, na.value = "grey80") +
#     coord_flip() +
#     facet_wrap(~climate_actions_and_policies, 
#                labeller = labeller(climate_actions_and_policies = function(x) str_wrap(x, width = 20))
#     ) +
#     labs(y = "Half-decade",
#          title = str_wrap(paste0("Change in ", dv_var_name, ", by half decade")),
#          x = ""
#     ) +
#     theme_bw() +
#     theme(axis.text.y = element_text(size = 7), 
#           strip.background = element_blank(),
#           axis.text.x = element_text(size = 7,  angle = 90),
#           axis.title.y = element_text(size = 8),
#           plot.title = element_text(hjust = 0.5, size = 14),
#           legend.position = "bottom"
#     ) +
#     guides(fill = guide_colourbar(title = "Average annual change", barwidth = 15, 
#                                   title.position = "top", title.hjust = 0.5))
#   
#   
#   # Save plots
#   save_plot(plot = p, path = paste0(output_dir, "/Figures/Descriptive plots/CAPMF/",
#                                     str_replace_all(str_to_lower(dv_var_name), "_", ""), ".png"))
#   
# })
# 
# # # Do the same for dv_var 
# # map(dv_var, ~{
# #   # Rename variable
# #   dv <- .x
# #   dv_var_name <- var_dict[dv]
# #   
# #   # Compute change in corp_var_name by country and half_decade
# #   data_plot <- oecd_merged[, .(reference_area, time_period, climate_actions_and_policies, half_decade, dv_value = get(dv))]
# #   
# #   # Calculate the delta
# #   data_plot <- data_plot[, dv_delta := dv_value - shift(dv_value, type = "lag"), by = .(reference_area, climate_actions_and_policies)]
# #   
# #   # Calculate the half-decade average change
# #   data_plot <- data_plot[, dv_delta_hc := mean(dv_delta, na.rm = TRUE), by = .(reference_area, climate_actions_and_policies, half_decade)]
# #   
# #   # Select unique values for plotting
# #   data_plot <- unique(data_plot[, .(reference_area, half_decade, climate_actions_and_policies, dv_delta_hc)])
# #   
# #   # Remove any NA values
# #   data_plot <- data_plot[!is.na(dv_delta_hc)]
# #   
# #   # Initialise list of plots 
# #   plot_list <- list()
# #   
# #   # Order half-decades vector 
# #   data_plot$half_decade <- factor(data_plot$half_decade, 
# #                                   levels = c("1990-1994", "1995-1999", "2000-2004", "2005-2009", "2010-2014", 
# #                                              "2015-2019", "2020-2022"))
# #   
# #   # Loop through each half-deacade, save plots, and wrap_plot them together
# #   for(decade in unique(data_plot$half_decade)){
# #     
# #     # Subset data
# #     data_plot_decade <- data_plot[half_decade == decade]
# #     
# #     # Plot
# #     p <- data_plot_decade %>%
# #       ggplot(aes(y = dv_delta_hc, x = reorder(reference_area, dv_delta_hc))) +
# #       geom_col() +
# #       geom_hline(yintercept = 0, linetype = "dashed", colour = "red", linewidth = 0.7) +
# #       facet_wrap(~climate_actions_and_policies, 
# #                  labeller = labeller(climate_actions_and_policies = function(x) str_wrap(x, width = 20))
# #                  ) +
# #       coord_flip() +
# #       labs(title = "",
# #            y = paste0("Change in stringency, ", decade),
# #            x = ""
# #            ) +
# #       theme_bw() +
# #       theme(axis.text.y = element_text(size = 5), 
# #             strip.background = element_blank(),
# #             axis.text.x = element_text(size = 6),
# #             axis.title.y = element_text(size = 8),
# #             )
# #     
# #     # Save to list
# #     plot_list[[decade]] <- p
# #     
# #   }
# #   
# #   # Wrap plots together
# #   comb_plot <- wrap_plots(plot_list, ncol = 3) +
# #     patchwork::plot_annotation(title = str_wrap(paste0("Change in ", dv_var_name, " by country and half-decade")), 
# #                                theme = theme(plot.title = element_text(hjust = 0.5, size = 14))
# #                                )
# #   
# #   # Save plots
# #   # save_plot(plot = comb_plot, path = paste0(output_dir, "/Figures/Descriptive plots/", 
# #   #                                           str_replace_all(str_to_lower(dv_var_name), "_", ""), ".png"))
# #   
# #   })
