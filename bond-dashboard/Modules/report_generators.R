#' @export
#' @title Enhanced Chart Collection with Error Handling and Cleanup
#' @description Generates report charts with comprehensive error handling and temp file management
#' @param processed_data The processed bond data
#' @param filtered_data The filtered bond data (without technical indicators)
#' @param filtered_data_with_technicals The filtered bond data WITH technical indicators (RSI, MACD, Bollinger Bands, etc.)
#' @param var_data Value-at-Risk data
#' @param regime_data Market regime data
#' @param carry_roll_data Carry and roll return data
#' @param treasury_holdings_ts Treasury holdings time series data (optional)
#' @param treasury_bond_holdings Treasury bond-level holdings data (optional)
#' @param input_params List of input parameters from the Shiny app
collect_report_charts <- function(processed_data, filtered_data, filtered_data_with_technicals,
                                  var_data, regime_data, carry_roll_data,
                                  treasury_holdings_ts = NULL, treasury_bond_holdings = NULL,
                                  input_params) {

    # ══════════════════════════════════════════════════════════════════════
    # INITIALIZE
    # ══════════════════════════════════════════════════════════════════════

    charts <- list()
    failed_charts <- list()
    temp_files <- character()  # Track all temp files for cleanup

    selected_sections <- input_params$report_sections

    if(is.null(selected_sections) || length(selected_sections) == 0) {
        return(list(
            charts = charts,
            failed = failed_charts,
            cleanup = function() {}  # No-op cleanup
        ))
    }

    # Initialize temporary directory for chart caching
    temp_chart_dir <- file.path(tempdir(), paste0("charts_", Sys.getpid()))
    if(!dir.exists(temp_chart_dir)) {
        dir.create(temp_chart_dir, recursive = TRUE)
    }


    # ══════════════════════════════════════════════════════════════════════
    # DEFINE CHART GENERATORS
    # ══════════════════════════════════════════════════════════════════════

    chart_generators <- list(
        overview = list(
            regime_plot = function() {
                if(!is.null(regime_data) && nrow(regime_data) > 0) {
                    generate_regime_analysis_plot(regime_data, list())
                } else { NULL }
            }
        ),
        relative = list(
            yield_curve = function() {
                if(!is.null(processed_data) && nrow(processed_data) > 0) {
                    generate_enhanced_yield_curve(
                        processed_data,
                        list(
                            xaxis_choice = input_params$xaxis_choice %||% "modified_duration",
                            curve_model = input_params$curve_model %||% "loess"
                        )
                    )
                } else { NULL }
            },
            relative_heatmap = function() {
                if(!is.null(filtered_data) && nrow(filtered_data) > 1) {
                    generate_relative_value_heatmap(filtered_data, list())
                } else { NULL }
            },
            zscore_plot = function() {
                if(!is.null(processed_data) && nrow(processed_data) > 0 &&
                   "z_score" %in% names(processed_data)) {
                    generate_enhanced_zscore_plot(processed_data, list())
                } else { NULL }
            },
            # ✨ NEW PLOT
            convexity = function() {
                if(!is.null(processed_data) && nrow(processed_data) > 0 &&
                   "convexity" %in% names(processed_data)) {
                    generate_enhanced_convexity_plot(processed_data, list())
                } else { NULL }
            }
        ),
        risk = list(
            var_distribution = function() {
                if(!is.null(filtered_data) && nrow(filtered_data) > 0) {
                    generate_var_distribution_plot(filtered_data, list())
                } else { NULL }
            },
            var_ladder = function() {
                if(!is.null(var_data) && nrow(var_data) > 0) {
                    generate_var_ladder_plot(var_data, list())
                } else { NULL }
            },
            dv01_ladder = function() {
                if(!is.null(processed_data) && nrow(processed_data) > 0) {
                    generate_dv01_ladder_plot(processed_data, list())
                } else { NULL }
            }
        ),
        technical = list(
            technical_plot = function() {
                # Select benchmark bond (~10y duration) instead of first/shortest bond
                bond_select <- input_params$tech_bond_select %||%
                    (if(!is.null(filtered_data_with_technicals) && nrow(filtered_data_with_technicals) > 0) {
                        tryCatch({
                            filtered_data_with_technicals %>%
                                filter(date == max(date, na.rm = TRUE)) %>%
                                mutate(dist_to_10y = abs(modified_duration - 10)) %>%
                                arrange(dist_to_10y) %>%
                                pull(bond) %>%
                                first()
                        }, error = function(e) unique(filtered_data_with_technicals$bond)[1])
                    } else { NULL })

                if(!is.null(bond_select) && !is.null(filtered_data_with_technicals) &&
                   nrow(filtered_data_with_technicals) > 0) {
                    indicator_type <- input_params$tech_indicator_type %||% "all"

                    # ✅ NOW USING CORRECT DATA WITH TECHNICAL INDICATORS
                    generate_advanced_technical_plot(
                        filtered_data_with_technicals,
                        bond_select,
                        indicator_type
                    )
                } else { NULL }
            },
            signal_matrix = function() {
                # ✅ NOW USING CORRECT DATA WITH TECHNICAL INDICATORS
                if(!is.null(filtered_data_with_technicals) && nrow(filtered_data_with_technicals) > 1) {
                    generate_signal_matrix_heatmap(filtered_data_with_technicals)
                } else { NULL }
            }
        ),
        carry = list(
            carry_heatmap = function() {
                if(!is.null(carry_roll_data) && nrow(carry_roll_data) > 0) {
                    return_type_value <- input_params$return_type %||% "net"
                    generate_enhanced_carry_roll_heatmap(carry_roll_data, return_type_value)
                } else { NULL }
            },
            scenario_analysis = function() {
                if(!is.null(processed_data) && nrow(processed_data) > 0) {
                    generate_scenario_analysis_plot(processed_data)
                } else { NULL }
            },
            # ✨ Butterfly Spread Analyzer
            butterfly_spread = function() {
                if(!is.null(filtered_data) && nrow(filtered_data) > 0) {
                    # Calculate butterfly spreads
                    butterflies <- calculate_butterfly_spreads(filtered_data, lookback_days = 365)
                    if(!is.null(butterflies) && length(butterflies) > 0) {
                        # Get the top butterfly by absolute Z-Score
                        top_bf_name <- names(butterflies)[which.max(sapply(butterflies, function(x) abs(x$z_score)))]
                        generate_butterfly_chart(butterflies[[top_bf_name]], zscore_threshold = 2.0)
                    } else { NULL }
                } else { NULL }
            },
            # ✨ NEW PLOT
            forward_curve = function() {
                if(!is.null(processed_data) && nrow(processed_data) > 0) {
                    generate_forward_curve_plot(processed_data, list())
                } else { NULL }
            }
        ),
        auction = list(
            auction_performance = function() {
                if(!is.null(filtered_data) && nrow(filtered_data) > 0) {
                    generate_enhanced_auction_analytics(filtered_data, list())
                } else { NULL }
            },
            auction_patterns = function() {
                if(!is.null(filtered_data)) {
                    auction_data <- filtered_data %>%
                        filter(!is.na(bid_to_cover))

                    if(nrow(auction_data) > 5) {
                        # Get selected bonds for highlighting
                        selected_bonds <- input_params$auction_bonds_select %||% character(0)
                        generate_auction_pattern_analysis(filtered_data, list(),
                                                          selected_bonds = selected_bonds)
                    } else { NULL }
                } else { NULL }
            },
            # ✨ NEW PLOT
            auction_forecast = function() {
                auction_bonds <- input_params$auction_bonds_select %||%
                    (if(!is.null(filtered_data) && nrow(filtered_data) > 0) {
                        unique(filtered_data$bond)[1:min(3, length(unique(filtered_data$bond)))]
                    } else { NULL })

                if(!is.null(auction_bonds) && !is.null(filtered_data) && nrow(filtered_data) > 0) {
                    generate_auction_forecast_plot(filtered_data, auction_bonds)
                } else { NULL }
            },
            # ✨ NEW PLOT
            demand_elasticity = function() {
                if(!is.null(filtered_data) && nrow(filtered_data) > 0) {
                    generate_demand_elasticity_plot(filtered_data, list())
                } else { NULL }
            },
            # ✨ NEW PLOT
            success_probability = function() {
                auction_bonds <- input_params$auction_bonds_select %||%
                    (if(!is.null(filtered_data) && nrow(filtered_data) > 0) {
                        unique(filtered_data$bond)[1:min(3, length(unique(filtered_data$bond)))]
                    } else { NULL })

                if(!is.null(auction_bonds) && !is.null(filtered_data) && nrow(filtered_data) > 0) {
                    generate_success_probability_plot(filtered_data, auction_bonds)
                } else { NULL }
            },
            # ✨ NEW PLOT
            bid_distribution = function() {
                if(!is.null(filtered_data) && nrow(filtered_data) > 0) {
                    # Get selected bonds for highlighting
                    selected_bonds <- input_params$auction_bonds_select %||% character(0)
                    generate_bid_distribution_plot(filtered_data, list(),
                                                   selected_bonds = selected_bonds)
                } else { NULL }
            },
            # ✨ NEW PLOT
            ytd_issuance = function() {
                if(!is.null(filtered_data) && nrow(filtered_data) > 0) {
                    generate_ytd_bond_issuance_chart(filtered_data, list())
                } else { NULL }
            },
            # ✨ NEW PLOT
            auction_sentiment = function() {
                if(!is.null(filtered_data) && nrow(filtered_data) > 0) {
                    generate_auction_sentiment_gauge(filtered_data, list())
                } else { NULL }
            },
            # ✨ NEW PLOT
            auction_success_factors = function() {
                if(!is.null(filtered_data) && nrow(filtered_data) > 0) {
                    generate_auction_success_factors_plot(filtered_data, list())
                } else { NULL }
            }
        ),
        intelligence = list(
            correlation = function() {
                if(!is.null(filtered_data) && length(unique(filtered_data$bond)) > 2) {
                    generate_enhanced_correlation_plot(filtered_data, list())
                } else { NULL }
            },
            term_structure = function() {
                if(!is.null(filtered_data) && nrow(filtered_data) > 10) {
                    generate_term_structure_3d_plot(filtered_data, list())
                } else { NULL }
            }
        ),

        # ═══════════════════════════════════════════════════════════════════════════
        # TREASURY HOLDINGS SECTION
        # ═══════════════════════════════════════════════════════════════════════════
        treasury = list(
            holdings_area = function() {
                if(!is.null(treasury_holdings_ts) && nrow(treasury_holdings_ts) > 0) {
                    generate_holdings_area_chart(
                        holdings_long = treasury_holdings_ts,
                        date_range = NULL,  # Use full range for reports
                        sectors_selected = NULL  # Include all sectors
                    )
                } else { NULL }
            },
            sector_trend = function() {
                if(!is.null(treasury_holdings_ts) && nrow(treasury_holdings_ts) > 0) {
                    # Use the largest sector for default report
                    top_sector <- tryCatch({
                        treasury_holdings_ts %>%
                            filter(date == max(date)) %>%
                            arrange(desc(percentage)) %>%
                            slice(1) %>%
                            pull(sector)
                    }, error = function(e) "Non-residents")
                    generate_sector_trend_chart(
                        holdings_long = treasury_holdings_ts,
                        sector_name = top_sector,
                        date_range = NULL
                    )
                } else { NULL }
            },
            holdings_fixed = function() {
                if(!is.null(treasury_bond_holdings) && nrow(treasury_bond_holdings) > 0) {
                    generate_bond_holdings_bar_chart(
                        bond_pct_long = treasury_bond_holdings,
                        selected_bond_type = "Fixed Rate",
                        target_date = NULL,  # Uses most recent
                        show_labels = TRUE
                    )
                } else { NULL }
            },
            holdings_ilb = function() {
                if(!is.null(treasury_bond_holdings) && nrow(treasury_bond_holdings) > 0) {
                    generate_bond_holdings_bar_chart(
                        bond_pct_long = treasury_bond_holdings,
                        selected_bond_type = "ILB",
                        target_date = NULL,
                        show_labels = TRUE
                    )
                } else { NULL }
            },
            holdings_frn = function() {
                if(!is.null(treasury_bond_holdings) && nrow(treasury_bond_holdings) > 0) {
                    generate_bond_holdings_bar_chart(
                        bond_pct_long = treasury_bond_holdings,
                        selected_bond_type = "FRN",
                        target_date = NULL,
                        show_labels = TRUE
                    )
                } else { NULL }
            },
            holdings_sukuk = function() {
                if(!is.null(treasury_bond_holdings) && nrow(treasury_bond_holdings) > 0) {
                    generate_bond_holdings_bar_chart(
                        bond_pct_long = treasury_bond_holdings,
                        selected_bond_type = "Sukuk",
                        target_date = NULL,
                        show_labels = TRUE
                    )
                } else { NULL }
            },
            ownership_changes = function() {
                if(!is.null(treasury_holdings_ts) && nrow(treasury_holdings_ts) > 0) {
                    generate_ownership_change_chart(
                        holdings_long = treasury_holdings_ts,
                        periods = c(1, 3, 12)
                    )
                } else { NULL }
            },
            holdings_diverging_fixed = function() {
                if(!is.null(treasury_bond_holdings) && nrow(treasury_bond_holdings) > 0) {
                    generate_holdings_change_diverging(
                        bond_pct_long = treasury_bond_holdings,
                        period_months = 3,
                        selected_bond_type = "Fixed Rate",
                        top_n = 12
                    )
                } else { NULL }
            },
            holdings_diverging_ilb = function() {
                if(!is.null(treasury_bond_holdings) && nrow(treasury_bond_holdings) > 0) {
                    generate_holdings_change_diverging(
                        bond_pct_long = treasury_bond_holdings,
                        period_months = 3,
                        selected_bond_type = "ILB",
                        top_n = 12
                    )
                } else { NULL }
            }
        ),

        # Recommendations section (text-only, no charts)
        recommendations = list()
    )

    # ══════════════════════════════════════════════════════════════════════
    # GENERATE CHARTS WITH ENHANCED ERROR HANDLING
    # ══════════════════════════════════════════════════════════════════════

    # Extract selected_plots from input_params for filtering
    selected_plots <- input_params$selected_plots

    for(section in selected_sections) {
        if(!section %in% names(chart_generators)) {
            warning(paste("Unknown report section:", section))
            next
        }

        section_generators <- chart_generators[[section]]

        for(chart_name in names(section_generators)) {
            # ✅ NEW: Check if this specific plot is selected
            if(!is.null(selected_plots) &&
               !is.null(selected_plots[[chart_name]]) &&
               !isTRUE(selected_plots[[chart_name]])) {
                message(sprintf("⊘ Skipping chart: %s (not selected)", chart_name))
                next
            }

            chart_file <- file.path(temp_chart_dir, paste0(chart_name, ".rds"))
            temp_files <- c(temp_files, chart_file)  # Track for cleanup

            tryCatch({
                # Generate the chart
                chart <- section_generators[[chart_name]]()

                if(!is.null(chart)) {
                    # Save successfully generated chart
                    saveRDS(chart, chart_file)
                    charts[[chart_name]] <- chart_file

                    # Log success
                    message(sprintf("✓ Generated chart: %s (%s)",
                                    chart_name, section))
                } else {
                    # Chart returned NULL (insufficient data, etc.)
                    failed_charts[[chart_name]] <- "Chart generation returned NULL (likely insufficient data)"
                    warning(sprintf("Chart returned NULL: %s", chart_name))
                }

            }, error = function(e) {
                # Log error with full context
                error_msg <- sprintf("Error in %s/%s: %s",
                                     section, chart_name, e$message)
                failed_charts[[chart_name]] <- error_msg

                warning(error_msg)
                log_error(e, context = paste("chart_generation", section, chart_name))

                # Clean up any partial file
                if(file.exists(chart_file)) {
                    tryCatch(
                        file.remove(chart_file),
                        error = function(e2) {
                            warning(paste("Failed to remove partial file:", chart_file))
                        }
                    )
                }
            })
        }
    }

    # ══════════════════════════════════════════════════════════════════════
    # REPORT SUMMARY
    # ══════════════════════════════════════════════════════════════════════

    message("\n╔══════════════════════════════════════════════════════╗")
    message("║         CHART GENERATION SUMMARY                     ║")
    message("╚══════════════════════════════════════════════════════╝")
    message(sprintf("  ✓ Successful: %d charts", length(charts)))
    message(sprintf("  ✗ Failed:     %d charts", length(failed_charts)))

    if(length(failed_charts) > 0) {
        message("\n⚠ Failed charts:")
        for(name in names(failed_charts)) {
            message(sprintf("  • %s", name))
            message(sprintf("    %s", failed_charts[[name]]))
        }
    }
    message("")

    # ══════════════════════════════════════════════════════════════════════
    # CREATE CLEANUP FUNCTION (kept for backward compatibility)
    # ══════════════════════════════════════════════════════════════════════

    # Keep the manual cleanup function for now
    cleanup_function <- function() {
        message("\n🧹 Cleaning up temporary chart files...")

        files_removed <- 0
        for(f in temp_files) {
            if(file.exists(f)) {
                tryCatch({
                    file.remove(f)
                    files_removed <- files_removed + 1
                }, error = function(e) {})
            }
        }

        if(dir.exists(temp_chart_dir)) {
            tryCatch({
                unlink(temp_chart_dir, recursive = TRUE)
                if(files_removed > 0) {
                    message(sprintf("✓ Cleaned up %d temporary files", files_removed))
                }
            }, error = function(e) {})
        }

        invisible(NULL)
    }

    # ══════════════════════════════════════════════════════════════════════
    # RETURN ENHANCED RESULT
    # ══════════════════════════════════════════════════════════════════════

    return(list(
        charts = charts,              # Successfully generated charts
        failed = failed_charts,       # Failed chart details
        cleanup = cleanup_function,   # Cleanup function (now redundant but kept for compatibility)
        temp_dir = temp_chart_dir,    # Directory location (for debugging)
        summary = list(
            total_requested = sum(sapply(chart_generators[selected_sections], length)),
            successful = length(charts),
            failed = length(failed_charts),
            success_rate = round(length(charts) /
                                     sum(sapply(chart_generators[selected_sections], length)) * 100, 1)
        )
    ))
}

#' @export
# This should NOT be a reactive - it should be a regular function
generate_report_summaries <- function(processed_data, filtered_data, var_data, regime_data, carry_roll_data,
                                       treasury_holdings_ts = NULL, treasury_bond_holdings = NULL) {
    summaries <- list()

    tryCatch({
        # Executive Summary with comprehensive null checks
        bond_count <- if(!is.null(filtered_data) && nrow(filtered_data) > 0) {
            length(unique(filtered_data$bond))
        } else { 0 }

        avg_yield <- if(!is.null(processed_data) && nrow(processed_data) > 0) {
            mean(processed_data$yield_to_maturity, na.rm = TRUE)
        } else { NA }

        avg_duration <- if(!is.null(processed_data) && nrow(processed_data) > 0) {
            mean(processed_data$modified_duration, na.rm = TRUE)
        } else { NA }

        opportunities_count <- if(!is.null(processed_data) && "z_score" %in% names(processed_data)) {
            sum(abs(processed_data$z_score) > 1.5, na.rm = TRUE)
        } else { 0 }

        avg_spread <- if(!is.null(processed_data) && "spread_to_curve" %in% names(processed_data)) {
            mean(abs(processed_data$spread_to_curve), na.rm = TRUE)
        } else { 0 }

        avg_btc <- if(!is.null(filtered_data) && "bid_to_cover" %in% names(filtered_data)) {
            mean(filtered_data$bid_to_cover, na.rm = TRUE)
        } else { NA }

        date_range <- if(!is.null(filtered_data) && nrow(filtered_data) > 0) {
            list(
                start = format(min(filtered_data$date, na.rm = TRUE), "%B %d, %Y"),
                end = format(max(filtered_data$date, na.rm = TRUE), "%B %d, %Y")
            )
        } else {
            list(
                start = format(Sys.Date() - 30, "%B %d, %Y"),
                end = format(Sys.Date(), "%B %d, %Y")
            )
        }

        summaries$executive <- sprintf(
            "This report analyzes %d South African Government bonds over the period from %s to %s. %s %s %s",
            bond_count,
            date_range$start,
            date_range$end,
            if(!is.na(avg_yield)) sprintf("The portfolio shows an average yield of %.2f%%", avg_yield) else "",
            if(!is.na(avg_duration)) sprintf("with an average modified duration of %.2f years.", avg_duration) else "",
            if(opportunities_count > 0) sprintf("Currently, %d bonds present relative value opportunities.", opportunities_count) else ""
        )

        # ══════════════════════════════════════════════════════════════════════
        # STRUCTURED EXECUTIVE METRICS FOR DASHBOARD-STYLE PDF PAGE
        # ══════════════════════════════════════════════════════════════════════
        exec_metrics <- list()
        tryCatch({
            # Yield range (min-max at latest date)
            if (!is.null(processed_data) && nrow(processed_data) > 0) {
                latest <- processed_data %>% filter(date == max(date, na.rm = TRUE))
                if (nrow(latest) > 0) {
                    yield_min <- min(latest$yield_to_maturity, na.rm = TRUE)
                    yield_max <- max(latest$yield_to_maturity, na.rm = TRUE)
                    min_bond <- latest$bond[which.min(latest$yield_to_maturity)]
                    max_bond <- latest$bond[which.max(latest$yield_to_maturity)]
                    exec_metrics$yield_range <- list(
                        value = sprintf("%.2f%% - %.2f%%", yield_min, yield_max),
                        subtitle = sprintf("%s to %s", min_bond, max_bond),
                        label = "Yield Range"
                    )

                    # Steepest spread — clarify direction
                    exec_metrics$steepest_spread <- list(
                        value = sprintf("%.0f bps", (yield_max - yield_min) * 100),
                        subtitle = sprintf("%s over %s", max_bond, min_bond),
                        label = "Steepest Spread"
                    )
                }
            }

            # Average modified duration
            exec_metrics$avg_duration <- list(
                value = if(!is.na(avg_duration)) sprintf("%.2f years", avg_duration) else "N/A",
                subtitle = sprintf("%d bonds in universe", bond_count),
                label = "Avg Modified Duration"
            )

            # Current regime
            if (!is.null(regime_data) && nrow(regime_data) > 0) {
                current_reg <- regime_data %>%
                    filter(date == max(date, na.rm = TRUE)) %>%
                    slice(1)
                regime_name <- if ("regime" %in% names(current_reg) && nrow(current_reg) > 0) {
                    tools::toTitleCase(tolower(as.character(current_reg$regime)))
                } else { "N/A" }
                vol_text <- if ("vol_20d" %in% names(current_reg) && nrow(current_reg) > 0) {
                    sprintf("20d vol: %.2f%%", current_reg$vol_20d * 100)
                } else { "" }
                exec_metrics$regime <- list(
                    value = regime_name,
                    subtitle = vol_text,
                    label = "Market Regime"
                )
            } else {
                exec_metrics$regime <- list(value = "N/A", subtitle = "", label = "Market Regime")
            }

            # Top carry trade
            if (!is.null(carry_roll_data) && nrow(carry_roll_data) > 0) {
                ret_col <- if ("total_return" %in% names(carry_roll_data)) "total_return" else if ("net_return" %in% names(carry_roll_data)) "net_return" else NULL
                if (!is.null(ret_col)) {
                    best_carry <- carry_roll_data %>%
                        arrange(desc(.data[[ret_col]])) %>%
                        slice(1)
                    if (nrow(best_carry) > 0 && "bond" %in% names(best_carry)) {
                        exec_metrics$top_carry <- list(
                            value = as.character(best_carry$bond[1]),
                            subtitle = sprintf("%.2f%% return", best_carry[[ret_col]][1]),
                            label = "Top Carry Trade"
                        )
                    }
                }
            }
            if (is.null(exec_metrics$top_carry)) {
                exec_metrics$top_carry <- list(value = "N/A", subtitle = "", label = "Top Carry Trade")
            }

            # Strongest buy/sell signals (from z-scores)
            if (!is.null(processed_data) && "z_score" %in% names(processed_data)) {
                sig_data <- processed_data %>%
                    filter(!is.na(z_score)) %>%
                    arrange(desc(abs(z_score))) %>%
                    head(2)
                if (nrow(sig_data) > 0) {
                    sig_text <- paste(sapply(1:nrow(sig_data), function(i) {
                        direction <- if (sig_data$z_score[i] > 0) "Cheap" else "Rich"
                        sprintf("%s (%s)", sig_data$bond[i], direction)
                    }), collapse = ", ")
                    exec_metrics$top_signals <- list(
                        value = sig_text,
                        subtitle = sprintf("Z: %+.2f, %+.2f", sig_data$z_score[1],
                                           if(nrow(sig_data) > 1) sig_data$z_score[2] else 0),
                        label = "Most Notable Deviations"
                    )
                }
            }
            if (is.null(exec_metrics$top_signals)) {
                exec_metrics$top_signals <- list(value = "N/A", subtitle = "", label = "Most Notable Deviations")
            }

            # VaR summary
            if (!is.null(var_data) && nrow(var_data) > 0 && "VaR_95_bps" %in% names(var_data)) {
                avg_var <- mean(var_data$VaR_95_bps, na.rm = TRUE)
                exec_metrics$var_summary <- list(
                    value = sprintf("%.0f bps", avg_var),
                    subtitle = "Portfolio avg 95% VaR",
                    label = "Value-at-Risk"
                )
            } else {
                exec_metrics$var_summary <- list(value = "N/A", subtitle = "", label = "Value-at-Risk")
            }

            # RV Opportunities — clarify zero as "no extreme mispricings"
            exec_metrics$rv_opportunities <- list(
                value = if(opportunities_count > 0) as.character(opportunities_count) else "No Extreme Mispricings",
                subtitle = if(opportunities_count > 0) {
                    sprintf("%d bonds with |Z| > 1.5", opportunities_count)
                } else {
                    paste0("All bonds within ", "\u00B1", "2\u03C3 of fair value")
                },
                label = "Relative Value"
            )

        }, error = function(e) {
            message(sprintf("[Exec Metrics] Error computing metrics: %s", e$message))
        })
        summaries$exec_metrics <- exec_metrics

        # Market Conditions with safe regime data handling
        if(!is.null(regime_data) && nrow(regime_data) > 0) {
            tryCatch({
                current_regime <- regime_data %>%
                    filter(date == max(date, na.rm = TRUE)) %>%
                    slice(1)

                if(nrow(current_regime) > 0) {
                    regime_name <- if("regime" %in% names(current_regime)) {
                        tolower(current_regime$regime)
                    } else { "normal" }

                    vol_level <- if("vol_20d" %in% names(current_regime)) {
                        current_regime$vol_20d * 100
                    } else { NA }

                    stress_level <- if("stress_score" %in% names(current_regime)) {
                        current_regime$stress_score
                    } else { NA }

                    summaries$market_conditions <- sprintf(
                        "The market is currently operating in a %s regime%s%s. %s",
                        regime_name,
                        if(!is.na(vol_level)) sprintf(" with 20-day volatility at %.2f%%", vol_level) else "",
                        if(!is.na(stress_level)) sprintf(" and a stress score of %.2f", stress_level) else "",
                        case_when(
                            regime_name == "stressed" ~ "Caution is advised with reduced position sizes.",
                            regime_name == "elevated" ~ "Close monitoring is recommended.",
                            TRUE ~ "Normal trading conditions prevail."
                        )
                    )
                } else {
                    summaries$market_conditions <- "Market conditions are within normal parameters."
                }
            }, error = function(e) {
                summaries$market_conditions <- "Market analysis in progress."
            })
        } else {
            summaries$market_conditions <- "Market regime data is being calculated."
        }

        # Chart-specific summaries
        summaries$relative_summary <- tryCatch({
            generate_chart_summary("yield_curve", processed_data)
        }, error = function(e) {
            "Yield curve analysis in progress."
        })

        summaries$risk_summary <- tryCatch({
            if(!is.null(var_data) && nrow(var_data) > 0) {
                generate_chart_summary("var_analysis", var_data)
            } else {
                "Risk metrics being calculated."
            }
        }, error = function(e) {
            "Risk analysis in progress."
        })

        summaries$auction_summary <- tryCatch({
            generate_chart_summary("auction", filtered_data)
        }, error = function(e) {
            "Auction analysis in progress."
        })

        # Technical Analysis summary
        summaries$technical_summary <- tryCatch({
            if (!is.null(filtered_data) && nrow(filtered_data) > 0) {
                n_bonds <- length(unique(filtered_data$bond))
                sprintf("Technical analysis across %d bonds using RSI, MACD, Bollinger Bands, and momentum indicators.", n_bonds)
            } else {
                "Technical indicators being calculated."
            }
        }, error = function(e) "Technical analysis in progress.")

        # Carry & Roll summary
        summaries$carry_summary <- tryCatch({
            if (!is.null(carry_roll_data) && nrow(carry_roll_data) > 0) {
                best_carry <- carry_roll_data %>%
                    arrange(desc(if ("total_return" %in% names(carry_roll_data)) total_return else net_return)) %>%
                    slice(1)
                if (nrow(best_carry) > 0 && "bond" %in% names(best_carry)) {
                    sprintf("Carry and roll analysis across %d bonds. %s shows the highest expected total return.",
                            length(unique(carry_roll_data$bond)),
                            best_carry$bond[1])
                } else {
                    "Carry analysis available for current bond universe."
                }
            } else {
                "Carry and roll metrics being calculated."
            }
        }, error = function(e) "Carry analysis in progress.")

        # Intelligence summary
        summaries$intelligence_summary <- tryCatch({
            if (!is.null(filtered_data) && length(unique(filtered_data$bond)) > 2) {
                sprintf("Cross-bond correlation analysis covers %d instruments with term structure evolution.",
                        length(unique(filtered_data$bond)))
            } else {
                "Market intelligence metrics require multiple bonds."
            }
        }, error = function(e) "Intelligence analysis in progress.")

        # Treasury Holdings summary
        summaries$treasury_summary <- tryCatch({
            if (!is.null(treasury_holdings_ts) && nrow(treasury_holdings_ts) > 0) {
                sprintf("Government bond ownership analysis across %d reporting periods.",
                        length(unique(treasury_holdings_ts$date)))
            } else {
                "Government bond ownership analysis across institutional sectors."
            }
        }, error = function(e) "Treasury holdings analysis in progress.")

        # Overview summary (alias for market_conditions)
        summaries$overview_summary <- summaries$market_conditions

        # Recommendations
        summaries$recommendations <- tryCatch({
            if(!is.null(processed_data) && "z_score" %in% names(processed_data)) {
                top_opportunities <- processed_data %>%
                    filter(abs(z_score) > 1.5) %>%
                    arrange(desc(abs(z_score))) %>%
                    head(3)

                if(nrow(top_opportunities) > 0) {
                    opp_text <- paste(sapply(1:nrow(top_opportunities), function(i) {
                        row <- top_opportunities[i,]
                        sprintf("%s (z-score: %.2f)",
                                row$bond,
                                row$z_score)
                    }), collapse = "; ")

                    paste(
                        "Based on current relative value analysis, consider the following opportunities:",
                        opp_text,
                        "Monitor upcoming auctions closely for entry points."
                    )
                } else {
                    "Current market pricing appears fair. Focus on carry strategies and maintain diversified exposure."
                }
            } else {
                "Trading recommendations will be available once analysis is complete."
            }
        }, error = function(e) {
            "Recommendations being generated."
        })

        summaries$risk_considerations <- "Key risks include potential interest rate volatility, changes in fiscal policy, and global risk sentiment. Maintain appropriate position sizing relative to risk tolerance."

    }, error = function(e) {
        log_error(e, context = "summary_generation")
        summaries$executive <- "Report generation in progress. Please ensure all required data is available."
    })

    return(summaries)
}


#' @export
#' @title Generate Structured Trading Recommendations
#' @description Produces structured trading recommendations from report data
#' @return List of recommendation sections for PDF rendering
generate_trading_recommendations <- function(processed_data, filtered_data, var_data,
                                              carry_roll_data, regime_data) {
    recs <- list()

    tryCatch({
        # 1. TOP CONVICTION IDEAS (from z-scores)
        conviction_ideas <- character()
        if (!is.null(processed_data) && "z_score" %in% names(processed_data)) {
            latest <- processed_data %>%
                filter(date == max(date, na.rm = TRUE), !is.na(z_score)) %>%
                arrange(desc(abs(z_score)))

            top_signals <- head(latest, 3)
            if (nrow(top_signals) > 0) {
                conviction_ideas <- sapply(1:nrow(top_signals), function(i) {
                    row <- top_signals[i, ]
                    direction <- if (row$z_score > 0) "Cheap (BUY)" else "Rich (SELL)"
                    spread_text <- if ("spread_to_curve" %in% names(row) && !is.na(row$spread_to_curve)) {
                        sprintf(", %.0f bps from fair value", row$spread_to_curve)
                    } else { "" }
                    sprintf("%s: %s signal (z-score: %+.2f%s)",
                            row$bond, direction, row$z_score, spread_text)
                })
            }
        }
        recs$conviction <- list(
            title = "Top Conviction Ideas",
            items = if (length(conviction_ideas) > 0) conviction_ideas else "No strong conviction signals at present."
        )

        # 2. RELATIVE VALUE TRADES (pairs from z-scores)
        rv_trades <- character()
        if (!is.null(processed_data) && "z_score" %in% names(processed_data)) {
            latest <- processed_data %>%
                filter(date == max(date, na.rm = TRUE), !is.na(z_score), !is.na(modified_duration))
            rich_bonds <- latest %>% filter(z_score < -1) %>% arrange(z_score)
            cheap_bonds <- latest %>% filter(z_score > 1) %>% arrange(desc(z_score))
            if (nrow(rich_bonds) > 0 && nrow(cheap_bonds) > 0) {
                n_pairs <- min(2, nrow(rich_bonds), nrow(cheap_bonds))
                for (i in 1:n_pairs) {
                    rv_trades <- c(rv_trades, sprintf(
                        "Buy %s (z: %+.2f, %.1fy) vs Sell %s (z: %+.2f, %.1fy)",
                        cheap_bonds$bond[i], cheap_bonds$z_score[i], cheap_bonds$modified_duration[i],
                        rich_bonds$bond[i], rich_bonds$z_score[i], rich_bonds$modified_duration[i]
                    ))
                }
            }
        }
        recs$rv_trades <- list(
            title = "Relative Value Trades",
            items = if (length(rv_trades) > 0) rv_trades else "No clear pair trade opportunities identified."
        )

        # 3. CARRY OPTIMIZATION (best per duration bucket)
        carry_items <- character()
        if (!is.null(carry_roll_data) && nrow(carry_roll_data) > 0) {
            ret_col <- if ("total_return" %in% names(carry_roll_data)) "total_return" else if ("net_return" %in% names(carry_roll_data)) "net_return" else NULL
            if (!is.null(ret_col) && "modified_duration" %in% names(carry_roll_data)) {
                carry_buckets <- carry_roll_data %>%
                    mutate(bucket = case_when(
                        modified_duration < 5 ~ "Short (0-5y)",
                        modified_duration < 10 ~ "Medium (5-10y)",
                        TRUE ~ "Long (10y+)"
                    )) %>%
                    group_by(bucket) %>%
                    arrange(desc(.data[[ret_col]])) %>%
                    slice(1) %>%
                    ungroup()

                if (nrow(carry_buckets) > 0) {
                    carry_items <- sapply(1:nrow(carry_buckets), function(i) {
                        row <- carry_buckets[i, ]
                        sprintf("%s: %s (%.2f%% return)", row$bucket, row$bond, row[[ret_col]])
                    })
                }
            }
        }
        recs$carry <- list(
            title = "Carry Optimization",
            items = if (length(carry_items) > 0) carry_items else "Carry data not available."
        )

        # 4. RISK WARNINGS
        risk_warnings <- character()
        if (!is.null(var_data) && nrow(var_data) > 0 && "VaR_95_bps" %in% names(var_data)) {
            high_var <- var_data %>%
                arrange(desc(VaR_95_bps)) %>%
                head(2)
            if (nrow(high_var) > 0) {
                risk_warnings <- c(risk_warnings,
                    sprintf("Highest VaR: %s (%.0f bps), %s (%.0f bps)",
                            high_var$bond[1], high_var$VaR_95_bps[1],
                            if(nrow(high_var) > 1) high_var$bond[2] else "N/A",
                            if(nrow(high_var) > 1) high_var$VaR_95_bps[2] else 0))
            }
        }
        if (!is.null(regime_data) && nrow(regime_data) > 0) {
            current_reg <- regime_data %>% filter(date == max(date, na.rm = TRUE)) %>% slice(1)
            if (nrow(current_reg) > 0 && "regime" %in% names(current_reg)) {
                reg_name <- tolower(as.character(current_reg$regime))
                if (reg_name %in% c("stressed", "elevated")) {
                    risk_warnings <- c(risk_warnings,
                        sprintf("Market regime is %s - consider reduced position sizes.", reg_name))
                }
            }
        }
        if (!is.null(processed_data) && "z_score" %in% names(processed_data)) {
            extreme_z <- processed_data %>%
                filter(date == max(date, na.rm = TRUE), abs(z_score) > 2.5)
            if (nrow(extreme_z) > 0) {
                risk_warnings <- c(risk_warnings,
                    sprintf("Extreme z-scores: %s - monitor for mean reversion or structural shift.",
                            paste(extreme_z$bond, collapse = ", ")))
            }
        }
        recs$risk_warnings <- list(
            title = "Risk Warnings",
            items = if (length(risk_warnings) > 0) risk_warnings else "No elevated risk warnings at present."
        )

        # 5. KEY LEVELS TO WATCH
        key_levels <- character()
        if (!is.null(processed_data) && nrow(processed_data) > 0) {
            # Pick benchmark bond (~10y duration)
            bench <- processed_data %>%
                filter(date == max(date, na.rm = TRUE)) %>%
                mutate(dist_to_10y = abs(modified_duration - 10)) %>%
                arrange(dist_to_10y) %>%
                slice(1)
            if (nrow(bench) > 0) {
                bench_hist <- processed_data %>% filter(bond == bench$bond)
                if (nrow(bench_hist) > 10) {
                    high_yield <- max(bench_hist$yield_to_maturity, na.rm = TRUE)
                    low_yield <- min(bench_hist$yield_to_maturity, na.rm = TRUE)
                    key_levels <- c(key_levels,
                        sprintf("Benchmark %s: Current %.2f%% | Range: %.2f%% - %.2f%% (%.0f bps)",
                                bench$bond, bench$yield_to_maturity,
                                low_yield, high_yield, (high_yield - low_yield) * 100))
                }
            }
        }
        recs$key_levels <- list(
            title = "Key Levels to Watch",
            items = if (length(key_levels) > 0) key_levels else "Insufficient data for key level identification."
        )

    }, error = function(e) {
        message(sprintf("[Trading Recs] Error: %s", e$message))
    })

    return(recs)
}


#' @export
# Replace or add this improved generate_chart_summary function
generate_chart_summary <- function(plot_type, data) {
    # Return default message if data is problematic
    if(is.null(data) ||
       (is.data.frame(data) && nrow(data) == 0) ||
       (is.list(data) && length(data) == 0)) {
        return("Data analysis in progress.")
    }

    summary_text <- tryCatch({

        if(plot_type == "yield_curve") {
            # Safe yield curve summary
            curve_level <- if("yield_to_maturity" %in% names(data)) {
                mean(data$yield_to_maturity, na.rm = TRUE)
            } else { NA }

            if(!is.na(curve_level)) {
                # Calculate slope safely
                curve_slope <- 0
                if("modified_duration" %in% names(data) &&
                   sum(!is.na(data$modified_duration)) > 1) {

                    dur_vals <- data$modified_duration[!is.na(data$modified_duration)]
                    if(length(dur_vals) > 2) {
                        dur_quantiles <- quantile(dur_vals, c(0.25, 0.75))
                        high_dur_yields <- data$yield_to_maturity[
                            !is.na(data$modified_duration) &
                                data$modified_duration > dur_quantiles[2]
                        ]
                        low_dur_yields <- data$yield_to_maturity[
                            !is.na(data$modified_duration) &
                                data$modified_duration < dur_quantiles[1]
                        ]

                        if(length(high_dur_yields) > 0 && length(low_dur_yields) > 0) {
                            curve_slope <- mean(high_dur_yields, na.rm = TRUE) -
                                mean(low_dur_yields, na.rm = TRUE)
                        }
                    }
                }

                spread_mean <- if("spread_to_curve" %in% names(data)) {
                    mean(abs(data$spread_to_curve), na.rm = TRUE)
                } else { 0 }

                sprintf(
                    "The yield curve shows an average level of %.2f%% with a slope of %.0f bps. %s",
                    curve_level,
                    curve_slope * 100,
                    if(!is.na(spread_mean) && spread_mean > 0) {
                        sprintf("Average spread to curve: %.1f bps.", spread_mean)
                    } else { "" }
                )
            } else {
                "Yield curve analysis in progress."
            }

        } else if(plot_type == "var_analysis") {
            # Safe VaR summary
            if("VaR_95_bps" %in% names(data)) {
                var_vals <- data$VaR_95_bps[!is.na(data$VaR_95_bps)]
                if(length(var_vals) > 0) {
                    avg_var <- mean(abs(var_vals))
                    max_var <- max(abs(var_vals))

                    sprintf(
                        "Portfolio 95%% VaR averages %.0f bps with maximum risk of %.0f bps. %s",
                        avg_var, max_var,
                        if(avg_var > 50) {
                            "Risk levels are elevated."
                        } else {
                            "Risk levels are within normal ranges."
                        }
                    )
                } else {
                    "VaR calculation in progress."
                }
            } else {
                "Risk metrics being calculated."
            }

        } else if(plot_type == "auction") {
            # Safe auction summary
            if("bid_to_cover" %in% names(data)) {
                btc_vals <- data$bid_to_cover[!is.na(data$bid_to_cover)]
                if(length(btc_vals) > 0) {
                    recent_btc <- tail(btc_vals, 10)
                    if(length(recent_btc) > 0) {
                        avg_btc <- mean(recent_btc)
                        sprintf(
                            "Average bid-to-cover ratio stands at %.2fx. %s",
                            avg_btc,
                            if(avg_btc > 2.5) {
                                "Auction demand remains healthy."
                            } else {
                                "Auction metrics suggest cautious market sentiment."
                            }
                        )
                    } else {
                        "Analyzing auction patterns."
                    }
                } else {
                    "No recent auction data available."
                }
            } else {
                "Auction analysis pending."
            }

        } else if(plot_type == "market") {
            # Safe market regime summary
            if("regime" %in% names(data)) {
                if(nrow(data) > 0) {
                    current_regime <- data[nrow(data),]
                    regime_text <- if("regime" %in% names(current_regime)) {
                        as.character(current_regime$regime)
                    } else { "Normal" }

                    vol_text <- if("vol_20d" %in% names(current_regime)) {
                        sprintf("%.1f%%", current_regime$vol_20d * 100)
                    } else { "moderate" }

                    sprintf(
                        "Market currently in %s regime with volatility at %s.",
                        regime_text, vol_text
                    )
                } else {
                    "Market regime analysis in progress."
                }
            } else {
                "Calculating market conditions."
            }

        } else {
            "Analysis in progress."
        }

    }, error = function(e) {
        log_error(e, context = paste("chart_summary", plot_type))
        "Summary generation in progress."
    })

    return(summary_text)
}


# create_report_template() removed — unused R Markdown template generator
# PDF reports are generated directly with pdf() + grid graphics in enhanced_bond_server.R

#' @export
# Enhanced email template function
create_email_template <- function(charts_base64, summaries, auction_data, message = "") {
    # Build auction table HTML
    auction_html <- ""
    if(!is.null(auction_data) && !is.null(auction_data$upcoming_bonds) &&
       nrow(auction_data$upcoming_bonds) > 0) {

        bonds_html <- paste(apply(auction_data$upcoming_bonds, 1, function(row) {
            sprintf('
                <tr>
                    <td style="padding: 10px; border-bottom: 1px solid #ddd;">%s</td>
                    <td style="padding: 10px; border-bottom: 1px solid #ddd;">%s</td>
                    <td style="padding: 10px; border-bottom: 1px solid #ddd;">%.2fx</td>
                    <td style="padding: 10px; border-bottom: 1px solid #ddd;">%.2fx</td>
                    <td style="padding: 10px; border-bottom: 1px solid #ddd;">%.2f%%</td>
                    <td style="padding: 10px; border-bottom: 1px solid #ddd;">%.1f</td>
                </tr>',
                    row["bond"],
                    format(as.Date(row["expected_next_auction"]), "%b %d"),
                    as.numeric(row["predicted_btc"]),
                    as.numeric(row["last_btc"]),
                    as.numeric(row["yield"]),
                    as.numeric(row["modified_duration"])
            )
        }), collapse = "")

        auction_html <- sprintf('
            <h2 style="color: #1B3A6B; margin-top: 30px;">Bonds on Auction This Week</h2>
            <table style="width: 100%%; border-collapse: collapse; margin: 20px 0;">
                <thead>
                    <tr style="background: #1B3A6B; color: white;">
                        <th style="padding: 12px; text-align: left;">Bond</th>
                        <th style="padding: 12px; text-align: left;">Expected Date</th>
                        <th style="padding: 12px; text-align: left;">Predicted B/C</th>
                        <th style="padding: 12px; text-align: left;">Last B/C</th>
                        <th style="padding: 12px; text-align: left;">Yield</th>
                        <th style="padding: 12px; text-align: left;">Duration</th>
                    </tr>
                </thead>
                <tbody>%s</tbody>
            </table>', bonds_html)
    } else {
        auction_html <- '<p style="color: #666; margin: 20px 0;">No auctions expected in the next 7 days based on historical patterns.</p>'
    }

    # Build chart sections
    chart_sections <- ""

    if(!is.null(charts_base64$yield_curve)) {
        chart_sections <- paste0(chart_sections, sprintf('
            <div class="chart-container" style="margin: 30px 0; text-align: center; background: white; padding: 15px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);">
                <div class="chart-title" style="font-weight: bold; color: #1B3A6B; margin-bottom: 10px;">Yield Curve Analysis</div>
                <img src="data:image/png;base64,%s" style="max-width: 100%%; height: auto;">
                <p style="font-size: 14px; color: #666; margin-top: 10px;">%s</p>
            </div>',
                                                         charts_base64$yield_curve,
                                                         if(!is.null(summaries$yield_curve)) summaries$yield_curve else ""
        ))
    }

    if(!is.null(charts_base64$var_ladder)) {
        chart_sections <- paste0(chart_sections, sprintf('
            <h2 style="color: #1B3A6B; margin-top: 30px;">Risk Analysis</h2>
            <div class="chart-container" style="margin: 20px 0; text-align: center; background: white; padding: 15px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);">
                <div class="chart-title" style="font-weight: bold; color: #1B3A6B; margin-bottom: 10px;">Value at Risk Ladder</div>
                <img src="data:image/png;base64,%s" style="max-width: 100%%; height: auto;">
                <p style="font-size: 14px; color: #666; margin-top: 10px;">%s</p>
            </div>',
                                                         charts_base64$var_ladder,
                                                         if(!is.null(summaries$risk)) summaries$risk else ""
        ))
    }

    if(!is.null(charts_base64$carry_heatmap)) {
        chart_sections <- paste0(chart_sections, sprintf('
            <h2 style="color: #1B3A6B; margin-top: 30px;">Carry & Roll Analysis</h2>
            <div class="chart-container" style="margin: 20px 0; text-align: center; background: white; padding: 15px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);">
                <div class="chart-title" style="font-weight: bold; color: #1B3A6B; margin-bottom: 10px;">Expected Returns Matrix</div>
                <img src="data:image/png;base64,%s" style="max-width: 100%%; height: auto;">
                <p style="font-size: 14px; color: #666; margin-top: 10px;">%s</p>
            </div>',
                                                         charts_base64$carry_heatmap,
                                                         if(!is.null(summaries$carry)) summaries$carry else ""
        ))
    }

    # Complete HTML template
    template <- sprintf('
<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <style>
        body {
            font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
            line-height: 1.6;
            color: #333;
            max-width: 800px;
            margin: 0 auto;
            padding: 20px;
            background: #f5f5f5;
        }
        .header {
            background: linear-gradient(135deg, #1B3A6B 0%%, #2A4D8C 100%%);
            color: white;
            padding: 30px;
            border-radius: 10px 10px 0 0;
            text-align: center;
        }
        .logo {
            font-size: 24px;
            font-weight: bold;
            margin-bottom: 10px;
        }
        .content {
            background: white;
            padding: 30px;
            border: 1px solid #e0e0e0;
            border-top: none;
        }
        .footer {
            background: #343A40;
            color: white;
            padding: 20px;
            text-align: center;
            border-radius: 0 0 10px 10px;
            font-size: 12px;
        }
    </style>
</head>
<body>
    <div class="header">
        <div class="logo">INSELE CAPITAL PARTNERS</div>
        <p>SA Government Bond Analysis Report</p>
        <p style="font-size: 14px;">%s</p>
    </div>
    <div class="content">
        %s
        <h2 style="color: #1B3A6B;">Market Overview</h2>
        <div style="background: #e8f4f8; padding: 15px; border-left: 4px solid #1B3A6B; margin: 15px 0;">
            %s
        </div>
        %s
        %s
        %s
    </div>
    <div class="footer">
        <p>© %s Insele Capital Partners - Broking Services</p>
        <p>This email and any attachments are confidential and intended solely for the addressee.</p>
    </div>
</body>
</html>',
                        format(Sys.Date(), "%B %d, %Y"),
                        if(message != "") paste0('<div style="background: #f8f9fa; padding: 15px; border-radius: 5px; margin-bottom: 20px;">', message, '</div>') else "",
                        if(!is.null(summaries$overview)) summaries$overview else "Market analysis in progress.",
                        chart_sections,
                        auction_html,
                        '<div style="background: #fff3cd; padding: 15px; border: 1px solid #ffc107; border-radius: 5px; margin: 20px 0;">
            <strong>Important Notice:</strong> This report contains proprietary analysis and should not be forwarded without authorization.
        </div>',
                        format(Sys.Date(), "%Y")
    )

    return(template)
}

#' @export
#' @title Full HTML Report Template with All Charts
#' @description Generates a complete HTML report with all selected charts organized by section
create_html_report_template <- function(charts, summaries, sections, config, auction_data = NULL) {
    # charts = named list of ggplot/grob objects
    # summaries = output of generate_report_summaries()
    # sections = character vector of selected sections
    # config = list with report_title, client_name, report_date

    # Section display names
    section_names <- c(
        overview = "Market Overview",
        relative = "Relative Value Analysis",
        risk = "Risk Analytics",
        technical = "Technical Analysis",
        carry = "Carry &amp; Roll Analysis",
        auction = "Auction Intelligence",
        intelligence = "Market Intelligence",
        treasury = "Treasury Holdings",
        recommendations = "Trading Recommendations"
    )

    # Map chart names to sections
    chart_sections_map <- list(
        overview = c("regime_plot"),
        relative = c("yield_curve", "relative_heatmap", "zscore_plot", "convexity"),
        risk = c("var_distribution", "var_ladder", "dv01_ladder"),
        technical = c("technical_plot", "signal_matrix"),
        carry = c("carry_heatmap", "scenario_analysis", "butterfly_spread", "forward_curve"),
        auction = c("auction_performance", "auction_patterns", "auction_forecast",
                     "demand_elasticity", "success_probability", "bid_distribution",
                     "ytd_issuance", "auction_sentiment", "auction_success_factors"),
        intelligence = c("correlation", "term_structure"),
        treasury = c("holdings_area", "sector_trend", "holdings_fixed", "holdings_ilb",
                      "holdings_frn", "holdings_sukuk", "ownership_changes",
                      "holdings_diverging_fixed", "holdings_diverging_ilb")
    )

    # Summary key mapping per section
    summary_keys <- c(
        overview = "overview_summary",
        relative = "relative_summary",
        risk = "risk_summary",
        technical = "technical_summary",
        carry = "carry_summary",
        auction = "auction_summary",
        intelligence = "intelligence_summary",
        treasury = "treasury_summary"
    )

    # Build HTML content section by section
    body_html <- ""

    for (section in sections) {
        section_title <- section_names[[section]]

        # Add section header
        body_html <- paste0(body_html, sprintf(
            '<h2 style="color: #1B3A6B; margin-top: 30px; border-bottom: 2px solid #1B3A6B; padding-bottom: 8px;">%s</h2>',
            section_title
        ))

        # Add section summary text if available
        summary_key <- summary_keys[[section]]
        if (!is.null(summary_key) && !is.null(summaries[[summary_key]]) && nchar(summaries[[summary_key]]) > 0) {
            body_html <- paste0(body_html, sprintf(
                '<div style="background: #e8f4f8; padding: 15px; border-left: 4px solid #1B3A6B; margin: 15px 0;">%s</div>',
                summaries[[summary_key]]
            ))
        }

        # Add recommendations text (no charts)
        if (section == "recommendations") {
            if (!is.null(summaries$recommendations)) {
                body_html <- paste0(body_html, sprintf(
                    '<div style="padding: 15px;"><h3 style="color: #1B3A6B;">Trading Opportunities</h3><p>%s</p></div>',
                    summaries$recommendations
                ))
            }
            if (!is.null(summaries$risk_considerations)) {
                body_html <- paste0(body_html, sprintf(
                    '<div style="padding: 15px;"><h3 style="color: #1B3A6B;">Risk Considerations</h3><p>%s</p></div>',
                    summaries$risk_considerations
                ))
            }
            next
        }

        # Add charts for this section
        section_charts <- chart_sections_map[[section]]
        if (!is.null(section_charts)) {
            for (chart_name in section_charts) {
                if (chart_name %in% names(charts) && !is.null(charts[[chart_name]])) {
                    chart_b64 <- tryCatch(
                        plot_to_base64(charts[[chart_name]], width = 10, height = 6, dpi = 150),
                        error = function(e) NULL
                    )
                    if (!is.null(chart_b64)) {
                        display_name <- gsub("_", " ", tools::toTitleCase(chart_name))
                        body_html <- paste0(body_html, sprintf(
                            '<div style="margin: 20px 0; text-align: center; background: white; padding: 15px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);">
                                <div style="font-weight: bold; color: #1B3A6B; margin-bottom: 10px;">%s</div>
                                <img src="data:image/png;base64,%s" style="max-width: 100%%; height: auto;">
                            </div>',
                            display_name, chart_b64
                        ))
                    }
                }
            }
        }
    }

    # Add auction table if auction section is selected and data exists
    auction_html <- ""
    if ("auction" %in% sections && !is.null(auction_data) && !is.null(auction_data$upcoming_bonds) &&
        nrow(auction_data$upcoming_bonds) > 0) {
        bonds_html <- paste(apply(auction_data$upcoming_bonds, 1, function(row) {
            sprintf('<tr>
                <td style="padding: 10px; border-bottom: 1px solid #ddd;">%s</td>
                <td style="padding: 10px; border-bottom: 1px solid #ddd;">%s</td>
                <td style="padding: 10px; border-bottom: 1px solid #ddd;">%.2fx</td>
                <td style="padding: 10px; border-bottom: 1px solid #ddd;">%.2f%%%%</td>
            </tr>',
                    row["bond"],
                    tryCatch(format(as.Date(row["expected_next_auction"]), "%%b %%d"), error = function(e) "TBD"),
                    as.numeric(row["predicted_btc"]),
                    as.numeric(row["yield"])
            )
        }), collapse = "")

        auction_html <- sprintf('
            <h3 style="color: #1B3A6B; margin-top: 20px;">Bonds on Auction This Week</h3>
            <table style="width: 100%%%%; border-collapse: collapse; margin: 15px 0;">
                <thead>
                    <tr style="background: #1B3A6B; color: white;">
                        <th style="padding: 12px; text-align: left;">Bond</th>
                        <th style="padding: 12px; text-align: left;">Expected Date</th>
                        <th style="padding: 12px; text-align: left;">Predicted B/C</th>
                        <th style="padding: 12px; text-align: left;">Yield</th>
                    </tr>
                </thead>
                <tbody>%s</tbody>
            </table>', bonds_html)
    }

    # Assemble full HTML document
    template <- sprintf('
<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <style>
        body { font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif; line-height: 1.6; color: #333; max-width: 900px; margin: 0 auto; padding: 20px; background: #f5f5f5; }
        .header { background: linear-gradient(135deg, #1B3A6B 0%%%%, #2A4D8C 100%%%%); color: white; padding: 30px; border-radius: 10px 10px 0 0; text-align: center; }
        .logo { font-size: 24px; font-weight: bold; margin-bottom: 10px; }
        .content { background: white; padding: 30px; border: 1px solid #e0e0e0; border-top: none; }
        .footer { background: #343A40; color: white; padding: 20px; text-align: center; border-radius: 0 0 10px 10px; font-size: 12px; }
    </style>
</head>
<body>
    <div class="header">
        <div class="logo">INSELE CAPITAL PARTNERS</div>
        <p>SA Government Bond Analysis Report</p>
        <p style="font-size: 14px;">%s</p>
        %s
    </div>
    <div class="content">
        %s
        %s
        <div style="background: #fff3cd; padding: 15px; border: 1px solid #ffc107; border-radius: 5px; margin: 20px 0;">
            <strong>Important Notice:</strong> This report contains proprietary analysis and should not be forwarded without authorization.
        </div>
    </div>
    <div class="footer">
        <p>&copy; %s Insele Capital Partners - Broking Services</p>
        <p>This report is confidential and intended solely for the addressee.</p>
    </div>
</body>
</html>',
        format(config$report_date, "%%B %%d, %%Y"),
        if (!is.null(config$client_name) && config$client_name != "") sprintf('<p style="font-size: 13px;">Prepared for: %s</p>', config$client_name) else "",
        body_html,
        auction_html,
        format(Sys.Date(), "%%Y")
    )

    return(template)
}

#' @export
# Convert plot to base64
plot_to_base64 <- function(plot, width = 8, height = 6, dpi = 100) {
    # Validate inputs
    if(is.null(plot)) {
        return(NULL)
    }

    # Check if plot is valid
    is_valid_plot <- tryCatch({
        "ggplot" %in% class(plot) ||
            "grob" %in% class(plot) ||
            "gtable" %in% class(plot) ||
            "arrangelist" %in% class(plot)
    }, error = function(e) FALSE)

    if(!is_valid_plot) {
        return(NULL)
    }

    tryCatch({
        # Create temporary file
        temp_file <- tempfile(fileext = ".png")

        # Save plot with appropriate method
        if("ggplot" %in% class(plot)) {
            # ggplot object
            ggsave(
                filename = temp_file,
                plot = plot,
                width = width,
                height = height,
                dpi = dpi,
                bg = "white",
                limitsize = FALSE
            )
        } else if("grob" %in% class(plot) || "gtable" %in% class(plot)) {
            # Grid graphics object
            png(temp_file, width = width * dpi, height = height * dpi, bg = "white")
            grid::grid.draw(plot)
            dev.off()
        } else if("arrangelist" %in% class(plot)) {
            # gridExtra arrangement
            ggsave(
                filename = temp_file,
                plot = gridExtra::marrangeGrob(plot, nrow = 1, ncol = 1),
                width = width,
                height = height,
                dpi = dpi,
                bg = "white"
            )
        } else {
            # Fallback for other plot types
            png(temp_file, width = width * dpi, height = height * dpi, bg = "white")
            print(plot)
            dev.off()
        }

        # Check if file was created and has content
        if(!file.exists(temp_file) || file.size(temp_file) == 0) {
            return(NULL)
        }

        # Read and encode
        img_data <- base64enc::base64encode(temp_file)

        # Clean up
        unlink(temp_file)

        return(img_data)

    }, error = function(e) {
        log_error(e, context = "plot_to_base64")

        # Ensure any open devices are closed
        while(dev.cur() > 1) dev.off()

        # Clean up temp file if it exists
        if(exists("temp_file") && file.exists(temp_file)) {
            unlink(temp_file)
        }

        return(NULL)
    })
}


# ================================================================================
# PRE-AUCTION REPORT FUNCTIONS
# ================================================================================

#' @export
#' Generate bond snapshot table for pre-auction report
#' @param data Filtered bond data
#' @param auction_bonds Character vector of bond names on auction
#' @return Data frame with latest metrics for each auction bond
generate_bond_snapshot_table <- function(data, auction_bonds) {
    snapshot <- data %>%
        filter(bond %in% auction_bonds) %>%
        group_by(bond) %>%
        filter(date == max(date, na.rm = TRUE)) %>%
        slice_head(n = 1) %>%
        ungroup()

    # Use any_of() for safe column selection
    col_map <- c(
        "Bond" = "bond",
        "Coupon (%)" = "coupon",
        "YTM (%)" = "yield_to_maturity",
        "Mod Duration" = "modified_duration",
        "Convexity" = "convexity",
        "BPV" = "dv01",
        "Clean Price" = "clean_price",
        "Accrued Int" = "accrued_interest",
        "Full Price" = "full_price"
    )

    available_cols <- col_map[col_map %in% names(snapshot)]
    snapshot <- snapshot %>%
        select(any_of(unname(available_cols)))

    # Rename to display names
    for (display_name in names(available_cols)) {
        col_name <- available_cols[[display_name]]
        if (col_name %in% names(snapshot)) {
            names(snapshot)[names(snapshot) == col_name] <- display_name
        }
    }

    snapshot <- snapshot %>%
        mutate(across(where(is.numeric), ~round(., 4)))

    return(snapshot)
}

#' @export
#' Generate auction forecast table using ARIMA predictions
#' @param data Filtered bond data
#' @param auction_bonds Character vector of bond names on auction
#' @return Data frame with forecast metrics for each auction bond
generate_auction_forecast_table <- function(data, auction_bonds) {
    forecasts <- lapply(auction_bonds, function(bond_name) {
        pred <- tryCatch(
            predict_btc_arima(data, bond_name),
            error = function(e) list(
                forecast = NA, lower_80 = NA, upper_80 = NA,
                lower_95 = NA, upper_95 = NA, confidence = "N/A"
            )
        )

        hist_avg <- mean(data$bid_to_cover[data$bond == bond_name], na.rm = TRUE)
        last_btc_vals <- na.omit(data$bid_to_cover[data$bond == bond_name & !is.na(data$bid_to_cover)])
        last_btc <- if (length(last_btc_vals) > 0) tail(last_btc_vals, 1) else NA

        data.frame(
            Bond = bond_name,
            `Predicted BTC` = round(pred$forecast %||% NA, 2),
            `80% CI` = sprintf("[%.2f, %.2f]", pred$lower_80 %||% NA, pred$upper_80 %||% NA),
            `95% CI` = sprintf("[%.2f, %.2f]", pred$lower_95 %||% NA, pred$upper_95 %||% NA),
            Confidence = pred$confidence %||% "N/A",
            `Hist Avg` = round(hist_avg, 2),
            `Last BTC` = round(last_btc, 2),
            check.names = FALSE,
            stringsAsFactors = FALSE
        )
    })

    do.call(rbind, forecasts)
}

#' @export
#' Generate Pre-Auction PDF Report
#' @param file Output file path
#' @param config Report configuration list (from report_config())
#' @param filtered_data Filtered bond data
#' @param processed_data Processed bond data (latest snapshot per bond)
#' @param carry_data Carry and roll return data
#' @param logo_grob Grid graphics object for logo (or NULL)
generate_pre_auction_pdf <- function(file, config, filtered_data, processed_data,
                                     carry_data, logo_grob) {
    require(gridExtra)
    require(grid)

    temp_dir <- tempdir()
    temp_pdf <- file.path(temp_dir, paste0("pre_auction_", Sys.getpid(), ".pdf"))
    total_pages <- 6
    auction_bonds <- config$pre_auction_bonds
    auction_date <- config$auction_date %||% Sys.Date()
    client_name <- config$client_name %||% ""

    # Helper: add footer to each page
    add_footer <- function(page_num, total) {
        grid.text(
            sprintf("(c) %s Insele Capital Partners - Confidential", format(Sys.Date(), "%Y")),
            x = 0.5, y = 0.02,
            gp = gpar(fontsize = 8, col = "#999999")
        )
        grid.text(
            sprintf("Page %d of %d", page_num, total),
            x = 0.95, y = 0.02,
            gp = gpar(fontsize = 8, col = "#999999")
        )
    }

    # ══════════════════════════════════════════════════════════════════
    # PRE-RENDER all plots to grobs BEFORE opening PDF device
    # ══════════════════════════════════════════════════════════════════
    message("[PRE-AUCTION PDF] Pre-rendering plots...")

    # Helper to safely render a ggplot to a rasterGrob
    safe_render_plot <- function(plot_expr, label = "plot") {
        tryCatch({
            p <- eval(plot_expr)
            if (is.null(p)) return(NULL)
            if ("ggplot" %in% class(p)) {
                temp_png <- tempfile(fileext = ".png")
                on.exit(unlink(temp_png), add = TRUE)
                ggsave(temp_png, p, width = 10, height = 6, dpi = 150, bg = "white")
                img <- png::readPNG(temp_png)
                return(grid::rasterGrob(img, interpolate = TRUE))
            } else if ("grob" %in% class(p) || "gtable" %in% class(p)) {
                return(p)
            } else {
                return(NULL)
            }
        }, error = function(e) {
            message(sprintf("[PRE-AUCTION PDF] Failed to render %s: %s", label, e$message))
            return(NULL)
        })
    }

    # Page 3: Auction Performance History
    auction_perf_grob <- safe_render_plot(
        quote(generate_enhanced_auction_analytics(filtered_data, list(selected_bonds = auction_bonds))),
        "auction_performance"
    )

    # Page 4: Historical Patterns
    auction_patterns_grob <- safe_render_plot(
        quote(generate_auction_pattern_analysis(filtered_data, list(), selected_bonds = auction_bonds)),
        "auction_patterns"
    )

    # Page 5: Supply & Demand - Bid Distribution
    bid_dist_grob <- safe_render_plot(
        quote(generate_bid_distribution_plot(filtered_data, list(), selected_bonds = auction_bonds)),
        "bid_distribution"
    )

    # Page 5: YTD Bond Issuance
    ytd_issuance_grob <- safe_render_plot(
        quote(generate_ytd_bond_issuance_chart(filtered_data, list())),
        "ytd_issuance"
    )

    # Page 6: Sentiment gauge
    sentiment_grob <- safe_render_plot(
        quote(generate_auction_sentiment_gauge(filtered_data, list())),
        "sentiment_gauge"
    )

    # Page 2: Bond Snapshot Table
    snapshot_tbl <- tryCatch({
        # Use filtered_data primarily, fall back to processed_data
        tbl_data <- if (!is.null(filtered_data) && nrow(filtered_data) > 0) {
            filtered_data
        } else if (!is.null(processed_data) && nrow(processed_data) > 0) {
            processed_data
        } else {
            NULL
        }

        if (!is.null(tbl_data)) {
            generate_bond_snapshot_table(tbl_data, auction_bonds)
        } else {
            NULL
        }
    }, error = function(e) {
        message(sprintf("[PRE-AUCTION PDF] Snapshot table error: %s", e$message))
        NULL
    })

    # Page 6: Forecast Table
    forecast_tbl <- tryCatch({
        if (!is.null(filtered_data) && nrow(filtered_data) > 0) {
            generate_auction_forecast_table(filtered_data, auction_bonds)
        } else {
            NULL
        }
    }, error = function(e) {
        message(sprintf("[PRE-AUCTION PDF] Forecast table error: %s", e$message))
        NULL
    })

    message("[PRE-AUCTION PDF] Pre-rendering complete. Opening PDF device...")

    # ══════════════════════════════════════════════════════════════════
    # GENERATE PDF
    # ══════════════════════════════════════════════════════════════════
    tryCatch({
        pdf(temp_pdf, width = 11, height = 8.5)

        # ─── PAGE 1: COVER ─────────────────────────────────────────
        grid.newpage()

        # Background header bar
        grid.rect(x = 0.5, y = 0.92, width = 1, height = 0.16,
                  gp = gpar(fill = "#1B3A6B", col = NA))

        # Logo
        if (!is.null(logo_grob)) {
            pushViewport(viewport(x = 0.5, y = 0.92, width = 0.2, height = 0.12))
            grid.draw(logo_grob)
            popViewport()
        }

        # Title
        grid.text("Weekly Auction Preview",
                  x = 0.5, y = 0.72,
                  gp = gpar(fontsize = 28, fontface = "bold", col = "#1B3A6B"))

        # Auction Date
        grid.text(format(auction_date, "%A, %B %d, %Y"),
                  x = 0.5, y = 0.63,
                  gp = gpar(fontsize = 18, col = "#555555"))

        # Bonds
        grid.text(paste("Bonds on Auction:", paste(auction_bonds, collapse = ", ")),
                  x = 0.5, y = 0.54,
                  gp = gpar(fontsize = 14, col = "#333333"))

        # Client
        if (nchar(client_name) > 0) {
            grid.text(paste("Prepared for:", client_name),
                      x = 0.5, y = 0.46,
                      gp = gpar(fontsize = 13, col = "#666666"))
        }

        # Company line
        grid.text("Prepared by Insele Capital Partners - Broking Services",
                  x = 0.5, y = 0.36,
                  gp = gpar(fontsize = 12, fontface = "italic", col = "#1B3A6B"))

        # Date generated
        grid.text(paste("Generated:", format(Sys.Date(), "%B %d, %Y")),
                  x = 0.5, y = 0.28,
                  gp = gpar(fontsize = 11, col = "#999999"))

        # Footer bar
        grid.rect(x = 0.5, y = 0.04, width = 1, height = 0.06,
                  gp = gpar(fill = "#1B3A6B", col = NA))
        grid.text("INSELE CAPITAL PARTNERS",
                  x = 0.5, y = 0.04,
                  gp = gpar(fontsize = 10, col = "white", fontface = "bold"))

        add_footer(1, total_pages)

        # ─── PAGE 2: BOND SNAPSHOT TABLE ───────────────────────────
        grid.newpage()

        # Page header
        grid.rect(x = 0.5, y = 0.96, width = 1, height = 0.06,
                  gp = gpar(fill = "#1B3A6B", col = NA))
        grid.text("Bond Snapshot - Current Market Data",
                  x = 0.5, y = 0.96,
                  gp = gpar(fontsize = 16, fontface = "bold", col = "white"))

        if (!is.null(snapshot_tbl) && nrow(snapshot_tbl) > 0) {
            tryCatch({
                # Style the table
                theme_tbl <- ttheme_default(
                    core = list(
                        bg_params = list(
                            fill = rep(c("white", "#f0f4f8"), length.out = nrow(snapshot_tbl))
                        ),
                        fg_params = list(fontsize = 10)
                    ),
                    colhead = list(
                        bg_params = list(fill = "#1B3A6B"),
                        fg_params = list(col = "white", fontsize = 10, fontface = "bold")
                    )
                )
                tbl_grob <- tableGrob(snapshot_tbl, rows = NULL, theme = theme_tbl)
                pushViewport(viewport(x = 0.5, y = 0.5, width = 0.9, height = 0.8))
                grid.draw(tbl_grob)
                popViewport()
            }, error = function(e) {
                grid.text("Bond snapshot table unavailable", x = 0.5, y = 0.5,
                          gp = gpar(fontsize = 14, col = "#999999"))
            })
        } else {
            grid.text("Bond snapshot data unavailable", x = 0.5, y = 0.5,
                      gp = gpar(fontsize = 14, col = "#999999"))
        }

        add_footer(2, total_pages)

        # ─── PAGE 3: AUCTION PERFORMANCE HISTORY ───────────────────
        grid.newpage()
        grid.rect(x = 0.5, y = 0.96, width = 1, height = 0.06,
                  gp = gpar(fill = "#1B3A6B", col = NA))
        grid.text("Auction Performance History",
                  x = 0.5, y = 0.96,
                  gp = gpar(fontsize = 16, fontface = "bold", col = "white"))

        if (!is.null(auction_perf_grob)) {
            pushViewport(viewport(x = 0.5, y = 0.47, width = 0.92, height = 0.82))
            grid.draw(auction_perf_grob)
            popViewport()
        } else {
            grid.text("Auction performance chart unavailable", x = 0.5, y = 0.5,
                      gp = gpar(fontsize = 14, col = "#999999"))
        }

        add_footer(3, total_pages)

        # ─── PAGE 4: HISTORICAL PATTERNS ───────────────────────────
        grid.newpage()
        grid.rect(x = 0.5, y = 0.96, width = 1, height = 0.06,
                  gp = gpar(fill = "#1B3A6B", col = NA))
        grid.text("Historical Auction Patterns",
                  x = 0.5, y = 0.96,
                  gp = gpar(fontsize = 16, fontface = "bold", col = "white"))

        if (!is.null(auction_patterns_grob)) {
            pushViewport(viewport(x = 0.5, y = 0.47, width = 0.92, height = 0.82))
            grid.draw(auction_patterns_grob)
            popViewport()
        } else {
            grid.text("Historical patterns chart unavailable", x = 0.5, y = 0.5,
                      gp = gpar(fontsize = 14, col = "#999999"))
        }

        add_footer(4, total_pages)

        # ─── PAGE 5: SUPPLY & DEMAND ──────────────────────────────
        grid.newpage()
        grid.rect(x = 0.5, y = 0.96, width = 1, height = 0.06,
                  gp = gpar(fill = "#1B3A6B", col = NA))
        grid.text("Supply & Demand Analysis",
                  x = 0.5, y = 0.96,
                  gp = gpar(fontsize = 16, fontface = "bold", col = "white"))

        has_bid_dist <- !is.null(bid_dist_grob)
        has_ytd <- !is.null(ytd_issuance_grob)

        if (has_bid_dist && has_ytd) {
            # Stack both plots vertically
            pushViewport(viewport(x = 0.5, y = 0.7, width = 0.92, height = 0.4))
            grid.draw(bid_dist_grob)
            popViewport()
            pushViewport(viewport(x = 0.5, y = 0.27, width = 0.92, height = 0.4))
            grid.draw(ytd_issuance_grob)
            popViewport()
        } else if (has_bid_dist) {
            pushViewport(viewport(x = 0.5, y = 0.47, width = 0.92, height = 0.82))
            grid.draw(bid_dist_grob)
            popViewport()
        } else if (has_ytd) {
            pushViewport(viewport(x = 0.5, y = 0.47, width = 0.92, height = 0.82))
            grid.draw(ytd_issuance_grob)
            popViewport()
        } else {
            grid.text("Supply & demand charts unavailable", x = 0.5, y = 0.5,
                      gp = gpar(fontsize = 14, col = "#999999"))
        }

        add_footer(5, total_pages)

        # ─── PAGE 6: FORECAST & SENTIMENT ─────────────────────────
        grid.newpage()
        grid.rect(x = 0.5, y = 0.96, width = 1, height = 0.06,
                  gp = gpar(fill = "#1B3A6B", col = NA))
        grid.text("Forecast & Sentiment",
                  x = 0.5, y = 0.96,
                  gp = gpar(fontsize = 16, fontface = "bold", col = "white"))

        # Top half: Forecast table
        if (!is.null(forecast_tbl) && nrow(forecast_tbl) > 0) {
            tryCatch({
                theme_fc <- ttheme_default(
                    core = list(
                        bg_params = list(
                            fill = rep(c("white", "#f0f4f8"), length.out = nrow(forecast_tbl))
                        ),
                        fg_params = list(fontsize = 9)
                    ),
                    colhead = list(
                        bg_params = list(fill = "#1B3A6B"),
                        fg_params = list(col = "white", fontsize = 9, fontface = "bold")
                    )
                )

                # Add table title
                grid.text("ARIMA Bid-to-Cover Forecast",
                          x = 0.5, y = 0.87,
                          gp = gpar(fontsize = 13, fontface = "bold", col = "#1B3A6B"))

                fc_grob <- tableGrob(forecast_tbl, rows = NULL, theme = theme_fc)
                pushViewport(viewport(x = 0.5, y = 0.7, width = 0.9, height = 0.28))
                grid.draw(fc_grob)
                popViewport()
            }, error = function(e) {
                grid.text("Forecast table unavailable", x = 0.5, y = 0.7,
                          gp = gpar(fontsize = 12, col = "#999999"))
            })
        } else {
            grid.text("Forecast data unavailable", x = 0.5, y = 0.7,
                      gp = gpar(fontsize = 12, col = "#999999"))
        }

        # Bottom half: Sentiment gauge
        if (!is.null(sentiment_grob)) {
            pushViewport(viewport(x = 0.5, y = 0.25, width = 0.8, height = 0.4))
            grid.draw(sentiment_grob)
            popViewport()
        } else {
            grid.text("Sentiment gauge unavailable", x = 0.5, y = 0.25,
                      gp = gpar(fontsize = 12, col = "#999999"))
        }

        add_footer(6, total_pages)

        dev.off()

        # Copy to output
        if (file.exists(temp_pdf)) {
            file.copy(temp_pdf, file, overwrite = TRUE)
            unlink(temp_pdf)
        }

        message("[PRE-AUCTION PDF] Generation complete.")

    }, error = function(e) {
        message(sprintf("[PRE-AUCTION PDF] ERROR: %s", e$message))
        while (dev.cur() > 1) { try(dev.off(), silent = TRUE) }
        # Fallback: create minimal error PDF
        pdf(file, width = 11, height = 8.5)
        plot.new()
        text(0.5, 0.5, "Pre-Auction Report generation encountered an error.\nPlease try again.",
             cex = 1.3, col = "#E53935")
        dev.off()
    })
}


#' @export
#' Generate Pre-Auction HTML Report
#' @param config Report configuration list
#' @param filtered_data Filtered bond data
#' @param processed_data Processed bond data
#' @param carry_data Carry and roll return data
#' @return HTML string for the report
create_pre_auction_html_report <- function(config, filtered_data, processed_data, carry_data) {
    auction_bonds <- config$pre_auction_bonds
    auction_date <- config$auction_date %||% Sys.Date()
    client_name <- config$client_name %||% ""

    # ══════════════════════════════════════════════════════════════════
    # BOND SNAPSHOT TABLE
    # ══════════════════════════════════════════════════════════════════
    snapshot_html <- ""
    tryCatch({
        tbl_data <- if (!is.null(filtered_data) && nrow(filtered_data) > 0) filtered_data
                    else if (!is.null(processed_data) && nrow(processed_data) > 0) processed_data
                    else NULL

        if (!is.null(tbl_data)) {
            snapshot <- generate_bond_snapshot_table(tbl_data, auction_bonds)
            if (nrow(snapshot) > 0) {
                rows_html <- paste(apply(snapshot, 1, function(row) {
                    cells <- paste(sprintf('<td style="padding: 8px 10px; border-bottom: 1px solid #ddd;">%s</td>',
                                          row), collapse = "")
                    sprintf("<tr>%s</tr>", cells)
                }), collapse = "\n")

                headers <- paste(sprintf('<th style="background: #1B3A6B; color: white; padding: 10px; text-align: left;">%s</th>',
                                         names(snapshot)), collapse = "")

                snapshot_html <- sprintf(
                    '<table style="width: 100%%%%; border-collapse: collapse; margin: 20px 0;">
                        <thead><tr>%s</tr></thead>
                        <tbody>%s</tbody>
                    </table>', headers, rows_html)
            }
        }
    }, error = function(e) {
        snapshot_html <<- '<p style="color: #999;">Bond snapshot data unavailable.</p>'
    })

    # ══════════════════════════════════════════════════════════════════
    # KPI CARDS
    # ══════════════════════════════════════════════════════════════════
    kpi_html <- ""
    tryCatch({
        if (!is.null(filtered_data) && nrow(filtered_data) > 0) {
            kpi_cards <- lapply(auction_bonds, function(b) {
                bond_data <- filtered_data %>%
                    filter(bond == b) %>%
                    arrange(desc(date))

                ytm_val <- if ("yield_to_maturity" %in% names(bond_data) && nrow(bond_data) > 0)
                    sprintf("%.3f%%", bond_data$yield_to_maturity[1]) else "N/A"

                btc_val <- if ("bid_to_cover" %in% names(bond_data)) {
                    btc_data <- bond_data %>% filter(!is.na(bid_to_cover))
                    if (nrow(btc_data) > 0) sprintf("%.2fx", btc_data$bid_to_cover[1]) else "N/A"
                } else "N/A"

                sprintf(
                    '<div class="kpi-card">
                        <div style="font-size: 14px; font-weight: bold; color: #1B3A6B; margin-bottom: 8px;">%s</div>
                        <div class="kpi-value">%s</div>
                        <div class="kpi-label">Current YTM</div>
                        <div style="margin-top: 8px; font-size: 16px; font-weight: bold; color: #333;">%s</div>
                        <div class="kpi-label">Last BTC</div>
                    </div>', b, ytm_val, btc_val)
            })
            kpi_html <- paste(kpi_cards, collapse = "\n")
        }
    }, error = function(e) {
        kpi_html <<- ""
    })

    # ══════════════════════════════════════════════════════════════════
    # CHARTS (rendered as base64 embedded images)
    # ══════════════════════════════════════════════════════════════════
    render_chart_html <- function(plot_expr, title, label = "chart") {
        tryCatch({
            p <- eval(plot_expr)
            if (is.null(p)) return(sprintf('<p style="color: #999;">%s chart unavailable.</p>', title))
            b64 <- plot_to_base64(p, width = 10, height = 6, dpi = 150)
            if (is.null(b64)) return(sprintf('<p style="color: #999;">%s chart unavailable.</p>', title))
            sprintf('<div class="chart-container"><img src="data:image/png;base64,%s" alt="%s"/></div>',
                    b64, title)
        }, error = function(e) {
            message(sprintf("[PRE-AUCTION HTML] Chart error (%s): %s", label, e$message))
            sprintf('<p style="color: #999;">%s chart unavailable.</p>', title)
        })
    }

    perf_chart <- render_chart_html(
        quote(generate_enhanced_auction_analytics(filtered_data, list(selected_bonds = auction_bonds))),
        "Auction Performance History", "auction_perf"
    )

    patterns_chart <- render_chart_html(
        quote(generate_auction_pattern_analysis(filtered_data, list(), selected_bonds = auction_bonds)),
        "Historical Auction Patterns", "auction_patterns"
    )

    bid_dist_chart <- render_chart_html(
        quote(generate_bid_distribution_plot(filtered_data, list(), selected_bonds = auction_bonds)),
        "Bid Distribution", "bid_dist"
    )

    ytd_chart <- render_chart_html(
        quote(generate_ytd_bond_issuance_chart(filtered_data, list())),
        "YTD Bond Issuance", "ytd_issuance"
    )

    sentiment_chart <- render_chart_html(
        quote(generate_auction_sentiment_gauge(filtered_data, list())),
        "Market Sentiment", "sentiment"
    )

    # ══════════════════════════════════════════════════════════════════
    # FORECAST TABLE
    # ══════════════════════════════════════════════════════════════════
    forecast_html <- ""
    tryCatch({
        if (!is.null(filtered_data) && nrow(filtered_data) > 0) {
            fc_tbl <- generate_auction_forecast_table(filtered_data, auction_bonds)
            if (nrow(fc_tbl) > 0) {
                fc_rows <- paste(apply(fc_tbl, 1, function(row) {
                    cells <- paste(sprintf('<td style="padding: 8px 10px; border-bottom: 1px solid #ddd;">%s</td>',
                                          row), collapse = "")
                    sprintf("<tr>%s</tr>", cells)
                }), collapse = "\n")

                fc_headers <- paste(sprintf('<th style="background: #1B3A6B; color: white; padding: 10px; text-align: left;">%s</th>',
                                            names(fc_tbl)), collapse = "")

                forecast_html <- sprintf(
                    '<table style="width: 100%%%%; border-collapse: collapse; margin: 20px 0;">
                        <thead><tr>%s</tr></thead>
                        <tbody>%s</tbody>
                    </table>', fc_headers, fc_rows)
            }
        }
    }, error = function(e) {
        forecast_html <<- '<p style="color: #999;">Forecast data unavailable.</p>'
    })

    # ══════════════════════════════════════════════════════════════════
    # ASSEMBLE HTML
    # ══════════════════════════════════════════════════════════════════
    html <- sprintf('<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <style>
        body { font-family: "Segoe UI", Tahoma, Geneva, Verdana, sans-serif; margin: 0; padding: 0; color: #333; }
        .header { background: #1B3A6B; color: white; padding: 30px 40px; }
        .header h1 { margin: 0; font-size: 28px; }
        .header .subtitle { opacity: 0.85; font-size: 16px; margin-top: 8px; }
        .section { padding: 30px 40px; }
        .section h2 { color: #1B3A6B; border-bottom: 2px solid #1B3A6B; padding-bottom: 8px; }
        table { width: 100%%%%; border-collapse: collapse; margin: 20px 0; }
        th { background: #1B3A6B; color: white; padding: 10px; text-align: left; }
        td { padding: 8px 10px; border-bottom: 1px solid #ddd; }
        tr:nth-child(even) { background: #f8f9fa; }
        .chart-container { margin: 20px 0; text-align: center; }
        .chart-container img { max-width: 100%%%%; border: 1px solid #e0e0e0; border-radius: 4px; }
        .footer { background: #f8f9fa; padding: 20px 40px; text-align: center; color: #666; font-size: 12px; border-top: 2px solid #1B3A6B; }
        .kpi-row { display: flex; gap: 15px; margin: 20px 0; }
        .kpi-card { flex: 1; background: white; border: 1px solid #e0e0e0; border-radius: 8px; padding: 15px; text-align: center; border-top: 3px solid #1B3A6B; }
        .kpi-value { font-size: 24px; font-weight: bold; color: #1B3A6B; }
        .kpi-label { font-size: 12px; color: #666; margin-top: 4px; }
        .disclaimer { background: #fff3cd; padding: 15px; border: 1px solid #ffc107; border-radius: 5px; margin: 20px 0; }
    </style>
</head>
<body>
    <div class="header">
        <h1>Weekly Auction Preview</h1>
        <div class="subtitle">%s | Bonds: %s</div>
        <div class="subtitle">%s Generated: %s</div>
    </div>

    <div class="section">
        <div class="kpi-row">
            %s
        </div>
    </div>

    <div class="section">
        <h2>Bond Snapshot</h2>
        %s
    </div>

    <div class="section">
        <h2>Auction Performance History</h2>
        %s
    </div>

    <div class="section">
        <h2>Historical Auction Patterns</h2>
        %s
    </div>

    <div class="section">
        <h2>Supply &amp; Demand Analysis</h2>
        %s
        %s
    </div>

    <div class="section">
        <h2>Auction Forecast</h2>
        %s
        %s
    </div>

    <div class="disclaimer">
        <strong>Important Notice:</strong> This report contains proprietary analysis by Insele Capital Partners and should not be forwarded without authorization.
    </div>

    <div class="footer">
        &copy; %s Insele Capital Partners - Broking Services | Confidential
    </div>
</body>
</html>',
        format(auction_date, "%%A, %%B %%d, %%Y"),
        paste(auction_bonds, collapse = ", "),
        if (nchar(client_name) > 0) sprintf("Prepared for: %s |", client_name) else "",
        format(Sys.Date(), "%%B %%d, %%Y"),
        kpi_html,
        snapshot_html,
        perf_chart,
        patterns_chart,
        bid_dist_chart,
        ytd_chart,
        forecast_html,
        sentiment_chart,
        format(Sys.Date(), "%%Y")
    )

    return(html)
}