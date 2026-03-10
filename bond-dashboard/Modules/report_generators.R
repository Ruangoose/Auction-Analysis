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
            # Auction Demand Trend (Concession)
            concession_trend = function() {
                if(!is.null(filtered_data) && nrow(filtered_data) > 0) {
                    create_concession_trend_chart(filtered_data, list())
                } else { NULL }
            }
        ),
        intelligence = list(
            yield_percentile = function() {
                if(!is.null(filtered_data) && nrow(filtered_data) > 0) {
                    generate_yield_percentile_heatmap(filtered_data, list())
                } else { NULL }
            },
            rate_of_change = function() {
                if(!is.null(filtered_data) && nrow(filtered_data) > 0) {
                    generate_rate_of_change_monitor(filtered_data, list())
                } else { NULL }
            },
            curve_comparison = function() {
                if(!is.null(filtered_data) && nrow(filtered_data) > 0) {
                    generate_curve_comparison_plot(filtered_data, list())
                } else { NULL }
            },
            curve_steepness = function() {
                if(!is.null(filtered_data) && nrow(filtered_data) > 0) {
                    generate_curve_steepness_gauge(filtered_data, list())
                } else { NULL }
            },
            regime_probability = function() {
                if(!is.null(regime_data) && nrow(regime_data) > 0) {
                    generate_regime_probability_gauge(regime_data, list())
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
        auction = c("auction_performance", "auction_patterns", "concession_trend",
                     "auction_forecast", "demand_elasticity", "success_probability",
                     "bid_distribution", "ytd_issuance", "auction_sentiment"),
        intelligence = c("yield_percentile", "rate_of_change", "curve_comparison",
                          "curve_steepness", "regime_probability"),
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
        ungroup() %>%
        arrange(bond)

    # Build display table with safe column access
    display_df <- data.frame(
        Bond = snapshot$bond,
        stringsAsFactors = FALSE
    )

    if ("coupon" %in% names(snapshot))
        display_df$`Coupon (%)` <- sprintf("%.3f", snapshot$coupon)
    if ("yield_to_maturity" %in% names(snapshot))
        display_df$`YTM (%)` <- sprintf("%.3f", snapshot$yield_to_maturity)
    else if ("yield" %in% names(snapshot))
        display_df$`YTM (%)` <- sprintf("%.3f", snapshot$yield)
    if ("modified_duration" %in% names(snapshot))
        display_df$`Mod Duration` <- sprintf("%.4f", snapshot$modified_duration)
    if ("convexity" %in% names(snapshot))
        display_df$Convexity <- sprintf("%.4f", snapshot$convexity)
    if ("dv01" %in% names(snapshot))
        display_df$BPV <- sprintf("%.4f", snapshot$dv01)
    if ("clean_price" %in% names(snapshot))
        display_df$`Clean Price` <- sprintf("%.4f", snapshot$clean_price)
    if ("accrued_interest" %in% names(snapshot))
        display_df$`Accrued Int` <- sprintf("%.4f", snapshot$accrued_interest)
    if ("full_price" %in% names(snapshot))
        display_df$`Full Price` <- sprintf("%.4f", snapshot$full_price)

    return(display_df)
}

#' @export
#' Generate bond snapshot as a properly formatted tableGrob (Fix 1)
#' @param data Filtered bond data
#' @param auction_bonds Character vector of bond names on auction
#' @return A tableGrob suitable for grid.draw()
generate_bond_snapshot_grob <- function(data, auction_bonds) {
    display_df <- generate_bond_snapshot_table(data, auction_bonds)
    if (is.null(display_df) || nrow(display_df) == 0) return(NULL)

    # CRITICAL: Convert everything to character and plain data.frame
    display_df <- as.data.frame(lapply(display_df, as.character), stringsAsFactors = FALSE)

    n_rows <- nrow(display_df)

    tt <- ttheme_default(
        base_size = 10,
        core = list(
            bg_params = list(
                fill = rep_len(c("white", "#F0F4F8"), n_rows),
                col = "#CCCCCC"
            ),
            padding = unit(c(5, 4), "mm")
        ),
        colhead = list(
            fg_params = list(col = "white", fontface = "bold"),
            bg_params = list(fill = "#1B3A6B", col = "#1B3A6B"),
            padding = unit(c(5, 5), "mm")
        )
    )

    tg <- tableGrob(display_df, rows = NULL, theme = tt)
    return(tg)
}

#' @export
#' Generate auction outlook narrative text
#' @param data Full filtered bond data
#' @param auction_bonds Character vector of bond names on auction
#' @param forecasts_df Forecast data frame from generate_auction_forecast_table()
#' @return Character string with narrative text
generate_auction_outlook_text <- function(data, auction_bonds, forecasts_df) {
    outlook_parts <- lapply(auction_bonds, function(b) {
        bond_data <- data %>% filter(bond == b, !is.na(bid_to_cover))

        n_auctions <- nrow(bond_data)
        avg_btc <- if (n_auctions > 0) mean(bond_data$bid_to_cover, na.rm = TRUE) else NA
        last_btc <- if (n_auctions > 0) tail(bond_data$bid_to_cover, 1) else NA
        last_3_avg <- if (n_auctions >= 3) mean(tail(bond_data$bid_to_cover, 3), na.rm = TRUE) else avg_btc

        # Trend
        trend <- if (n_auctions >= 3) {
            recent_3 <- tail(bond_data$bid_to_cover, 3)
            if (recent_3[3] > recent_3[1] + 0.2) "improving"
            else if (recent_3[3] < recent_3[1] - 0.2) "deteriorating"
            else "stable"
        } else {
            "insufficient history"
        }

        # Forecast from forecasts_df
        fc <- if (!is.null(forecasts_df) && is.data.frame(forecasts_df))
            forecasts_df[forecasts_df$Bond == b, ] else data.frame()
        pred_btc <- if (nrow(fc) > 0 && "Predicted BTC" %in% names(fc))
            fc$`Predicted BTC`[1] else NA
        signal <- if (!is.na(pred_btc)) {
            if (pred_btc >= 3.5) "STRONG BUY"
            else if (pred_btc >= 2.5) "BUY"
            else if (pred_btc >= 2.0) "HOLD"
            else "CAUTION"
        } else "N/A"

        # Current yield and duration for context
        ytm_col <- if ("yield_to_maturity" %in% names(data)) "yield_to_maturity" else if ("yield" %in% names(data)) "yield" else NULL
        latest <- data %>% filter(bond == b) %>%
            filter(date == max(date)) %>% slice_head(n = 1)
        yield_txt <- if (!is.null(ytm_col) && ytm_col %in% names(latest) && nrow(latest) > 0)
            sprintf("%.2f%%", latest[[ytm_col]][1]) else "N/A"
        dur_txt <- if ("modified_duration" %in% names(latest) && nrow(latest) > 0)
            sprintf("%.1f", latest$modified_duration[1]) else "N/A"

        list(bond = b, n = n_auctions, avg = avg_btc, last = last_btc,
             last3 = last_3_avg, trend = trend, pred = pred_btc,
             signal = signal, yield = yield_txt, duration = dur_txt)
    })

    # Build narrative text
    lines <- c()
    lines <- c(lines, sprintf("This week's auction features %d bonds: %s.",
                               length(auction_bonds), paste(auction_bonds, collapse = ", ")))
    lines <- c(lines, "")

    for (o in outlook_parts) {
        lines <- c(lines, sprintf(
            "%s  |  YTM: %s  |  Dur: %s  |  Signal: %s",
            o$bond, o$yield, o$duration, o$signal
        ))
        lines <- c(lines, sprintf(
            "    Hist Avg: %.2fx (%d auctions)  |  Last: %.2fx  |  Trend: %s  |  Forecast: %.2fx",
            if (!is.na(o$avg)) o$avg else 0, o$n,
            if (!is.na(o$last)) o$last else 0, o$trend,
            if (!is.na(o$pred)) o$pred else 0
        ))
        lines <- c(lines, "")
    }

    # Overall market context
    all_recent <- tryCatch({
        data %>%
            filter(!is.na(bid_to_cover), date >= Sys.Date() - 30) %>%
            summarise(avg = mean(bid_to_cover), n = n(), pct_strong = mean(bid_to_cover > 3) * 100)
    }, error = function(e) data.frame(n = 0))

    if (nrow(all_recent) > 0 && all_recent$n > 0) {
        lines <- c(lines, sprintf(
            "Market Context: %d auctions in the last 30 days with average BTC of %.2fx. %.0f%% achieved strong coverage (>3x).",
            all_recent$n, all_recent$avg, all_recent$pct_strong
        ))
    }

    return(paste(lines, collapse = "\n"))
}

#' @export
#' Generate outlook parts (KPI data) for auction bonds
#' @param data Full filtered bond data
#' @param auction_bonds Character vector of bond names on auction
#' @param forecasts_df Forecast data frame
#' @return List of lists with per-bond KPI data
generate_outlook_parts <- function(data, auction_bonds, forecasts_df) {
    lapply(auction_bonds, function(b) {
        bond_data <- data %>% filter(bond == b, !is.na(bid_to_cover))
        n_auctions <- nrow(bond_data)

        # Forecast from forecasts_df
        fc <- if (!is.null(forecasts_df) && is.data.frame(forecasts_df))
            forecasts_df[forecasts_df$Bond == b, ] else data.frame()
        pred_btc <- if (nrow(fc) > 0 && "Predicted BTC" %in% names(fc))
            fc$`Predicted BTC`[1] else NA
        signal <- if (!is.na(pred_btc)) {
            if (pred_btc >= 3.5) "STRONG BUY"
            else if (pred_btc >= 2.5) "BUY"
            else if (pred_btc >= 2.0) "HOLD"
            else "CAUTION"
        } else "N/A"

        # YTM
        ytm_col <- if ("yield_to_maturity" %in% names(data)) "yield_to_maturity" else if ("yield" %in% names(data)) "yield" else NULL
        latest <- data %>% filter(bond == b) %>%
            filter(date == max(date)) %>% slice_head(n = 1)
        yield_txt <- if (!is.null(ytm_col) && ytm_col %in% names(latest) && nrow(latest) > 0)
            sprintf("%.2f%%", latest[[ytm_col]][1]) else "N/A"

        list(bond = b, yield = yield_txt,
             pred = if (!is.na(pred_btc)) sprintf("%.2fx", pred_btc) else "N/A",
             signal = signal)
    })
}

#' @export
#' Generate recent auction history table for auction bonds
#' @param data Full filtered bond data
#' @param auction_bonds Character vector of bond names on auction
#' @param n_recent Number of recent auctions to show per bond
#' @return Data frame with recent auction history
generate_recent_auction_history <- function(data, auction_bonds, n_recent = 5) {
    history <- data %>%
        filter(bond %in% auction_bonds, !is.na(bid_to_cover)) %>%
        group_by(bond) %>%
        arrange(desc(date)) %>%
        slice_head(n = n_recent) %>%
        ungroup() %>%
        arrange(bond, desc(date))

    if (nrow(history) == 0) return(NULL)

    display_df <- data.frame(
        Date = format(history$date, "%b %d, %Y"),
        Bond = history$bond,
        stringsAsFactors = FALSE
    )

    if ("offer_amount" %in% names(history))
        display_df$`Offer (R bn)` <- sprintf("%.2f", history$offer_amount / 1e9)
    if ("bids_received" %in% names(history))
        display_df$`Bids (R bn)` <- sprintf("%.2f", history$bids_received / 1e9)

    display_df$BTC <- sprintf("%.2fx", history$bid_to_cover)
    display_df$Performance <- dplyr::case_when(
        history$bid_to_cover >= 3.0 ~ "Strong",
        history$bid_to_cover >= 2.5 ~ "Good",
        history$bid_to_cover >= 2.0 ~ "Normal",
        TRUE ~ "Weak"
    )

    return(display_df)
}

#' @export
#' Generate recent auction history as a tableGrob
#' @param data Full filtered bond data
#' @param auction_bonds Character vector of bond names on auction
#' @param n_recent Number of recent auctions per bond
#' @return A tableGrob suitable for grid.draw()
generate_recent_auction_history_grob <- function(data, auction_bonds, n_recent = 5) {
    recent <- data %>%
        filter(bond %in% auction_bonds, !is.na(bid_to_cover)) %>%
        group_by(bond) %>%
        arrange(desc(date)) %>%
        slice_head(n = n_recent) %>%
        ungroup() %>%
        arrange(bond, desc(date))

    if (nrow(recent) == 0) return(NULL)

    display_df <- data.frame(
        Date = format(recent$date, "%b %d, %y"),
        Bond = recent$bond,
        stringsAsFactors = FALSE
    )

    if ("offer_amount" %in% names(recent))
        display_df$`Offer(Rbn)` <- sprintf("%.2f", recent$offer_amount / 1e9)
    if ("bids_received" %in% names(recent))
        display_df$`Bids(Rbn)` <- sprintf("%.2f", recent$bids_received / 1e9)

    display_df$BTC <- sprintf("%.2fx", recent$bid_to_cover)
    display_df$Rating <- ifelse(recent$bid_to_cover >= 3, "Strong",
                          ifelse(recent$bid_to_cover >= 2.5, "Good",
                          ifelse(recent$bid_to_cover >= 2, "Normal", "Weak")))

    # Convert ALL to character
    display_df <- as.data.frame(lapply(display_df, as.character), stringsAsFactors = FALSE)

    n_rows <- nrow(display_df)

    tt <- ttheme_default(
        base_size = 8,
        core = list(
            bg_params = list(
                fill = rep_len(c("white", "#F0F4F8"), n_rows),
                col = "#CCCCCC"
            ),
            padding = unit(c(3, 2), "mm")
        ),
        colhead = list(
            fg_params = list(col = "white", fontface = "bold"),
            bg_params = list(fill = "#1B3A6B", col = "#1B3A6B"),
            padding = unit(c(3, 3), "mm")
        )
    )

    tg <- tableGrob(display_df, rows = NULL, theme = tt)
    return(tg)
}

#' @export
#' Generate yield curve context chart with auction bonds highlighted
#' @param data Full filtered bond data
#' @param auction_bonds Character vector of bond names on auction
#' @return ggplot object
generate_auction_yield_curve <- function(data, auction_bonds) {
    ytm_col <- if ("yield_to_maturity" %in% names(data)) "yield_to_maturity" else if ("yield" %in% names(data)) "yield" else NULL
    if (is.null(ytm_col) || !"modified_duration" %in% names(data)) return(NULL)

    latest <- data %>%
        group_by(bond) %>%
        filter(date == max(date, na.rm = TRUE)) %>%
        slice_head(n = 1) %>%
        ungroup() %>%
        filter(!is.na(.data[[ytm_col]]), !is.na(modified_duration))

    if (nrow(latest) < 3) return(NULL)

    latest <- latest %>%
        mutate(yield_val = .data[[ytm_col]],
               is_auction = bond %in% auction_bonds)

    # Fit curve for fair value line
    fit <- tryCatch(
        loess(yield_val ~ modified_duration, data = latest, span = 0.75),
        error = function(e) NULL
    )

    curve_df <- NULL
    if (!is.null(fit)) {
        curve_x <- seq(min(latest$modified_duration), max(latest$modified_duration), length.out = 100)
        curve_pred <- predict(fit, newdata = data.frame(modified_duration = curve_x))
        curve_df <- data.frame(modified_duration = curve_x, yield_val = curve_pred)

        # Calculate spread to curve for auction bonds
        latest$fitted_yield <- predict(fit, newdata = latest)
        latest$spread_bps <- (latest$yield_val - latest$fitted_yield) * 100
    }

    p <- ggplot() +
        {if (!is.null(curve_df)) geom_line(data = curve_df, aes(x = modified_duration, y = yield_val),
                                            color = "#8BC34A", linewidth = 1.2, linetype = "dashed", alpha = 0.8)} +
        geom_point(data = latest %>% filter(!is_auction),
                   aes(x = modified_duration, y = yield_val),
                   color = "#999999", size = 3, alpha = 0.6) +
        geom_text(data = latest %>% filter(!is_auction),
                  aes(x = modified_duration, y = yield_val, label = bond),
                  color = "#999999", size = 2.5, vjust = -1.2, alpha = 0.7) +
        geom_point(data = latest %>% filter(is_auction),
                   aes(x = modified_duration, y = yield_val),
                   color = "#E53935", size = 5, alpha = 0.9) +
        geom_text(data = latest %>% filter(is_auction),
                  aes(x = modified_duration, y = yield_val, label = bond),
                  color = "#1B3A6B", size = 3.5, fontface = "bold", vjust = -1.5) +
        {if (!is.null(fit) && "spread_bps" %in% names(latest)) {
            geom_text(data = latest %>% filter(is_auction),
                      aes(x = modified_duration, y = yield_val,
                          label = sprintf("%+.0f bps", spread_bps)),
                      color = "#E53935", size = 2.8, vjust = 2.5, fontface = "italic")
        }} +
        labs(
            title = "SA Government Bond Yield Curve - Auction Bonds Highlighted",
            subtitle = paste0("As at ", format(max(latest$date), "%B %d, %Y"),
                              " | Red = Bonds on auction | Dashed = Fitted fair value curve"),
            x = "Modified Duration",
            y = "Yield to Maturity (%)",
            caption = "Source: Insele Capital Partners Bond Analytics"
        ) +
        theme_minimal(base_size = 11) +
        theme(
            plot.title = element_text(face = "bold", size = 13, color = "#1B3A6B"),
            plot.subtitle = element_text(size = 9, color = "grey50"),
            plot.caption = element_text(size = 8, color = "#999", hjust = 1),
            panel.grid.minor = element_blank(),
            plot.background = element_rect(fill = "white", color = NA),
            panel.background = element_rect(fill = "white", color = NA)
        )

    return(p)
}

#' @export
#' Generate historical BTC line chart for auction bonds
#' @param data Full filtered bond data
#' @param auction_bonds Character vector of bond names on auction
#' @return ggplot object
generate_auction_btc_history_chart <- function(data, auction_bonds) {
    chart_data <- data %>%
        filter(bond %in% auction_bonds, !is.na(bid_to_cover)) %>%
        arrange(date)

    if (nrow(chart_data) == 0) return(NULL)

    # Use offer_date if available, otherwise date
    date_col <- if ("offer_date" %in% names(chart_data)) "offer_date" else "date"
    chart_data$plot_date <- as.Date(chart_data[[date_col]])

    p <- ggplot(chart_data, aes(x = plot_date, y = bid_to_cover, color = bond)) +
        geom_line(linewidth = 0.8, alpha = 0.8) +
        geom_point(size = 2.5, alpha = 0.9) +
        geom_hline(yintercept = 2, linetype = "dashed", color = "#C62828", alpha = 0.4, linewidth = 0.4) +
        geom_hline(yintercept = 3, linetype = "dashed", color = "#388E3C", alpha = 0.4, linewidth = 0.4) +
        annotate("text", x = min(chart_data$plot_date), y = 2, label = "Minimum (2x)",
                 hjust = 0, vjust = -0.5, size = 2.5, color = "#C62828") +
        annotate("text", x = min(chart_data$plot_date), y = 3, label = "Strong (3x)",
                 hjust = 0, vjust = -0.5, size = 2.5, color = "#388E3C") +
        scale_color_brewer(palette = "Set1", name = "Bond") +
        labs(
            title = "Bid-to-Cover Historical Performance",
            subtitle = paste("Bonds:", paste(auction_bonds, collapse = ", ")),
            x = NULL,
            y = "Bid-to-Cover Ratio"
        ) +
        theme_minimal(base_size = 10) +
        theme(
            plot.title = element_text(face = "bold", size = 12, color = "#1B3A6B"),
            plot.subtitle = element_text(size = 9, color = "grey50"),
            legend.position = "bottom",
            legend.title = element_text(size = 8),
            panel.grid.minor = element_blank(),
            plot.background = element_rect(fill = "white", color = NA),
            panel.background = element_rect(fill = "white", color = NA)
        )

    return(p)
}

#' @export
#' Generate enhanced forecast table with signal column
#' @param data Full filtered bond data
#' @param auction_bonds Character vector of bond names on auction
#' @return Data frame with forecast metrics and signal classification
generate_enhanced_forecast_table <- function(data, auction_bonds) {
    base_tbl <- generate_auction_forecast_table(data, auction_bonds)
    if (is.null(base_tbl) || nrow(base_tbl) == 0) return(base_tbl)

    # Add Signal column
    base_tbl$Signal <- dplyr::case_when(
        base_tbl$`Predicted BTC` >= 4.0 ~ "STRONG BUY",
        base_tbl$`Predicted BTC` >= 3.0 ~ "BUY",
        base_tbl$`Predicted BTC` >= 2.5 ~ "HOLD",
        base_tbl$`Predicted BTC` >= 2.0 ~ "CAUTION",
        TRUE ~ "WEAK"
    )

    # Reorder columns to put Signal after CI columns
    col_order <- c("Bond", "Predicted BTC", "80% CI", "95% CI", "Signal", "Confidence", "Hist Avg", "Last BTC")
    available_order <- col_order[col_order %in% names(base_tbl)]
    base_tbl <- base_tbl[, available_order, drop = FALSE]

    return(base_tbl)
}

#' @export
#' Generate enhanced forecast table as a color-coded tableGrob
#' @param data Full filtered bond data
#' @param auction_bonds Character vector of bond names on auction
#' @return A tableGrob suitable for grid.draw()
generate_enhanced_forecast_grob <- function(data, auction_bonds) {
    forecast_df <- generate_enhanced_forecast_table(data, auction_bonds)
    if (is.null(forecast_df) || nrow(forecast_df) == 0) return(NULL)

    # Add Signal column if not present
    if (!"Signal" %in% names(forecast_df) && "Predicted BTC" %in% names(forecast_df)) {
        pred <- as.numeric(forecast_df$`Predicted BTC`)
        forecast_df$Signal <- ifelse(pred >= 4.0, "STRONG BUY",
                              ifelse(pred >= 3.0, "BUY",
                              ifelse(pred >= 2.5, "HOLD",
                              ifelse(pred >= 2.0, "CAUTION", "WEAK"))))
        # Reorder columns to put Signal after 95% CI
        ci95_pos <- which(names(forecast_df) == "95% CI")
        if (length(ci95_pos) > 0) {
            sig_pos <- which(names(forecast_df) == "Signal")
            other_cols <- setdiff(seq_along(forecast_df), sig_pos)
            new_order <- append(other_cols, sig_pos, after = ci95_pos)
            forecast_df <- forecast_df[, new_order]
        }
    }

    # Convert ALL to character
    display_df <- as.data.frame(lapply(forecast_df, as.character), stringsAsFactors = FALSE)

    n_rows <- nrow(display_df)

    tt <- ttheme_default(
        base_size = 10,
        core = list(
            bg_params = list(
                fill = rep_len(c("white", "#F0F4F8"), n_rows),
                col = "#CCCCCC"
            ),
            padding = unit(c(5, 4), "mm")
        ),
        colhead = list(
            fg_params = list(col = "white", fontface = "bold"),
            bg_params = list(fill = "#1B3A6B", col = "#1B3A6B"),
            padding = unit(c(5, 5), "mm")
        )
    )

    tg <- tableGrob(display_df, rows = NULL, theme = tt)
    return(tg)
}

#' @export
#' Generate a text-based sentiment summary fallback when gauge fails
#' @param data Full filtered bond data
#' @return A grob with text-based sentiment summary
generate_sentiment_fallback_grob <- function(data) {
    # Calculate market-wide metrics
    recent_30d <- tryCatch({
        data %>%
            filter(!is.na(bid_to_cover), date >= Sys.Date() - 30) %>%
            summarise(
                avg_btc = mean(bid_to_cover, na.rm = TRUE),
                n_auctions = n(),
                pct_strong = mean(bid_to_cover > 3, na.rm = TRUE) * 100
            )
    }, error = function(e) data.frame(avg_btc = NA, n_auctions = 0, pct_strong = NA))

    recent_90d <- tryCatch({
        data %>%
            filter(!is.na(bid_to_cover), date >= Sys.Date() - 90) %>%
            summarise(avg_btc = mean(bid_to_cover, na.rm = TRUE))
    }, error = function(e) data.frame(avg_btc = NA))

    # Determine trend
    trend <- if (!is.na(recent_30d$avg_btc) && !is.na(recent_90d$avg_btc) && recent_90d$avg_btc != 0) {
        change <- (recent_30d$avg_btc - recent_90d$avg_btc) / recent_90d$avg_btc * 100
        if (change > 5) "Improving" else if (change < -5) "Deteriorating" else "Stable"
    } else "Insufficient data"

    lines <- c(
        "Market Sentiment Summary",
        "",
        sprintf("Average BTC (30d): %.2fx", ifelse(is.na(recent_30d$avg_btc), 0, recent_30d$avg_btc)),
        sprintf("Average BTC (90d): %.2fx", ifelse(is.na(recent_90d$avg_btc), 0, recent_90d$avg_btc)),
        sprintf("Auctions (30d): %d", recent_30d$n_auctions),
        sprintf("Strong coverage (>3x): %.0f%%", ifelse(is.na(recent_30d$pct_strong), 0, recent_30d$pct_strong)),
        sprintf("Trend: %s", trend)
    )

    text_content <- paste(lines, collapse = "\n")

    # Create a grob that can be drawn with grid.draw
    grob <- grobTree(
        rectGrob(gp = gpar(fill = "#f8f9fa", col = "#1B3A6B", lwd = 1.5)),
        textGrob(
            label = text_content,
            x = 0.5, y = 0.5,
            gp = gpar(fontsize = 11, lineheight = 1.5, col = "#333333")
        )
    )
    return(grob)
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
#' Generate YTD vs Prior Year Issuance Comparison Chart
#' @param data Full bond data with offer_amount column
#' @return ggplot object
generate_issuance_comparison_chart <- function(data) {
    if (!"offer_amount" %in% names(data) || all(is.na(data$offer_amount))) return(NULL)
    if (!"date" %in% names(data)) return(NULL)

    data <- ensure_date_columns(data)

    # Determine date ranges
    latest_date <- max(data$date, na.rm = TRUE)
    current_year <- as.numeric(format(latest_date, "%Y"))
    prior_year <- current_year - 1

    # Current YTD: Jan 1 of latest year to latest date
    ytd_start <- as.Date(paste0(current_year, "-01-01"))
    ytd_data <- data %>%
        filter(date >= ytd_start, date <= latest_date,
               !is.na(offer_amount), offer_amount > 0)

    # Prior year same period
    prior_start <- as.Date(paste0(prior_year, "-01-01"))
    # Same month-day in prior year
    prior_end_str <- paste0(prior_year, format(latest_date, "-%m-%d"))
    prior_end <- tryCatch(as.Date(prior_end_str), error = function(e) as.Date(paste0(prior_year, "-12-31")))
    prior_data <- data %>%
        filter(date >= prior_start, date <= prior_end,
               !is.na(offer_amount), offer_amount > 0)

    # Summarize each period
    summarize_period <- function(d, label) {
        if (nrow(d) == 0) return(data.frame())
        d %>%
            group_by(bond) %>%
            summarise(total = sum(offer_amount, na.rm = TRUE),
                      n = n(), .groups = "drop") %>%
            mutate(period = label,
                   total_mil = total / 1e6)
    }

    ytd_label <- paste0(current_year, " YTD")
    prior_label <- paste0(prior_year, " YTD (to ", format(prior_end, "%b %d"), ")")

    ytd_summary <- summarize_period(ytd_data, ytd_label)
    prior_summary <- summarize_period(prior_data, prior_label)

    combined <- bind_rows(ytd_summary, prior_summary)
    if (nrow(combined) == 0) return(NULL)

    # Order bonds by current YTD total
    bond_order <- ytd_summary %>% arrange(total) %>% pull(bond)
    extra_bonds <- setdiff(prior_summary$bond, bond_order)
    bond_order <- c(extra_bonds, bond_order)
    combined$bond <- factor(combined$bond, levels = bond_order)

    # Totals for subtitle
    ytd_total_bn <- sum(ytd_summary$total, na.rm = TRUE) / 1e9
    prior_total_bn <- sum(prior_summary$total, na.rm = TRUE) / 1e9

    p <- ggplot(combined, aes(x = bond, y = total_mil, fill = period)) +
        geom_col(position = position_dodge(width = 0.7), width = 0.6, alpha = 0.9) +
        geom_text(
            aes(label = paste0("R", format(round(total_mil), big.mark = ","), "m")),
            position = position_dodge(width = 0.7),
            hjust = -0.05, size = 2.5, fontface = "bold", color = "#333333"
        ) +
        scale_fill_manual(values = setNames(
            c("#1B3A6B", "#8BADD4"),
            c(ytd_label, prior_label)
        )) +
        scale_y_continuous(
            labels = function(x) paste0("R", format(x, big.mark = ","), "m"),
            expand = expansion(mult = c(0, 0.22))
        ) +
        coord_flip() +
        labs(
            title = "Cumulative Government Bond Issuance",
            subtitle = sprintf("%s: R%.1fbn  |  %s: R%.1fbn",
                               ytd_label, ytd_total_bn, prior_label, prior_total_bn),
            x = NULL, y = "Total Issuance (R millions)",
            caption = "Source: Insele Capital Partners Bond Analytics"
        ) +
        theme_minimal(base_size = 9) +
        theme(
            plot.title = element_text(face = "bold", size = 12, color = "#1B3A6B"),
            plot.subtitle = element_text(size = 8, color = "grey50"),
            plot.caption = element_text(size = 7, color = "#999", hjust = 1),
            legend.position = "bottom",
            legend.title = element_blank(),
            legend.text = element_text(size = 8),
            panel.grid.major.y = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.y = element_text(size = 8, face = "bold"),
            plot.background = element_rect(fill = "white", color = NA)
        )

    return(p)
}

#' @export
#' Generate Pre-Auction PDF Report (7-page enhanced version)
#' @param file Output file path
#' @param config Report configuration list (from report_config())
#' @param filtered_data Filtered bond data (FULL dataset - all bonds, all dates)
#' @param processed_data Processed bond data (latest snapshot per bond)
#' @param carry_data Carry and roll return data
#' @param logo_grob Grid graphics object for logo (or NULL)
generate_pre_auction_pdf <- function(file, config, filtered_data, processed_data,
                                     carry_data, logo_grob) {
    require(gridExtra)
    require(grid)

    temp_dir <- tempdir()
    temp_pdf <- file.path(temp_dir, paste0("pre_auction_", Sys.getpid(), ".pdf"))
    total_pages <- 9
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

    # Helper: draw page header bar
    draw_page_header <- function(title) {
        grid.rect(x = 0.5, y = 0.96, width = 1, height = 0.06,
                  gp = gpar(fill = "#1B3A6B", col = NA))
        grid.text(title, x = 0.5, y = 0.96,
                  gp = gpar(fontsize = 16, fontface = "bold", col = "white"))
    }

    # Helper: draw placeholder for unavailable content
    draw_placeholder <- function(msg, y = 0.5) {
        grid.rect(x = 0.5, y = y, width = 0.6, height = 0.08,
                  gp = gpar(fill = "#f8f9fa", col = "#dddddd"))
        grid.text(msg, x = 0.5, y = y,
                  gp = gpar(fontsize = 12, col = "#999999"))
    }

    # ══════════════════════════════════════════════════════════════════
    # PRE-RENDER all plots to grobs BEFORE opening PDF device
    # ══════════════════════════════════════════════════════════════════
    message("[PRE-AUCTION PDF] Pre-rendering plots...")

    # Helper to safely render a ggplot to a rasterGrob
    safe_render_plot <- function(plot_expr, label = "plot", width = 10, height = 6) {
        tryCatch({
            p <- eval(plot_expr)
            if (is.null(p)) return(NULL)
            if ("ggplot" %in% class(p)) {
                temp_png <- tempfile(fileext = ".png")
                on.exit(unlink(temp_png), add = TRUE)
                ggsave(temp_png, p, width = width, height = height, dpi = 150, bg = "white")
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

    # 1. Bond Snapshot table grob (Page 3)
    snapshot_grob <- tryCatch({
        tbl_data <- if (!is.null(filtered_data) && nrow(filtered_data) > 0) filtered_data
                    else if (!is.null(processed_data) && nrow(processed_data) > 0) processed_data
                    else NULL
        if (!is.null(tbl_data)) generate_bond_snapshot_grob(tbl_data, auction_bonds) else NULL
    }, error = function(e) { message(sprintf("[PRE-AUCTION PDF] Snapshot error: %s", e$message)); NULL })

    # 2. Forecast table (needed for outlook + page 7)
    forecast_tbl <- tryCatch({
        if (!is.null(filtered_data) && nrow(filtered_data) > 0)
            generate_enhanced_forecast_table(filtered_data, auction_bonds) else NULL
    }, error = function(e) { message(sprintf("[PRE-AUCTION PDF] Forecast error: %s", e$message)); NULL })

    # 3. Forecast table grob (Page 7)
    forecast_grob <- tryCatch({
        if (!is.null(filtered_data) && nrow(filtered_data) > 0)
            generate_enhanced_forecast_grob(filtered_data, auction_bonds) else NULL
    }, error = function(e) { message(sprintf("[PRE-AUCTION PDF] Forecast grob error: %s", e$message)); NULL })

    # 4. Outlook narrative + KPI parts (Page 2)
    outlook_text <- tryCatch(
        generate_auction_outlook_text(filtered_data, auction_bonds, forecast_tbl),
        error = function(e) "Auction outlook generation in progress."
    )
    outlook_parts <- tryCatch(
        generate_outlook_parts(filtered_data, auction_bonds, forecast_tbl),
        error = function(e) list()
    )

    # 5. Recent auction history grob (Page 2)
    recent_history_grob <- tryCatch(
        generate_recent_auction_history_grob(filtered_data, auction_bonds, n_recent = 4),
        error = function(e) NULL
    )

    # 6. Yield curve context (Page 4)
    yield_curve_grob <- safe_render_plot(
        quote(generate_auction_yield_curve(filtered_data, auction_bonds)),
        "yield_curve", width = 10, height = 6
    )

    # 7. Auction performance history (Page 5)
    auction_perf_grob <- safe_render_plot(
        quote(generate_enhanced_auction_analytics(filtered_data, list(selected_bonds = auction_bonds))),
        "auction_performance"
    )

    # 8. Historical auction patterns (Page 6 - full page)
    auction_patterns_grob <- safe_render_plot(
        quote(generate_auction_pattern_analysis(filtered_data, list(), selected_bonds = auction_bonds)),
        "auction_patterns", width = 10, height = 7
    )

    # 8b. Supply & demand bid distribution (Page 7 - full page)
    bid_dist_grob <- safe_render_plot(
        quote(generate_bid_distribution_plot(filtered_data, list(), selected_bonds = auction_bonds)),
        "bid_distribution", width = 10, height = 7
    )

    # 9. BTC history line chart (Page 8)
    btc_history_grob <- safe_render_plot(
        quote(generate_auction_btc_history_chart(filtered_data, auction_bonds)),
        "btc_history", width = 10, height = 4
    )

    # 10. Dual Issuance charts — YTD + Last 12 Months (Page 9)
    ytd_issuance_grob <- tryCatch({
        g <- generate_dual_issuance_charts(filtered_data)
        if (!is.null(g)) {
            # generate_dual_issuance_charts returns an arrangeGrob (gtable/grob)
            # Render it to a rasterGrob via PNG for consistent PDF output
            temp_png <- tempfile(fileext = ".png")
            on.exit(unlink(temp_png), add = TRUE)
            png(temp_png, width = 10 * 150, height = 7 * 150, res = 150, bg = "white")
            grid::grid.draw(g)
            dev.off()
            img <- png::readPNG(temp_png)
            grid::rasterGrob(img, interpolate = TRUE)
        } else {
            NULL
        }
    }, error = function(e) {
        message(sprintf("[PRE-AUCTION PDF] Failed to render dual_issuance: %s", e$message))
        NULL
    })

    message("[PRE-AUCTION PDF] Pre-rendering complete. Opening PDF device...")

    # ══════════════════════════════════════════════════════════════════
    # GENERATE PDF
    # ══════════════════════════════════════════════════════════════════
    tryCatch({
        pdf(temp_pdf, width = 11, height = 8.5)

        # ─── PAGE 1: COVER (Professional Branded Layout) ──────────────────
        grid.newpage()

        # --- Navy left panel (35% width, full height minus footer) ---
        grid.rect(x = unit(0.175, "npc"), y = unit(0.53, "npc"),
                  width = unit(0.35, "npc"), height = unit(0.94, "npc"),
                  gp = gpar(fill = "#1B3A6B", col = NA))

        # --- Decorative elements inside navy panel ---
        # Large partial circle anchored to bottom-left corner
        # Only the upper-right quadrant is visible; rest bleeds off-page
        grid.circle(x = unit(-0.02, "npc"), y = unit(0.08, "npc"),
                    r = unit(0.22, "npc"),
                    gp = gpar(fill = adjustcolor("#2B4F7F", alpha.f = 0.25), col = NA))

        # Thin horizontal orange accent line across the navy panel
        grid.lines(x = c(0.03, 0.32), y = c(0.15, 0.15),
                   gp = gpar(col = adjustcolor("#E8913A", alpha.f = 0.6), lwd = 3))

        # --- Logo (top of white area, left-aligned) ---
        if (!is.null(logo_grob)) {
            pushViewport(viewport(x = 0.42, y = 0.90, width = 0.42, height = 0.14,
                                  just = c("left", "center")))
            grid.draw(logo_grob)
            popViewport()
        } else {
            grid.text("INSELE CAPITAL PARTNERS",
                      x = 0.42, y = 0.89, just = "left",
                      gp = gpar(fontsize = 20, fontface = 2, col = "#1B3A6B"))
            grid.text("BROKING SERVICES",
                      x = 0.42, y = 0.85, just = "left",
                      gp = gpar(fontsize = 12, col = "#5B7B8A"))
        }

        # --- Orange accent line (separator below logo) ---
        grid.lines(x = c(0.38, 0.92), y = c(0.81, 0.81),
                   gp = gpar(col = "#E8913A", lwd = 3))

        # --- Title: WEEKLY AUCTION PREVIEW (stacked, bold, left-aligned) ---
        grid.text("WEEKLY", x = 0.42, y = 0.72, just = "left",
                  gp = gpar(fontsize = 36, fontface = 2, col = "#1B3A6B"))
        grid.text("AUCTION", x = 0.42, y = 0.64, just = "left",
                  gp = gpar(fontsize = 36, fontface = 2, col = "#1B3A6B"))
        grid.text("PREVIEW", x = 0.42, y = 0.56, just = "left",
                  gp = gpar(fontsize = 36, fontface = 2, col = "#1B3A6B"))

        # --- Auction date (in brand orange) ---
        grid.text(format(auction_date, "%A, %B %d, %Y"),
                  x = 0.42, y = 0.46, just = "left",
                  gp = gpar(fontsize = 16, col = "#E8913A"))

        # --- Bonds on auction ---
        grid.text(paste("Bonds on Auction:", paste(auction_bonds, collapse = ", ")),
                  x = 0.42, y = 0.38, just = "left",
                  gp = gpar(fontsize = 14, col = "#333333"))

        # --- Client name (conditional) ---
        if (nchar(client_name) > 0) {
            grid.text(paste("Prepared for:", client_name),
                      x = 0.42, y = 0.31, just = "left",
                      gp = gpar(fontsize = 13, fontface = 3, col = "#666666"))
        }

        # --- Navy footer bar (full width) ---
        grid.rect(x = unit(0.5, "npc"), y = unit(0.03, "npc"),
                  width = unit(1, "npc"), height = unit(0.06, "npc"),
                  gp = gpar(fill = "#1B3A6B", col = NA))

        # Footer contact details (white text on navy)
        grid.text("www.insele.capital",
                  x = 0.10, y = 0.03, just = "left",
                  gp = gpar(fontsize = 9, col = "white"))
        grid.text("+27 11 286 1949",
                  x = 0.35, y = 0.03,
                  gp = gpar(fontsize = 9, col = "white"))
        grid.text("bonds@insele.capital",
                  x = 0.60, y = 0.03,
                  gp = gpar(fontsize = 9, col = "white"))
        grid.text("Prepared by: Insele Capital Partners",
                  x = 0.90, y = 0.03, just = "right",
                  gp = gpar(fontsize = 9, col = "white"))

        # Note: No add_footer() call on cover — branded footer bar replaces it

        # ─── PAGE 2: AUCTION OUTLOOK (Enhancement 1) ──────────────
        grid.newpage()
        draw_page_header("Auction Outlook")

        # KPI Cards for each auction bond
        if (length(outlook_parts) > 0) {
            n_cards <- length(outlook_parts)
            card_width <- min(0.28, 0.85 / n_cards)
            card_gap <- (0.85 - n_cards * card_width) / max(1, n_cards - 1)
            start_x <- 0.075 + card_width / 2

            for (i in seq_along(outlook_parts)) {
                o <- outlook_parts[[i]]
                cx <- start_x + (i - 1) * (card_width + card_gap)
                cy <- 0.84

                # Border color by signal
                border_col <- switch(o$signal,
                                     "STRONG BUY" = "#388E3C",
                                     "BUY" = "#1565C0",
                                     "HOLD" = "#FF8F00",
                                     "CAUTION" = "#C62828",
                                     "#999999")

                grid.rect(x = cx, y = cy, width = card_width, height = 0.12,
                          gp = gpar(fill = "white", col = border_col, lwd = 2))
                grid.text(o$bond, x = cx, y = cy + 0.04,
                          gp = gpar(fontsize = 12, fontface = "bold", col = "#1B3A6B"))
                grid.text(paste("YTM:", o$yield), x = cx, y = cy + 0.01,
                          gp = gpar(fontsize = 9, col = "#555555"))
                grid.text(paste("Pred:", o$pred), x = cx, y = cy - 0.02,
                          gp = gpar(fontsize = 9, col = "#555555"))
                grid.text(o$signal, x = cx, y = cy - 0.045,
                          gp = gpar(fontsize = 10, fontface = "bold", col = border_col))
            }
        }

        # Outlook narrative text — wrap each line to fit the page width
        outlook_lines <- strsplit(outlook_text, "\n")[[1]]
        wrapped_lines <- sapply(outlook_lines, function(line) {
            if (nchar(trimws(line)) > 0) {
                paste(strwrap(line, width = 105), collapse = "\n")
            } else {
                ""
            }
        }, USE.NAMES = FALSE)
        wrapped_outlook <- paste(wrapped_lines, collapse = "\n")

        grid.text(wrapped_outlook,
                  x = 0.06, y = 0.68, hjust = 0, vjust = 1,
                  gp = gpar(fontsize = 9, lineheight = 1.4, col = "#333333",
                            fontfamily = "mono"))

        # Recent auction history table (bottom of page)
        if (!is.null(recent_history_grob)) {
            grid.text("Recent Auction History", x = 0.08, y = 0.32, hjust = 0,
                      gp = gpar(fontsize = 12, fontface = "bold", col = "#1B3A6B"))
            pushViewport(viewport(x = 0.5, y = 0.17, width = 0.9, height = 0.26))
            grid.draw(recent_history_grob)
            popViewport()
        }

        add_footer(2, total_pages)

        # ─── PAGE 3: BOND SNAPSHOT TABLE (Fix 1) ──────────────────
        grid.newpage()
        draw_page_header("Bond Snapshot - Current Market Data")

        if (!is.null(snapshot_grob)) {
            tryCatch({
                pushViewport(viewport(x = 0.5, y = 0.5, width = 0.9, height = 0.8))
                grid.draw(snapshot_grob)
                popViewport()
            }, error = function(e) {
                draw_placeholder("Bond snapshot table unavailable")
            })
        } else {
            draw_placeholder("Bond snapshot data unavailable")
        }

        add_footer(3, total_pages)

        # ─── PAGE 4: RELATIVE VALUE CONTEXT (Enhancement 5) ───────
        grid.newpage()
        draw_page_header("Relative Value Context")

        if (!is.null(yield_curve_grob)) {
            pushViewport(viewport(x = 0.5, y = 0.47, width = 0.92, height = 0.82))
            grid.draw(yield_curve_grob)
            popViewport()
        } else {
            draw_placeholder("Yield curve chart unavailable")
        }

        add_footer(4, total_pages)

        # ─── PAGE 5: AUCTION PERFORMANCE HISTORY ───────────────────
        grid.newpage()
        draw_page_header("Auction Performance History")

        if (!is.null(auction_perf_grob)) {
            pushViewport(viewport(x = 0.5, y = 0.47, width = 0.92, height = 0.82))
            grid.draw(auction_perf_grob)
            popViewport()
        } else {
            draw_placeholder("Auction performance chart unavailable")
        }

        add_footer(5, total_pages)

        # ─── PAGE 6: HISTORICAL AUCTION PATTERNS (Full Page) ──────────
        grid.newpage()
        draw_page_header("Historical Auction Patterns")

        if (!is.null(auction_patterns_grob)) {
            pushViewport(viewport(x = 0.5, y = 0.47, width = 0.92, height = 0.82))
            grid.draw(auction_patterns_grob)
            popViewport()
        } else {
            draw_placeholder("Historical auction patterns chart unavailable")
        }

        add_footer(6, total_pages)

        # ─── PAGE 7: SUPPLY & DEMAND (Full Page) ──────────
        grid.newpage()
        draw_page_header("Supply & Demand")

        if (!is.null(bid_dist_grob)) {
            pushViewport(viewport(x = 0.5, y = 0.47, width = 0.92, height = 0.82))
            grid.draw(bid_dist_grob)
            popViewport()
        } else {
            draw_placeholder("Supply & demand chart unavailable")
        }

        add_footer(7, total_pages)

        # ─── PAGE 8: FORECAST ─
        grid.newpage()
        draw_page_header("Forecast")

        # Top section: Enhanced forecast table with signals
        if (!is.null(forecast_grob)) {
            tryCatch({
                grid.text("ARIMA Bid-to-Cover Forecast",
                          x = 0.5, y = 0.90,
                          gp = gpar(fontsize = 13, fontface = "bold", col = "#1B3A6B"))
                pushViewport(viewport(x = 0.5, y = 0.78, width = 0.92, height = 0.22))
                grid.draw(forecast_grob)
                popViewport()
            }, error = function(e) {
                draw_placeholder("Forecast table unavailable", y = 0.78)
            })
        } else {
            draw_placeholder("Forecast data unavailable", y = 0.78)
        }

        # Bottom section: BTC history line chart (expanded)
        if (!is.null(btc_history_grob)) {
            pushViewport(viewport(x = 0.5, y = 0.40, width = 0.92, height = 0.55))
            grid.draw(btc_history_grob)
            popViewport()
        } else {
            draw_placeholder("BTC history chart unavailable", y = 0.40)
        }

        add_footer(8, total_pages)

        # ─── PAGE 9: CUMULATIVE ISSUANCE ──────────────────────────────
        grid.newpage()
        draw_page_header("Cumulative Government Bond Issuance")

        if (!is.null(ytd_issuance_grob)) {
            pushViewport(viewport(x = 0.5, y = 0.47, width = 0.92, height = 0.82))
            grid.draw(ytd_issuance_grob)
            popViewport()
        } else {
            draw_placeholder("Issuance charts unavailable")
        }

        add_footer(9, total_pages)

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

    # Cleanup any lingering devices
    while (dev.cur() > 1) { try(dev.off(), silent = TRUE) }
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
    # HELPER: render chart to base64 HTML
    # ══════════════════════════════════════════════════════════════════
    render_chart_html <- function(plot_expr, title, label = "chart", width = 10, height = 6) {
        tryCatch({
            p <- eval(plot_expr)
            if (is.null(p)) return(sprintf('<p style="color: #999;">%s chart unavailable.</p>', title))
            b64 <- plot_to_base64(p, width = width, height = height, dpi = 150)
            if (is.null(b64)) return(sprintf('<p style="color: #999;">%s chart unavailable.</p>', title))
            sprintf('<div class="chart-container"><img src="data:image/png;base64,%s" alt="%s"/></div>',
                    b64, title)
        }, error = function(e) {
            message(sprintf("[PRE-AUCTION HTML] Chart error (%s): %s", label, e$message))
            sprintf('<p style="color: #999;">%s chart unavailable.</p>', title)
        })
    }

    # Helper: render data frame to HTML table with optional signal coloring
    render_table_html <- function(df, color_col = NULL) {
        if (is.null(df) || nrow(df) == 0) return('<p style="color: #999;">Data unavailable.</p>')
        headers <- paste(sprintf('<th>%s</th>', names(df)), collapse = "")
        rows_html <- paste(sapply(1:nrow(df), function(i) {
            row <- df[i, ]
            # Determine row background color
            bg <- if (!is.null(color_col) && color_col %in% names(df)) {
                val <- as.character(df[[color_col]][i])
                switch(val,
                       "STRONG BUY" = "#E8F5E9", "BUY" = "#E3F2FD",
                       "HOLD" = "#FFF3E0", "CAUTION" = "#FFEBEE", "WEAK" = "#FFEBEE",
                       "Strong" = "#E8F5E9", "Good" = "#E3F2FD",
                       "Normal" = "#FFF3E0", "Weak" = "#FFEBEE",
                       if (i %% 2 == 0) "#f8f9fa" else "white")
            } else {
                if (i %% 2 == 0) "#f8f9fa" else "white"
            }
            cells <- paste(sprintf('<td>%s</td>', row), collapse = "")
            sprintf('<tr style="background: %s;">%s</tr>', bg, cells)
        }), collapse = "\n")
        sprintf('<table><thead><tr>%s</tr></thead><tbody>%s</tbody></table>', headers, rows_html)
    }

    # ══════════════════════════════════════════════════════════════════
    # GENERATE DATA
    # ══════════════════════════════════════════════════════════════════

    # Enhanced forecast table with Signal column
    forecast_tbl <- tryCatch({
        if (!is.null(filtered_data) && nrow(filtered_data) > 0)
            generate_enhanced_forecast_table(filtered_data, auction_bonds)
        else NULL
    }, error = function(e) NULL)

    # KPI cards with signal info
    outlook_parts <- tryCatch(
        generate_outlook_parts(filtered_data, auction_bonds, forecast_tbl),
        error = function(e) list()
    )

    # Outlook narrative
    outlook_text <- tryCatch(
        generate_auction_outlook_text(filtered_data, auction_bonds, forecast_tbl),
        error = function(e) "Auction outlook generation in progress."
    )

    # Bond snapshot table
    snapshot_tbl <- tryCatch({
        tbl_data <- if (!is.null(filtered_data) && nrow(filtered_data) > 0) filtered_data
                    else if (!is.null(processed_data) && nrow(processed_data) > 0) processed_data
                    else NULL
        if (!is.null(tbl_data)) generate_bond_snapshot_table(tbl_data, auction_bonds) else NULL
    }, error = function(e) NULL)

    # Recent auction history
    recent_history <- tryCatch(
        generate_recent_auction_history(filtered_data, auction_bonds, n_recent = 5),
        error = function(e) NULL
    )

    # ══════════════════════════════════════════════════════════════════
    # BUILD KPI CARDS HTML
    # ══════════════════════════════════════════════════════════════════
    kpi_html <- ""
    tryCatch({
        if (length(outlook_parts) > 0) {
            kpi_cards <- lapply(outlook_parts, function(o) {
                border_col <- switch(o$signal,
                                     "STRONG BUY" = "#388E3C", "BUY" = "#1565C0",
                                     "HOLD" = "#FF8F00", "CAUTION" = "#C62828", "#1B3A6B")
                sprintf(
                    '<div class="kpi-card" style="border-top: 3px solid %s;">
                        <div style="font-size: 14px; font-weight: bold; color: #1B3A6B; margin-bottom: 8px;">%s</div>
                        <div class="kpi-value">%s</div>
                        <div class="kpi-label">Current YTM</div>
                        <div style="margin-top: 8px; font-size: 16px; font-weight: bold; color: #333;">%s</div>
                        <div class="kpi-label">Predicted BTC</div>
                        <div style="margin-top: 8px; font-size: 14px; font-weight: bold; color: %s;">%s</div>
                    </div>', border_col, o$bond, o$yield, o$pred, border_col, o$signal)
            })
            kpi_html <- paste(kpi_cards, collapse = "\n")
        }
    }, error = function(e) { kpi_html <<- "" })

    # ══════════════════════════════════════════════════════════════════
    # RENDER CHARTS
    # ══════════════════════════════════════════════════════════════════

    # Yield curve context
    yield_curve_chart <- render_chart_html(
        quote(generate_auction_yield_curve(filtered_data, auction_bonds)),
        "Yield Curve Context", "yield_curve"
    )

    # Auction performance
    perf_chart <- render_chart_html(
        quote(generate_enhanced_auction_analytics(filtered_data, list(selected_bonds = auction_bonds))),
        "Auction Performance History", "auction_perf"
    )

    # Historical patterns
    patterns_chart <- render_chart_html(
        quote(generate_auction_pattern_analysis(filtered_data, list(), selected_bonds = auction_bonds)),
        "Historical Auction Patterns", "auction_patterns"
    )

    # Bid distribution (kept as fallback)
    bid_dist_chart <- render_chart_html(
        quote(generate_bid_distribution_plot(filtered_data, list(), selected_bonds = auction_bonds)),
        "Bid Distribution", "bid_dist"
    )

    # BTC history line chart
    btc_history_chart <- render_chart_html(
        quote(generate_auction_btc_history_chart(filtered_data, auction_bonds)),
        "BTC Historical Performance", "btc_history"
    )

    # Dual Issuance charts (YTD + Last 12 Months)
    ytd_issuance_chart <- tryCatch({
        g <- generate_dual_issuance_charts(filtered_data)
        if (!is.null(g)) {
            # Render arrangeGrob to base64 PNG
            temp_png <- tempfile(fileext = ".png")
            on.exit(unlink(temp_png), add = TRUE)
            png(temp_png, width = 10 * 150, height = 7 * 150, res = 150, bg = "white")
            grid::grid.draw(g)
            dev.off()
            b64 <- base64enc::base64encode(temp_png)
            sprintf('<div class="chart-container"><img src="data:image/png;base64,%s" alt="Cumulative Issuance"/></div>', b64)
        } else {
            '<p style="color: #999;">Cumulative issuance charts unavailable.</p>'
        }
    }, error = function(e) {
        message(sprintf("[PRE-AUCTION HTML] Dual issuance chart error: %s", e$message))
        '<p style="color: #999;">Cumulative issuance charts unavailable.</p>'
    })

    # ══════════════════════════════════════════════════════════════════
    # BUILD TABLE HTML
    # ══════════════════════════════════════════════════════════════════
    snapshot_html <- render_table_html(snapshot_tbl)
    forecast_html <- render_table_html(forecast_tbl, color_col = "Signal")
    recent_history_html <- render_table_html(recent_history, color_col = "Performance")

    # Outlook text as HTML paragraphs
    outlook_html <- paste(
        sprintf('<p style="line-height: 1.6;">%s</p>',
                gsub("\n\n", "</p><p style='line-height: 1.6;'>",
                     gsub("\n", "<br/>", htmltools::htmlEscape(outlook_text)))),
        collapse = ""
    )

    # ══════════════════════════════════════════════════════════════════
    # ASSEMBLE HTML (9-section structure matching PDF)
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
        .kpi-row { display: flex; gap: 15px; margin: 20px 0; flex-wrap: wrap; }
        .kpi-card { flex: 1; min-width: 150px; background: white; border: 1px solid #e0e0e0; border-radius: 8px; padding: 15px; text-align: center; }
        .kpi-value { font-size: 24px; font-weight: bold; color: #1B3A6B; }
        .kpi-label { font-size: 12px; color: #666; margin-top: 4px; }
        .disclaimer { background: #fff3cd; padding: 15px; border: 1px solid #ffc107; border-radius: 5px; margin: 20px 40px; }
        .signal-strong-buy { color: #388E3C; font-weight: bold; }
        .signal-buy { color: #1565C0; font-weight: bold; }
        .signal-hold { color: #FF8F00; font-weight: bold; }
        .signal-caution { color: #C62828; font-weight: bold; }
    </style>
</head>
<body>
    <div class="header">
        <h1>Weekly Auction Preview</h1>
        <div class="subtitle">%s | Bonds: %s</div>
        <div class="subtitle">%s Generated: %s</div>
    </div>

    <!-- Section 1: Auction Outlook -->
    <div class="section">
        <h2>Auction Outlook</h2>
        <div class="kpi-row">
            %s
        </div>
        %s
        <h3 style="color: #1B3A6B; margin-top: 25px;">Recent Auction History</h3>
        %s
    </div>

    <!-- Section 2: Bond Snapshot -->
    <div class="section">
        <h2>Bond Snapshot - Current Market Data</h2>
        %s
    </div>

    <!-- Section 3: Relative Value Context -->
    <div class="section">
        <h2>Relative Value Context</h2>
        %s
    </div>

    <!-- Section 4: Auction Performance History -->
    <div class="section">
        <h2>Auction Performance History</h2>
        %s
    </div>

    <!-- Section 5: Historical Auction Patterns -->
    <div class="section">
        <h2>Historical Auction Patterns</h2>
        %s
    </div>

    <!-- Section 6: Supply & Demand -->
    <div class="section">
        <h2>Supply &amp; Demand</h2>
        %s
    </div>

    <!-- Section 7: Forecast -->
    <div class="section">
        <h2>Forecast</h2>
        <h3 style="color: #1B3A6B;">ARIMA Bid-to-Cover Forecast</h3>
        %s
        %s
    </div>

    <!-- Section 8: Cumulative Issuance -->
    <div class="section">
        <h2>Cumulative Government Bond Issuance</h2>
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
        outlook_html,
        recent_history_html,
        snapshot_html,
        yield_curve_chart,
        perf_chart,
        patterns_chart,
        bid_dist_chart,
        forecast_html,
        btc_history_chart,
        ytd_issuance_chart,
        format(Sys.Date(), "%%Y")
    )

    return(html)
}


# ================================================================================
# .EML EMAIL DRAFT FUNCTIONS (Pre-Auction Reports)
# ================================================================================

#' Build HTML email body with CID image references for Outlook rendering
#'
#' @param page_labels Character vector of labels for each page. NULL uses defaults.
#' @param n_pages Number of pages in the PDF report
#' @param auction_bonds Character vector of bond names on auction
#' @param auction_date Date of the auction
#' @param client_name Client name for header
#' @param primary_color Navy color for header/footer
#' @param accent_color Orange accent color
#' @param tagline Company tagline
#' @return Character string of HTML email body
build_eml_email_html <- function(page_labels, n_pages, auction_bonds, auction_date,
                                  client_name = "Insele Capital Partners",
                                  primary_color = "#1B3A6B",
                                  accent_color = "#E8913A",
                                  tagline = "The Power of Partnership") {

    # Default page labels matching the 9-page report structure
    default_page_labels <- c(
        "Cover Page", "Auction Outlook", "Bond Snapshot",
        "Relative Value Context", "Auction Performance History",
        "Supply & Demand Analysis", "Forecast",
        "Cumulative Issuance"
    )

    if (is.null(page_labels)) {
        page_labels <- default_page_labels
    }

    # Pad labels if n_pages exceeds the list
    if (n_pages > length(page_labels)) {
        extra <- n_pages - length(page_labels)
        page_labels <- c(page_labels, paste("Page", seq(length(page_labels) + 1, n_pages)))
    }

    # Truncate if fewer pages than labels
    if (n_pages < length(page_labels)) {
        page_labels <- page_labels[seq_len(n_pages)]
    }

    # Pre-compute ALL formatted dates before any sprintf() calls
    auction_date_long <- format(auction_date, "%A, %d %B %Y")
    auction_date_short <- format(auction_date, "%d %B %Y")
    current_year <- format(Sys.Date(), "%Y")
    bonds_str <- paste(auction_bonds, collapse = ", ")

    # Build chart sections
    chart_sections <- ""
    for (i in seq_len(n_pages)) {
        cid_ref <- sprintf("chart%02d", i)
        label <- page_labels[i]
        chart_sections <- paste0(chart_sections, sprintf(
            '<tr><td style="padding: 15px 30px 5px 30px; font-family: Arial, Helvetica, sans-serif; font-size: 16px; font-weight: bold; color: %s;">%s</td></tr>
<tr><td style="padding: 5px 30px 15px 30px; text-align: center;"><img src="cid:%s" width="840" style="display: block; margin: 0 auto; max-width: 840px;" /></td></tr>
',
            primary_color, label, cid_ref
        ))
    }

    # Assemble complete HTML (no DOCTYPE - can cause Outlook issues)
    html <- sprintf(
'<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
</head>
<body style="margin: 0; padding: 0; background-color: #f4f4f4; font-family: Arial, Helvetica, sans-serif;">
<table width="900" align="center" cellpadding="0" cellspacing="0" border="0" style="margin: 0 auto; background-color: #ffffff;">
<!-- Header Bar -->
<tr>
<td style="background-color: %s; padding: 20px 30px;">
<table width="100%%" cellpadding="0" cellspacing="0" border="0">
<tr>
<td style="font-family: Arial, Helvetica, sans-serif; font-size: 24px; font-weight: bold; color: #ffffff;">%s</td>
<td style="font-family: Arial, Helvetica, sans-serif; font-size: 13px; color: #cccccc; text-align: right;">%s<br/>%s</td>
</tr>
<tr>
<td colspan="2" style="font-family: Arial, Helvetica, sans-serif; font-size: 16px; color: %s; padding-top: 4px;">Pre-Auction Report</td>
</tr>
</table>
</td>
</tr>
<!-- Intro Paragraph -->
<tr>
<td style="padding: 20px 30px; background-color: #f8f9fa;">
<table width="100%%" cellpadding="0" cellspacing="0" border="0">
<tr>
<td style="font-family: Arial, Helvetica, sans-serif; font-size: 14px; color: #333333; line-height: 1.5;">
Pre-Auction analysis for <strong>%s</strong> scheduled for <strong>%s</strong>.
This report contains yield curve analysis, relative value context, auction performance history,
supply and demand dynamics, and forecast data to support auction preparation.
</td>
</tr>
</table>
</td>
</tr>
<!-- Chart Sections -->
%s
<!-- Footer -->
<tr>
<td style="background-color: %s; padding: 20px 30px;">
<table width="100%%" cellpadding="0" cellspacing="0" border="0">
<tr>
<td style="font-family: Arial, Helvetica, sans-serif; font-size: 12px; color: #ffffff; text-align: center;">
&copy; %s %s &nbsp;|&nbsp; <span style="color: %s;">%s</span><br/>
<span style="font-size: 11px; color: #aaaaaa;">This report is confidential and intended solely for the recipient. Do not distribute without authorisation.</span>
</td>
</tr>
</table>
</td>
</tr>
</table>
</body>
</html>',
        primary_color,                  # Header bg
        client_name,                    # Header left: client name
        auction_date_short,             # Header right: date
        bonds_str,                      # Header right: bonds
        accent_color,                   # "Pre-Auction Report" subtitle color
        bonds_str,                      # Intro: bonds
        auction_date_long,              # Intro: auction date
        chart_sections,                 # All chart sections
        primary_color,                  # Footer bg
        current_year,                   # Footer: year
        client_name,                    # Footer: client name
        accent_color,                   # Footer: tagline color
        tagline                         # Footer: tagline
    )

    return(html)
}


#' Assemble a complete MIME .eml file structure
#'
#' @param html_body Character string of the HTML email body
#' @param png_paths Character vector of paths to PNG files (for filename references)
#' @param png_base64_list List of base64-encoded PNG strings
#' @param pdf_path Path to the PDF attachment
#' @param auction_bonds Character vector of bond names
#' @param auction_date Date of the auction
#' @param subject_prefix Subject line prefix
#' @return Character vector of .eml lines (one element per line)
build_eml_file <- function(html_body, png_paths, png_base64_list, pdf_path,
                            auction_bonds, auction_date,
                            subject_prefix = "Insele Pre-Auction Report",
                            pdf_attachment_name = NULL) {

    # Helper: split base64 into 76-char lines (MIME standard)
    split_b64 <- function(b64_string) {
        n <- nchar(b64_string)
        starts <- seq(1, n, 76)
        ends <- pmin(starts + 75, n)
        substring(b64_string, starts, ends)
    }

    # Pre-compute formatted values
    auction_date_fmt <- format(auction_date, "%d %B %Y")
    bonds_str <- paste(auction_bonds, collapse = ", ")
    subject <- if (!is.null(auction_bonds) && length(auction_bonds) > 0) {
        sprintf("%s: %s - %s", subject_prefix, bonds_str, auction_date_fmt)
    } else {
        sprintf("%s - %s", subject_prefix, auction_date_fmt)
    }
    rfc2822_date <- format(Sys.time(), "%a, %d %b %Y %H:%M:%S %z")

    # Unique boundary strings
    ts_str <- format(Sys.time(), "%Y%m%d%H%M%S")
    boundary_mixed <- paste0("_=_mixed_boundary_", ts_str, "_001_=_")
    boundary_related <- paste0("_=_related_boundary_", ts_str, "_002_=_")

    # PDF attachment filename (use custom name if provided, else default)
    if (is.null(pdf_attachment_name)) {
        pdf_filename <- sprintf("Insele_Pre_Auction_Report_%s.pdf", format(auction_date, "%Y%m%d"))
    } else {
        pdf_filename <- pdf_attachment_name
    }

    # Base64-encode the PDF
    pdf_b64 <- base64enc::base64encode(pdf_path)

    # ── Build .eml lines ──────────────────────────────────────────
    lines <- character()

    # RFC 2822 headers
    lines <- c(lines,
        "MIME-Version: 1.0",
        paste0("X-Unsent: 1"),
        paste0("Date: ", rfc2822_date),
        "From: ",
        "To: ",
        paste0("Subject: ", subject),
        paste0("Content-Type: multipart/mixed; boundary=\"", boundary_mixed, "\""),
        ""
    )

    # ── multipart/mixed top level ──
    lines <- c(lines,
        paste0("--", boundary_mixed),
        paste0("Content-Type: multipart/related; boundary=\"", boundary_related, "\""),
        ""
    )

    # ── multipart/related: HTML body ──
    lines <- c(lines,
        paste0("--", boundary_related),
        "Content-Type: text/html; charset=\"UTF-8\"",
        "Content-Transfer-Encoding: 7bit",
        "",
        html_body,
        ""
    )

    # ── multipart/related: inline images ──
    n_pages <- length(png_base64_list)
    for (i in seq_len(n_pages)) {
        cid <- sprintf("chart%02d", i)
        b64_lines <- split_b64(png_base64_list[[i]])

        lines <- c(lines,
            paste0("--", boundary_related),
            "Content-Type: image/png",
            "Content-Transfer-Encoding: base64",
            paste0("Content-ID: <", cid, ">"),
            "Content-Disposition: inline",
            "",
            b64_lines,
            ""
        )
    }

    # Close multipart/related
    lines <- c(lines,
        paste0("--", boundary_related, "--"),
        ""
    )

    # ── PDF attachment ──
    pdf_b64_lines <- split_b64(pdf_b64)
    lines <- c(lines,
        paste0("--", boundary_mixed),
        "Content-Type: application/pdf",
        "Content-Transfer-Encoding: base64",
        paste0("Content-Disposition: attachment; filename=\"", pdf_filename, "\""),
        "",
        pdf_b64_lines,
        ""
    )

    # Close multipart/mixed
    lines <- c(lines,
        paste0("--", boundary_mixed, "--")
    )

    return(lines)
}


# ================================================================================
# TREASURY HOLDINGS PDF GENERATOR
# ================================================================================

#' Generate a branded multi-page landscape PDF of treasury holdings analysis
#'
#' @param file Output file path
#' @param config Report configuration list from report_config()
#' @param treasury_ts Holdings time series data (or NULL)
#' @param treasury_bonds Bond-level holdings data (or NULL)
#' @param logo_grob Logo rasterGrob (or NULL)
generate_treasury_holdings_pdf <- function(file, config, treasury_ts, treasury_bonds, logo_grob) {
    require(gridExtra)
    require(grid)

    temp_dir <- tempdir()
    temp_pdf <- file.path(temp_dir, paste0("treasury_holdings_", Sys.getpid(), ".pdf"))
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

    # Helper: draw page header bar
    draw_page_header <- function(title) {
        grid.rect(x = 0.5, y = 0.96, width = 1, height = 0.06,
                  gp = gpar(fill = "#1B3A6B", col = NA))
        grid.text(title, x = 0.5, y = 0.96,
                  gp = gpar(fontsize = 16, fontface = 2, col = "white"))
    }

    # Helper: draw placeholder for unavailable content
    draw_placeholder <- function(msg, y = 0.5) {
        grid.rect(x = 0.5, y = y, width = 0.6, height = 0.08,
                  gp = gpar(fill = "#f8f9fa", col = "#dddddd"))
        grid.text(msg, x = 0.5, y = y,
                  gp = gpar(fontsize = 12, col = "#999999"))
    }

    # Helper: safely render a ggplot to a rasterGrob via temp PNG
    render_chart_to_grob <- function(chart_expr, width = 10, height = 6, dpi = 150) {
        tryCatch({
            p <- chart_expr
            if (is.null(p)) return(NULL)
            temp_png <- tempfile(fileext = ".png")
            ggsave(temp_png, p, width = width, height = height, dpi = dpi, bg = "white")
            img <- png::readPNG(temp_png)
            grob <- grid::rasterGrob(img, interpolate = TRUE)
            unlink(temp_png)
            return(grob)
        }, error = function(e) {
            message(sprintf("[TREASURY PDF] Chart render error: %s", e$message))
            return(NULL)
        })
    }

    # ══════════════════════════════════════════════════════════════════
    # PRE-RENDER all charts BEFORE opening PDF device
    # ══════════════════════════════════════════════════════════════════
    message("[TREASURY PDF] Pre-rendering charts...")

    # Track which pages will be generated (cover always included)
    pages <- list()

    # Page 1: Cover (always)
    pages[[length(pages) + 1]] <- list(type = "cover")

    # Pages from treasury_ts data
    if (!is.null(treasury_ts) && nrow(treasury_ts) > 0) {
        # Page 2: Aggregate Holdings Area Chart
        area_grob <- render_chart_to_grob(
            generate_holdings_area_chart(treasury_ts),
            width = 12, height = 7
        )
        pages[[length(pages) + 1]] <- list(
            type = "chart", title = "Aggregate Holdings Over Time",
            grob = area_grob
        )

        # Page 3: Top Sector Trend
        top_sector <- tryCatch({
            treasury_ts %>%
                filter(date == max(date)) %>%
                arrange(desc(percentage)) %>%
                slice(1) %>%
                pull(sector)
        }, error = function(e) "Non-residents")

        sector_grob <- render_chart_to_grob(
            generate_sector_trend_chart(treasury_ts, sector_name = top_sector),
            width = 10, height = 6
        )
        pages[[length(pages) + 1]] <- list(
            type = "chart",
            title = sprintf("Sector Trend Analysis - %s", top_sector),
            grob = sector_grob
        )

        # Page 4: Ownership Changes
        ownership_grob <- render_chart_to_grob(
            generate_ownership_change_chart(treasury_ts, periods = c(1, 3, 12)),
            width = 12, height = 7
        )
        pages[[length(pages) + 1]] <- list(
            type = "chart", title = "Ownership Changes (1m, 3m, 12m)",
            grob = ownership_grob
        )
    }

    # Pages from treasury_bonds data
    if (!is.null(treasury_bonds) && nrow(treasury_bonds) > 0) {
        # Check which bond types have data
        available_types <- unique(treasury_bonds$bond_type)

        if ("Fixed Rate" %in% available_types) {
            fixed_data <- treasury_bonds %>% filter(bond_type == "Fixed Rate")
            if (nrow(fixed_data) > 0) {
                # Page 5: Fixed Rate Bond Holdings
                fixed_bar_grob <- render_chart_to_grob(
                    generate_bond_holdings_bar_chart(treasury_bonds, "Fixed Rate"),
                    width = 10, height = 12
                )
                pages[[length(pages) + 1]] <- list(
                    type = "chart_tall", title = "Fixed Rate Bond Holdings",
                    grob = fixed_bar_grob
                )

                # Page 7: Fixed Rate Changes (3-month diverging)
                fixed_div_grob <- render_chart_to_grob(
                    generate_holdings_change_diverging(treasury_bonds, 3, "Fixed Rate", 12),
                    width = 10, height = 8
                )
                pages[[length(pages) + 1]] <- list(
                    type = "chart", title = "Fixed Rate Ownership Changes (3-Month)",
                    grob = fixed_div_grob
                )
            }
        }

        if ("ILB" %in% available_types) {
            ilb_data <- treasury_bonds %>% filter(bond_type == "ILB")
            if (nrow(ilb_data) > 0) {
                # Page 6: ILB Bond Holdings
                ilb_bar_grob <- render_chart_to_grob(
                    generate_bond_holdings_bar_chart(treasury_bonds, "ILB"),
                    width = 10, height = 12
                )
                pages[[length(pages) + 1]] <- list(
                    type = "chart_tall", title = "Inflation-Linked Bond Holdings",
                    grob = ilb_bar_grob
                )

                # Page 8: ILB Changes (3-month diverging)
                ilb_div_grob <- render_chart_to_grob(
                    generate_holdings_change_diverging(treasury_bonds, 3, "ILB", 12),
                    width = 10, height = 8
                )
                pages[[length(pages) + 1]] <- list(
                    type = "chart", title = "ILB Ownership Changes (3-Month)",
                    grob = ilb_div_grob
                )
            }
        }
    }

    total_pages <- length(pages)

    message(sprintf("[TREASURY PDF] Pre-rendering complete. %d pages to generate.", total_pages))

    # Determine data date range for the cover page
    data_date_range <- tryCatch({
        if (!is.null(treasury_ts) && nrow(treasury_ts) > 0) {
            dr <- range(treasury_ts$date, na.rm = TRUE)
            sprintf("%s to %s", format(dr[1], "%b %Y"), format(dr[2], "%b %Y"))
        } else if (!is.null(treasury_bonds) && nrow(treasury_bonds) > 0) {
            dr <- range(treasury_bonds$file_date, na.rm = TRUE)
            sprintf("%s to %s", format(dr[1], "%b %Y"), format(dr[2], "%b %Y"))
        } else {
            format(Sys.Date(), "%B %Y")
        }
    }, error = function(e) format(Sys.Date(), "%B %Y"))

    # ══════════════════════════════════════════════════════════════════
    # GENERATE PDF
    # ══════════════════════════════════════════════════════════════════
    tryCatch({
        pdf(temp_pdf, width = 11, height = 8.5)

        for (pg_idx in seq_along(pages)) {
            pg <- pages[[pg_idx]]

            if (pg$type == "cover") {
                # ─── COVER PAGE (same style as Pre-Auction) ──────────────────
                grid.newpage()

                # Navy left panel (35% width)
                grid.rect(x = unit(0.175, "npc"), y = unit(0.53, "npc"),
                          width = unit(0.35, "npc"), height = unit(0.94, "npc"),
                          gp = gpar(fill = "#1B3A6B", col = NA))

                # Decorative circle
                grid.circle(x = unit(-0.02, "npc"), y = unit(0.08, "npc"),
                            r = unit(0.22, "npc"),
                            gp = gpar(fill = adjustcolor("#2B4F7F", alpha.f = 0.25), col = NA))

                # Orange accent line in navy panel
                grid.lines(x = c(0.03, 0.32), y = c(0.15, 0.15),
                           gp = gpar(col = adjustcolor("#E8913A", alpha.f = 0.6), lwd = 3))

                # Logo
                if (!is.null(logo_grob)) {
                    pushViewport(viewport(x = 0.42, y = 0.90, width = 0.42, height = 0.14,
                                          just = c("left", "center")))
                    grid.draw(logo_grob)
                    popViewport()
                } else {
                    grid.text("INSELE CAPITAL PARTNERS",
                              x = 0.42, y = 0.89, just = "left",
                              gp = gpar(fontsize = 20, fontface = 2, col = "#1B3A6B"))
                    grid.text("BROKING SERVICES",
                              x = 0.42, y = 0.85, just = "left",
                              gp = gpar(fontsize = 12, col = "#5B7B8A"))
                }

                # Orange accent line (separator below logo)
                grid.lines(x = c(0.38, 0.92), y = c(0.81, 0.81),
                           gp = gpar(col = "#E8913A", lwd = 3))

                # Title: TREASURY HOLDINGS REPORT (stacked)
                grid.text("TREASURY", x = 0.42, y = 0.72, just = "left",
                          gp = gpar(fontsize = 36, fontface = 2, col = "#1B3A6B"))
                grid.text("HOLDINGS", x = 0.42, y = 0.64, just = "left",
                          gp = gpar(fontsize = 36, fontface = 2, col = "#1B3A6B"))
                grid.text("REPORT", x = 0.42, y = 0.56, just = "left",
                          gp = gpar(fontsize = 36, fontface = 2, col = "#1B3A6B"))

                # Data date range (in brand orange)
                grid.text(data_date_range,
                          x = 0.42, y = 0.46, just = "left",
                          gp = gpar(fontsize = 16, col = "#E8913A"))

                # Subtitle
                grid.text("SA Government Bond Institutional Ownership Analysis",
                          x = 0.42, y = 0.38, just = "left",
                          gp = gpar(fontsize = 14, col = "#333333"))

                # Client name (conditional)
                if (nchar(client_name) > 0) {
                    grid.text(paste("Prepared for:", client_name),
                              x = 0.42, y = 0.31, just = "left",
                              gp = gpar(fontsize = 13, fontface = 3, col = "#666666"))
                }

                # Navy footer bar (full width)
                grid.rect(x = unit(0.5, "npc"), y = unit(0.03, "npc"),
                          width = unit(1, "npc"), height = unit(0.06, "npc"),
                          gp = gpar(fill = "#1B3A6B", col = NA))

                grid.text("www.insele.capital",
                          x = 0.10, y = 0.03, just = "left",
                          gp = gpar(fontsize = 9, col = "white"))
                grid.text("+27 11 286 1949",
                          x = 0.35, y = 0.03,
                          gp = gpar(fontsize = 9, col = "white"))
                grid.text("bonds@insele.capital",
                          x = 0.60, y = 0.03,
                          gp = gpar(fontsize = 9, col = "white"))
                grid.text("Prepared by: Insele Capital Partners",
                          x = 0.90, y = 0.03, just = "right",
                          gp = gpar(fontsize = 9, col = "white"))

            } else if (pg$type == "chart_tall") {
                # Tall charts (bond holdings bar charts) — preserve aspect ratio
                grid.newpage()
                draw_page_header(pg$title)

                if (!is.null(pg$grob)) {
                    pushViewport(viewport(x = 0.5, y = 0.45, width = 0.60, height = 0.82))
                    grid.draw(pg$grob)
                    popViewport()
                } else {
                    draw_placeholder(paste(pg$title, "- chart unavailable"))
                }

                add_footer(pg_idx, total_pages)

            } else {
                # Standard chart pages
                grid.newpage()
                draw_page_header(pg$title)

                if (!is.null(pg$grob)) {
                    pushViewport(viewport(x = 0.5, y = 0.47, width = 0.92, height = 0.82))
                    grid.draw(pg$grob)
                    popViewport()
                } else {
                    draw_placeholder(paste(pg$title, "- chart unavailable"))
                }

                add_footer(pg_idx, total_pages)
            }
        }

        dev.off()

        # Copy to output path
        if (temp_pdf != file) {
            file.copy(temp_pdf, file, overwrite = TRUE)
        }

        message("[TREASURY PDF] PDF generation complete.")

    }, error = function(e) {
        message(sprintf("[TREASURY PDF] Error: %s", e$message))
        # Clean up any lingering PDF devices
        while (dev.cur() > 1) { try(dev.off(), silent = TRUE) }

        # Write fallback error PDF
        tryCatch({
            pdf(file, width = 11, height = 8.5)
            grid.newpage()
            grid.text("Treasury Holdings Report",
                      x = 0.5, y = 0.6,
                      gp = gpar(fontsize = 24, fontface = 2, col = "#1B3A6B"))
            grid.text(paste("Error generating report:", e$message),
                      x = 0.5, y = 0.4,
                      gp = gpar(fontsize = 14, col = "#C62828"))
            dev.off()
        }, error = function(e2) {
            while (dev.cur() > 1) { try(dev.off(), silent = TRUE) }
        })
    })
}


# ================================================================================
# TREASURY EMAIL HTML BUILDER
# ================================================================================

#' Build Outlook-compatible HTML email body for treasury holdings reports
#'
#' @param page_labels Character vector of labels for each page. NULL uses defaults.
#' @param n_pages Number of pages in the PDF report
#' @param data_date_range Character string of the data date range (e.g., "Jan 2023 to Dec 2025")
#' @param client_name Client name for header
#' @param primary_color Navy color for header/footer
#' @param accent_color Orange accent color
#' @param tagline Company tagline
#' @return Character string of HTML email body
build_treasury_email_html <- function(page_labels, n_pages, data_date_range = NULL,
                                       client_name = "Insele Capital Partners",
                                       primary_color = "#1B3A6B",
                                       accent_color = "#E8913A",
                                       tagline = "The Power of Partnership") {

    # Default page labels for treasury report
    default_treasury_labels <- c(
        "Cover Page",
        "Aggregate Holdings Over Time",
        "Sector Trend Analysis",
        "Ownership Changes",
        "Fixed Rate Bond Holdings",
        "Fixed Rate Changes (3-Month)",
        "Inflation-Linked Bond Holdings",
        "ILB Changes (3-Month)"
    )

    if (is.null(page_labels)) {
        page_labels <- default_treasury_labels
    }

    # Pad labels if n_pages exceeds the list
    if (n_pages > length(page_labels)) {
        extra <- n_pages - length(page_labels)
        page_labels <- c(page_labels, paste("Page", seq(length(page_labels) + 1, n_pages)))
    }

    # Truncate if fewer pages than labels
    if (n_pages < length(page_labels)) {
        page_labels <- page_labels[seq_len(n_pages)]
    }

    # Pre-compute formatted values
    current_year <- format(Sys.Date(), "%Y")
    date_range_str <- if (!is.null(data_date_range)) data_date_range else format(Sys.Date(), "%B %Y")

    # Build chart sections
    chart_sections <- ""
    for (i in seq_len(n_pages)) {
        cid_ref <- sprintf("chart%02d", i)
        label <- page_labels[i]
        chart_sections <- paste0(chart_sections, sprintf(
            '<tr><td style="padding: 15px 30px 5px 30px; font-family: Arial, Helvetica, sans-serif; font-size: 16px; font-weight: bold; color: %s;">%s</td></tr>
<tr><td style="padding: 5px 30px 15px 30px; text-align: center;"><img src="cid:%s" width="840" style="display: block; margin: 0 auto; max-width: 840px;" /></td></tr>
',
            primary_color, label, cid_ref
        ))
    }

    # Assemble complete HTML (no DOCTYPE - can cause Outlook issues)
    html <- sprintf(
'<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
</head>
<body style="margin: 0; padding: 0; background-color: #f4f4f4; font-family: Arial, Helvetica, sans-serif;">
<table width="900" align="center" cellpadding="0" cellspacing="0" border="0" style="margin: 0 auto; background-color: #ffffff;">
<!-- Header Bar -->
<tr>
<td style="background-color: %s; padding: 20px 30px;">
<table width="100%%" cellpadding="0" cellspacing="0" border="0">
<tr>
<td style="font-family: Arial, Helvetica, sans-serif; font-size: 24px; font-weight: bold; color: #ffffff;">%s</td>
<td style="font-family: Arial, Helvetica, sans-serif; font-size: 13px; color: #cccccc; text-align: right;">Treasury Holdings<br/>%s</td>
</tr>
<tr>
<td colspan="2" style="font-family: Arial, Helvetica, sans-serif; font-size: 16px; color: %s; padding-top: 4px;">Treasury Holdings Report</td>
</tr>
</table>
</td>
</tr>
<!-- Intro Paragraph -->
<tr>
<td style="padding: 20px 30px; background-color: #f8f9fa;">
<table width="100%%" cellpadding="0" cellspacing="0" border="0">
<tr>
<td style="font-family: Arial, Helvetica, sans-serif; font-size: 14px; color: #333333; line-height: 1.5;">
Treasury holdings analysis covering <strong>%s</strong>.
This report contains aggregate sector holdings, ownership change analysis, and bond-level
institutional ownership data for SA Government bonds to support investment decision-making.
</td>
</tr>
</table>
</td>
</tr>
<!-- Chart Sections -->
%s
<!-- Footer -->
<tr>
<td style="background-color: %s; padding: 20px 30px;">
<table width="100%%" cellpadding="0" cellspacing="0" border="0">
<tr>
<td style="font-family: Arial, Helvetica, sans-serif; font-size: 12px; color: #ffffff; text-align: center;">
&copy; %s %s &nbsp;|&nbsp; <span style="color: %s;">%s</span><br/>
<span style="font-size: 11px; color: #aaaaaa;">This report is confidential and intended solely for the recipient. Do not distribute without authorisation.</span>
</td>
</tr>
</table>
</td>
</tr>
</table>
</body>
</html>',
        primary_color,                  # Header bg
        client_name,                    # Header left: client name
        date_range_str,                 # Header right: date range
        accent_color,                   # "Treasury Holdings Report" subtitle color
        date_range_str,                 # Intro: data period
        chart_sections,                 # All chart sections
        primary_color,                  # Footer bg
        current_year,                   # Footer: year
        client_name,                    # Footer: client name
        accent_color,                   # Footer: tagline color
        tagline                         # Footer: tagline
    )

    return(html)
}


# ================================================================================
# CUSTOM / DYNAMIC REPORT EMAIL HTML BUILDER
# ================================================================================

#' Build HTML email body for custom/dynamic reports with user-selected sections
#'
#' @param page_labels Character vector of labels for each page
#' @param n_pages Number of pages in the PDF report
#' @param report_title User-specified report title
#' @param report_type_label Display name for the report type (e.g., "Custom Report")
#' @param sections_included Character vector of section display names included
#' @param client_name Client/company name for header
#' @param report_date Date of the report
#' @param primary_color Navy color for header/footer
#' @param accent_color Orange accent color
#' @param tagline Company tagline
#' @return Character string of HTML email body
build_custom_email_html <- function(page_labels, n_pages, report_title, report_type_label,
                                     sections_included = NULL,
                                     client_name = "Insele Capital Partners",
                                     report_date = Sys.Date(),
                                     primary_color = "#1B3A6B",
                                     accent_color = "#E8913A",
                                     tagline = "The Power of Partnership") {

    # Handle page labels
    if (is.null(page_labels)) {
        page_labels <- paste("Page", seq_len(n_pages))
    }
    if (n_pages > length(page_labels)) {
        page_labels <- c(page_labels, paste("Page", seq(length(page_labels) + 1, n_pages)))
    }
    if (n_pages < length(page_labels)) {
        page_labels <- page_labels[seq_len(n_pages)]
    }

    # Pre-compute formatted values
    current_year <- format(Sys.Date(), "%Y")
    report_date_str <- format(report_date, "%d %B %Y")

    # Build intro text from sections included
    sections_str <- if (!is.null(sections_included) && length(sections_included) > 0) {
        paste(sections_included, collapse = ", ")
    } else {
        "selected analytics"
    }

    # Build chart sections with CID image references
    chart_sections <- ""
    for (i in seq_len(n_pages)) {
        cid_ref <- sprintf("chart%02d", i)
        label <- page_labels[i]
        chart_sections <- paste0(chart_sections, sprintf(
            '<tr><td style="padding: 15px 30px 5px 30px; font-family: Arial, Helvetica, sans-serif; font-size: 16px; font-weight: bold; color: %s;">%s</td></tr>
<tr><td style="padding: 5px 30px 15px 30px; text-align: center;"><img src="cid:%s" width="840" style="display: block; margin: 0 auto; max-width: 840px;" /></td></tr>
',
            primary_color, label, cid_ref
        ))
    }

    # Assemble Outlook-safe HTML (no DOCTYPE)
    html <- sprintf(
'<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
</head>
<body style="margin: 0; padding: 0; background-color: #f4f4f4; font-family: Arial, Helvetica, sans-serif;">
<table width="900" align="center" cellpadding="0" cellspacing="0" border="0" style="margin: 0 auto; background-color: #ffffff;">
<!-- Header Bar -->
<tr>
<td style="background-color: %s; padding: 20px 30px;">
<table width="100%%" cellpadding="0" cellspacing="0" border="0">
<tr>
<td style="font-family: Arial, Helvetica, sans-serif; font-size: 24px; font-weight: bold; color: #ffffff;">%s</td>
<td style="font-family: Arial, Helvetica, sans-serif; font-size: 13px; color: #cccccc; text-align: right;">%s<br/>%s</td>
</tr>
<tr>
<td colspan="2" style="font-family: Arial, Helvetica, sans-serif; font-size: 16px; color: %s; padding-top: 4px;">%s</td>
</tr>
</table>
</td>
</tr>
<!-- Intro Paragraph -->
<tr>
<td style="padding: 20px 30px; background-color: #f8f9fa;">
<table width="100%%" cellpadding="0" cellspacing="0" border="0">
<tr>
<td style="font-family: Arial, Helvetica, sans-serif; font-size: 14px; color: #333333; line-height: 1.5;">
<strong>%s</strong> prepared on <strong>%s</strong>.
This report covers %s to support investment decision-making.
</td>
</tr>
</table>
</td>
</tr>
<!-- Chart Sections -->
%s
<!-- Footer -->
<tr>
<td style="background-color: %s; padding: 20px 30px;">
<table width="100%%" cellpadding="0" cellspacing="0" border="0">
<tr>
<td style="font-family: Arial, Helvetica, sans-serif; font-size: 12px; color: #ffffff; text-align: center;">
&copy; %s %s &nbsp;|&nbsp; <span style="color: %s;">%s</span><br/>
<span style="font-size: 11px; color: #aaaaaa;">This report is confidential and intended solely for the recipient. Do not distribute without authorisation.</span>
</td>
</tr>
</table>
</td>
</tr>
</table>
</body>
</html>',
        primary_color,                  # Header bg
        client_name,                    # Header left: client name
        report_type_label,              # Header right: report type
        report_date_str,                # Header right: date
        accent_color,                   # Report type subtitle color
        report_type_label,              # Report type subtitle text
        report_title,                   # Intro: report title
        report_date_str,                # Intro: date
        sections_str,                   # Intro: sections included
        chart_sections,                 # All chart sections
        primary_color,                  # Footer bg
        current_year,                   # Footer: year
        client_name,                    # Footer: client name
        accent_color,                   # Footer: tagline color
        tagline                         # Footer: tagline
    )

    return(html)
}


# ================================================================================
# CUSTOM REPORT PDF GENERATOR (shared by PDF download and .eml email)
# ================================================================================

#' Generate a custom report PDF with dynamically selected sections and charts
#'
#' Encapsulates the full PDF rendering pipeline for non-pre-auction, non-treasury
#' reports. Used by both the PDF download handler and the .eml email draft handler.
#'
#' @param output_path File path to write the PDF
#' @param config Report configuration from report_config() reactive
#' @param report_data Report data from collect_report_data()
#' @param logo_grob Pre-loaded logo grob (or NULL)
#' @return List with success (logical), page_labels (character vector), sections_included (character vector), n_pages (integer)
generate_custom_report_pdf <- function(output_path, config, report_data, logo_grob) {

    # Section display names
    section_names <- c(
        overview = "Market Overview",
        relative = "Relative Value Analysis",
        risk = "Risk Analytics",
        technical = "Technical Analysis",
        carry = "Carry & Roll Analysis",
        auction = "Auction Intelligence",
        intelligence = "Market Intelligence",
        treasury = "Treasury Holdings",
        recommendations = "Trading Recommendations"
    )

    # Chart-to-section mapping
    chart_sections_map <- list(
        overview = c("regime_plot"),
        relative = c("yield_curve", "relative_heatmap", "zscore_plot", "convexity"),
        risk = c("var_distribution", "var_ladder", "dv01_ladder"),
        technical = c("technical_plot", "signal_matrix"),
        carry = c("carry_heatmap", "scenario_analysis", "butterfly_spread", "forward_curve"),
        auction = c("auction_performance", "auction_patterns", "concession_trend",
                     "auction_forecast", "demand_elasticity", "success_probability",
                     "bid_distribution", "ytd_issuance", "auction_sentiment"),
        intelligence = c("yield_percentile", "rate_of_change", "curve_comparison",
                          "curve_steepness", "regime_probability"),
        treasury = c("holdings_area", "sector_trend", "holdings_fixed", "holdings_ilb",
                      "holdings_frn", "holdings_sukuk", "ownership_changes",
                      "holdings_diverging_fixed", "holdings_diverging_ilb")
    )

    # Chart display names for page labels
    chart_display_names <- c(
        regime_plot = "Regime Analysis",
        yield_curve = "Yield Curve",
        relative_heatmap = "Relative Value Heatmap",
        zscore_plot = "Z-Score Distribution",
        convexity = "Enhanced Convexity",
        var_distribution = "VaR Distribution",
        var_ladder = "VaR Ladder",
        dv01_ladder = "DV01 Analysis",
        technical_plot = "Technical Indicators",
        signal_matrix = "Signal Matrix",
        carry_heatmap = "Carry & Roll Heatmap",
        scenario_analysis = "Scenario Analysis",
        butterfly_spread = "Butterfly Spread",
        forward_curve = "Forward Curve",
        auction_performance = "Auction Performance",
        auction_patterns = "Auction Patterns",
        auction_forecast = "Auction Forecast",
        demand_elasticity = "Demand Elasticity",
        success_probability = "Success Probability",
        bid_distribution = "Bid Distribution",
        ytd_issuance = "YTD Issuance",
        auction_sentiment = "Auction Sentiment",
        concession_trend = "Auction Demand Trend",
        yield_percentile = "Yield Percentile Heatmap",
        rate_of_change = "Rate of Change Monitor",
        curve_comparison = "Curve Comparison",
        curve_steepness = "Curve Steepness Gauge",
        regime_probability = "Regime Probability Gauge",
        holdings_area = "Aggregate Holdings",
        sector_trend = "Sector Trend",
        holdings_fixed = "Fixed Rate Holdings",
        holdings_ilb = "ILB Holdings",
        holdings_frn = "FRN Holdings",
        holdings_sukuk = "Sukuk Holdings",
        ownership_changes = "Ownership Changes",
        holdings_diverging_fixed = "Fixed Rate Changes",
        holdings_diverging_ilb = "ILB Changes"
    )

    proc_data <- report_data$proc_data
    filt_data <- report_data$filt_data
    var_data_val <- report_data$var_data_val
    regime_data_val <- report_data$regime_data_val
    carry_data_val <- report_data$carry_data_val
    filt_data_with_tech <- report_data$filt_data_with_tech

    # Collect charts using existing collect_report_charts()
    charts <- list()
    chart_collection <- NULL
    tryCatch({
        chart_collection <- collect_report_charts(
            proc_data, filt_data, filt_data_with_tech,
            var_data_val, regime_data_val, carry_data_val,
            report_data$treasury_ts, report_data$treasury_bonds,
            config$input_params
        )
        for (name in names(chart_collection$charts)) {
            chart_path <- chart_collection$charts[[name]]
            if (is.character(chart_path) && file.exists(chart_path)) {
                charts[[name]] <- readRDS(chart_path)
            } else if (!is.character(chart_path)) {
                charts[[name]] <- chart_path
            }
        }
    }, error = function(e) {
        message(sprintf("Chart collection error: %s", e$message))
    })

    on.exit({
        if (!is.null(chart_collection) && !is.null(chart_collection$cleanup)) {
            chart_collection$cleanup()
        }
    }, add = TRUE)

    # Generate summaries for executive summary page
    summaries <- tryCatch({
        generate_report_summaries(
            proc_data, filt_data, var_data_val,
            regime_data_val, carry_data_val,
            report_data$treasury_ts, report_data$treasury_bonds
        )
    }, error = function(e) {
        list(executive = "Report generation in progress.")
    })

    # Pre-render ggplot charts to grobs BEFORE opening PDF device
    chart_grobs <- list()
    for (name in names(charts)) {
        chart_obj <- charts[[name]]
        if ("ggplot" %in% class(chart_obj)) {
            tryCatch({
                temp_png <- tempfile(fileext = ".png")
                ggsave(temp_png, chart_obj, width = 10, height = 6, dpi = 150, bg = "white")
                chart_img <- png::readPNG(temp_png)
                chart_grobs[[name]] <- grid::rasterGrob(chart_img, interpolate = TRUE)
                unlink(temp_png)
            }, error = function(e) {
                chart_grobs[[name]] <<- NULL
            })
        } else {
            chart_grobs[[name]] <- chart_obj
        }
    }

    # Track page labels as we build the PDF
    page_labels <- character()
    sections_included <- character()

    # Count total pages for page numbering
    total_chart_count <- sum(sapply(config$sections, function(s) {
        if (s == "recommendations") return(1)
        cs <- chart_sections_map[[s]]
        if (is.null(cs)) return(0)
        sum(vapply(cs, function(cn) !is.null(chart_grobs[[cn]]), logical(1)))
    }))
    total_pages <- 2 + total_chart_count + 1  # title + exec + charts + footer

    add_page_number <- function(pn, tp) {
        if (pn > 1) {
            grid.text(
                label = paste0("Page ", pn, " of ", tp),
                x = 0.95, y = 0.02,
                just = c("right", "bottom"),
                gp = gpar(fontsize = 8, col = "#999999", fontface = 1)
            )
        }
    }

    add_page_header <- function(title_text = "") {
        if (!is.null(logo_grob)) {
            pushViewport(viewport(x = 0.92, y = 0.97, width = 0.12, height = 0.05))
            grid.draw(logo_grob)
            popViewport()
        }
        if (title_text != "") {
            grid.text(title_text, x = 0.05, y = 0.97, just = "left",
                      gp = gpar(fontsize = 12, fontface = 2, col = "#1B3A6B"))
        }
        grid.lines(x = c(0.05, 0.95), y = c(0.94, 0.94),
                   gp = gpar(col = "#E0E0E0", lwd = 0.5))
    }

    tryCatch({
        pdf(output_path, width = 11, height = 8.5)
        page_number <- 0

        # ── TITLE PAGE ─────────────────────────────────────────────────────
        page_number <- page_number + 1
        grid.newpage()
        page_labels <- c(page_labels, "Cover Page")

        if (!is.null(logo_grob)) {
            pushViewport(viewport(x = 0.5, y = 0.9, width = 0.3, height = 0.15))
            grid.draw(logo_grob)
            popViewport()
            title_y <- 0.7; subtitle_y <- 0.6; date_y <- 0.5; client_y <- 0.4
            grid.lines(x = c(0.2, 0.8), y = c(0.82, 0.82),
                       gp = gpar(col = "#1B3A6B", lwd = 2))
        } else {
            title_y <- 0.7; subtitle_y <- 0.6; date_y <- 0.5; client_y <- 0.4
        }

        grid.text("SA Government Bond Analysis Report",
                  x = 0.5, y = title_y,
                  gp = gpar(fontsize = 24, fontface = 2, col = "#1B3A6B"))
        grid.text(config$report_title %||% "Market Analysis",
                  x = 0.5, y = subtitle_y,
                  gp = gpar(fontsize = 18, col = "#333333"))
        grid.text(format(config$report_date %||% Sys.Date(), "%B %d, %Y"),
                  x = 0.5, y = date_y,
                  gp = gpar(fontsize = 14, col = "#666666"))
        if (!is.null(config$client_name) && nchar(config$client_name) > 0) {
            grid.text(paste("Prepared for:", config$client_name),
                      x = 0.5, y = client_y,
                      gp = gpar(fontsize = 12, fontface = 3, col = "#666666"))
        }
        if (is.null(logo_grob)) {
            grid.text("INSELE CAPITAL PARTNERS", x = 0.5, y = 0.15,
                      gp = gpar(fontsize = 16, fontface = 2, col = "#1B3A6B"))
        }

        # ── EXECUTIVE SUMMARY PAGE ─────────────────────────────────────────
        page_number <- page_number + 1
        grid.newpage()
        page_labels <- c(page_labels, "Executive Summary")

        if (!is.null(logo_grob)) {
            pushViewport(viewport(x = 0.92, y = 0.95, width = 0.1, height = 0.06,
                                  just = c("right", "top")))
            grid.draw(logo_grob)
            popViewport()
        }
        grid.lines(x = c(0.05, 0.95), y = c(0.92, 0.92),
                   gp = gpar(col = "#E0E0E0", lwd = 1))
        grid.text("Executive Summary", x = 0.05, y = 0.95, just = "left",
                  gp = gpar(fontsize = 18, fontface = 2, col = "#1B3A6B"))

        exec_metrics <- summaries$exec_metrics
        if (!is.null(exec_metrics) && length(exec_metrics) > 0) {
            metric_names <- names(exec_metrics)
            n_metrics <- length(metric_names)
            n_cols <- min(4, n_metrics)
            n_rows <- ceiling(n_metrics / n_cols)
            card_w <- 0.9 / n_cols
            card_h <- 0.22
            card_fill_map <- c(
                "var_summary" = "#E8F0FE", "regime" = "#E8F0FE",
                "rv_opportunities" = "#E8F5E9", "top_signals" = "#E8F5E9",
                "yield_range" = "#F5F5F5", "avg_duration" = "#F5F5F5",
                "steepest_spread" = "#F5F5F5", "top_carry" = "#E8F5E9"
            )
            for (i in seq_along(metric_names)) {
                m <- exec_metrics[[metric_names[i]]]
                if (is.null(m)) next
                col_idx <- ((i - 1) %% n_cols) + 1
                row_idx <- ((i - 1) %/% n_cols) + 1
                cx <- 0.05 + (col_idx - 0.5) * card_w
                cy <- 0.85 - row_idx * (card_h + 0.02)
                card_fill <- card_fill_map[[metric_names[i]]] %||% "#F5F7FA"
                grid.rect(x = cx, y = cy, width = card_w * 0.92, height = card_h,
                          gp = gpar(fill = card_fill, col = "#E0E4E8", lwd = 0.5), just = "centre")
                grid.text(m$label, x = cx, y = cy + card_h * 0.35,
                          gp = gpar(fontsize = 8, col = "#888888", fontface = 1))
                grid.text(m$value, x = cx, y = cy + card_h * 0.05,
                          gp = gpar(fontsize = 13, col = "#1B3A6B", fontface = 2))
                if (!is.null(m$subtitle) && nchar(m$subtitle) > 0) {
                    grid.text(m$subtitle, x = cx, y = cy - card_h * 0.3,
                              gp = gpar(fontsize = 7, col = "#999999", fontface = 1))
                }
            }
            summary_y <- 0.85 - (n_rows + 1) * (card_h + 0.02)
            if (summary_y > 0.05 && !is.null(summaries$executive)) {
                wrapped_exec <- strwrap(summaries$executive, width = 110)
                grid.text(paste(wrapped_exec, collapse = "\n"),
                          x = 0.5, y = max(summary_y, 0.08),
                          gp = gpar(fontsize = 9, col = "#555555", lineheight = 1.3))
            }
        } else if (!is.null(summaries$executive)) {
            wrapped_text <- strwrap(summaries$executive, width = 95)
            summary_df <- data.frame(Content = wrapped_text, stringsAsFactors = FALSE)
            summary_table <- gridExtra::tableGrob(summary_df, rows = NULL, cols = NULL,
                theme = gridExtra::ttheme_minimal(base_size = 11, base_colour = "#333333"))
            pushViewport(viewport(x = 0.5, y = 0.5, width = 0.9, height = 0.7))
            grid.draw(summary_table)
            popViewport()
        }
        add_page_number(page_number, total_pages)

        # ── Section summaries for divider headers ──
        section_summaries <- list()
        tryCatch({
            section_summaries$overview <- summaries$market_conditions %||% ""
            section_summaries$relative <- summaries$relative_summary %||% ""
            section_summaries$risk <- summaries$risk_summary %||% ""
            section_summaries$technical <- summaries$technical_summary %||% ""
            section_summaries$carry <- summaries$carry_summary %||% ""
            section_summaries$auction <- summaries$auction_summary %||% ""
            section_summaries$intelligence <- summaries$intelligence_summary %||% ""
            section_summaries$treasury <- summaries$treasury_summary %||% ""
            section_summaries$recommendations <- ""
        }, error = function(e) {})

        # Generate trading recommendations
        trading_recs <- tryCatch({
            generate_trading_recommendations(
                proc_data, filt_data, var_data_val,
                carry_data_val, regime_data_val
            )
        }, error = function(e) { list() })

        # ── CHART PAGES BY SECTION ─────────────────────────────────────────
        for (section in config$sections) {
            section_title <- section_names[[section]]
            sections_included <- c(sections_included, section_title)

            # Trading Recommendations (text-only page)
            if (section == "recommendations") {
                page_number <- page_number + 1
                grid.newpage()
                add_page_header("Trading Recommendations")
                page_labels <- c(page_labels, "Trading Recommendations")

                if (length(trading_recs) > 0) {
                    y_pos <- 0.88
                    for (rec_section in trading_recs) {
                        if (is.null(rec_section) || !is.list(rec_section)) next
                        title <- rec_section$title %||% ""
                        items <- rec_section$items %||% character()
                        grid.text(title, x = 0.06, y = y_pos, just = "left",
                                  gp = gpar(fontsize = 12, fontface = 2, col = "#1B3A6B"))
                        y_pos <- y_pos - 0.03
                        grid.lines(x = c(0.06, 0.94), y = c(y_pos, y_pos),
                                   gp = gpar(col = "#E0E4E8", lwd = 0.5))
                        y_pos <- y_pos - 0.02
                        if (is.character(items) && length(items) > 0) {
                            for (item in items) {
                                wrapped <- strwrap(paste0("\u2022 ", item), width = 105)
                                for (line in wrapped) {
                                    grid.text(line, x = 0.08, y = y_pos, just = "left",
                                              gp = gpar(fontsize = 9, col = "#444444", lineheight = 1.3))
                                    y_pos <- y_pos - 0.025
                                }
                            }
                        }
                        y_pos <- y_pos - 0.02
                        if (y_pos < 0.08) break
                    }
                } else if (!is.null(summaries$recommendations)) {
                    wrapped_recs <- strwrap(summaries$recommendations, width = 95)
                    grid.text(paste(wrapped_recs, collapse = "\n"), x = 0.5, y = 0.5,
                              gp = gpar(fontsize = 11, col = "#333333", lineheight = 1.4))
                }
                add_page_number(page_number, total_pages)
                next
            }

            # Chart pages for this section
            section_chart_names <- chart_sections_map[[section]]
            first_chart_in_section <- TRUE

            if (!is.null(section_chart_names)) {
                for (chart_name in section_chart_names) {
                    if (!chart_name %in% names(chart_grobs)) next
                    chart_grob <- chart_grobs[[chart_name]]
                    if (is.null(chart_grob)) next

                    page_number <- page_number + 1
                    display_name <- chart_display_names[[chart_name]] %||%
                        gsub("_", " ", tools::toTitleCase(chart_name))
                    page_labels <- c(page_labels,
                        paste0(section_title, ": ", display_name))

                    tryCatch({
                        grid.newpage()
                        if (first_chart_in_section) {
                            # Combined section header + first chart
                            pushViewport(viewport(x = 0.5, y = 0.91, width = 0.92, height = 0.16))
                            if (!is.null(logo_grob)) {
                                pushViewport(viewport(x = 0.95, y = 0.85, width = 0.08, height = 0.5,
                                                      just = c("right", "top")))
                                grid.draw(logo_grob)
                                popViewport()
                            }
                            grid.text(section_title, x = 0.02, y = 0.65, just = "left",
                                      gp = gpar(fontsize = 18, fontface = 2, col = "#1B3A6B"))
                            grid.lines(x = c(0, 1), y = c(0.35, 0.35),
                                       gp = gpar(col = "#1B3A6B", lwd = 1.5))
                            sec_summary <- section_summaries[[section]] %||% ""
                            if (nchar(sec_summary) > 0) {
                                wrapped_summary <- strwrap(sec_summary, width = 110)
                                display_summary <- paste(wrapped_summary[1:min(2, length(wrapped_summary))], collapse = "\n")
                                grid.text(display_summary, x = 0.02, y = 0.1, just = "left",
                                          gp = gpar(fontsize = 9, col = "#666666", lineheight = 1.3))
                            }
                            popViewport()
                            pushViewport(viewport(x = 0.5, y = 0.39, width = 0.92, height = 0.72))
                            grid.draw(chart_grob)
                            popViewport()
                            first_chart_in_section <- FALSE
                        } else {
                            # Subsequent charts: full page with small header
                            add_page_header(display_name)
                            pushViewport(viewport(x = 0.5, y = 0.45, width = 0.92, height = 0.82))
                            grid.draw(chart_grob)
                            popViewport()
                        }
                        add_page_number(page_number, total_pages)
                    }, error = function(e) {
                        grid.newpage()
                        add_page_header()
                        grid.text(paste("Chart", chart_name, "temporarily unavailable"),
                                  x = 0.5, y = 0.5,
                                  gp = gpar(fontsize = 12, col = "#666666"))
                        add_page_number(page_number, total_pages)
                    })
                }
            }

            # If section had no valid charts, still render a section page
            if (first_chart_in_section) {
                page_number <- page_number + 1
                grid.newpage()
                page_labels <- c(page_labels, section_title)
                grid.text(section_title, x = 0.5, y = 0.6,
                          gp = gpar(fontsize = 22, fontface = 2, col = "#1B3A6B"))
                grid.text("No charts available for this section",
                          x = 0.5, y = 0.45,
                          gp = gpar(fontsize = 11, col = "#999999"))
                add_page_number(page_number, total_pages)
            }
        }

        # ── FOOTER PAGE ────────────────────────────────────────────────────
        page_number <- page_number + 1
        grid.newpage()
        page_labels <- c(page_labels, "Disclaimer")

        if (!is.null(logo_grob)) {
            pushViewport(viewport(x = 0.5, y = 0.7, width = 0.25, height = 0.1))
            grid.draw(logo_grob)
            popViewport()
        }
        grid.text(
            paste("\u00A9", format(Sys.Date(), "%Y"),
                  "Insele Capital Partners. All rights reserved.\n\n",
                  "This report is proprietary and confidential.\n",
                  "Disclaimer: This report is for informational purposes only",
                  "and does not constitute investment advice."),
            x = 0.5, y = 0.4,
            gp = gpar(fontsize = 10, col = "#666666", lineheight = 1.5)
        )
        add_page_number(page_number, total_pages)

        dev.off()

        return(list(
            success = TRUE,
            page_labels = page_labels,
            sections_included = sections_included,
            n_pages = page_number
        ))

    }, error = function(e) {
        message(sprintf("Custom PDF generation error: %s", e$message))
        while (dev.cur() > 1) { try(dev.off(), silent = TRUE) }
        return(list(
            success = FALSE,
            page_labels = character(),
            sections_included = character(),
            n_pages = 0
        ))
    })
}