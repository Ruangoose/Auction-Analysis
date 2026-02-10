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
                if(!is.null(processed_data) && nrow(processed_data) > 0) {
                    # Calculate butterfly spreads
                    butterflies <- calculate_butterfly_spreads(processed_data, lookback_days = 365)
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
        )
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
            if (!is.null(filtered_data) && "yield" %in% names(filtered_data)) {
                n_bonds <- length(unique(filtered_data$bond))
                sprintf("Technical analysis covers %d bonds with RSI, MACD, and Bollinger Band indicators applied to yield time series.", n_bonds)
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