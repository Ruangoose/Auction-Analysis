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
                # Use filtered_data_with_technicals for bond selection fallback
                bond_select <- input_params$tech_bond_select %||%
                    (if(!is.null(filtered_data_with_technicals) && nrow(filtered_data_with_technicals) > 0) {
                        unique(filtered_data_with_technicals$bond)[1]
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
            # ✨ NEW PLOT
            optimal_holding = function() {
                if(!is.null(carry_roll_data) && nrow(carry_roll_data) > 0) {
                    generate_optimal_holding_enhanced_plot(carry_roll_data)
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
                        generate_auction_pattern_analysis(filtered_data, list())
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
                    generate_bid_distribution_plot(filtered_data, list())
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
            },
            # ✨ NEW PLOT
            btc_decomposition = function() {
                if(!is.null(filtered_data) && nrow(filtered_data) > 0) {
                    generate_btc_decomposition_plot(filtered_data, list())
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
generate_report_summaries <- function(processed_data, filtered_data, var_data, regime_data, carry_roll_data) {
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


#' @export
# Create R Markdown template for PDF reports
create_report_template <- function(params) {
    template <- paste0('
---
title: "SA Government Bond Analysis Report"
subtitle: "', params$report_title, '"
author: "Insele Capital Partners - Broking Services"
date: "', format(params$report_date, "%B %d, %Y"), '"
output:
  pdf_document:
    toc: true
    toc_depth: 2
    number_sections: true
    fig_caption: true
    highlight: tango
header-includes:
  - \\usepackage{fancyhdr}
  - \\pagestyle{fancy}
  - \\fancyhead[L]{Insele Capital Partners}
  - \\fancyhead[R]{Bond Analysis Report}
  - \\usepackage{color}
  - \\definecolor{insele}{RGB}{27,58,107}
---

\\newpage

# Executive Summary

**Report Date:** ', format(params$report_date, "%B %d, %Y"), '
**Analysis Period:** ', format(params$date_range[1], "%B %d, %Y"), ' to ',
                       format(params$date_range[2], "%B %d, %Y"), '
**Bonds Analyzed:** ', params$bond_count, '

## Key Findings

', params$executive_summary, '

## Market Conditions

', params$market_conditions, '

\\newpage

')

    # Add sections based on selection
    if("relative" %in% params$sections) {
        template <- paste0(template, '
# Relative Value Analysis

', params$relative_summary, '

![Yield Curve Analysis](', params$chart_paths$yield_curve, ')

![Relative Value Heatmap](', params$chart_paths$relative_heatmap, ')

![Z-Score Distribution](', params$chart_paths$zscore_plot, ')

')
    }

    if("risk" %in% params$sections) {
        template <- paste0(template, '
# Risk Analytics

', params$risk_summary, '

![Value at Risk Distribution](', params$chart_paths$var_distribution, ')

![VaR Ladder](', params$chart_paths$var_ladder, ')

![DV01 Analysis](', params$chart_paths$dv01_ladder, ')

')
    }

    if("auction" %in% params$sections) {
        template <- paste0(template, '
# Auction Intelligence

', params$auction_summary, '

## Upcoming Auctions This Week

', params$weekly_auctions, '

![Auction Performance](', params$chart_paths$auction_performance, ')

![Historical Patterns](', params$chart_paths$auction_patterns, ')

')
    }

    if("recommendations" %in% params$sections) {
        template <- paste0(template, '
# Recommendations

## Trading Opportunities

', params$recommendations, '

## Risk Considerations

', params$risk_considerations, '

')
    }

    template <- paste0(template, '

---

*This report is proprietary and confidential. © ', format(Sys.Date(), "%Y"),
                       ' Insele Capital Partners. All rights reserved.*

*Disclaimer: This report is for informational purposes only and does not constitute investment advice.*
')

    return(template)
}

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