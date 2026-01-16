# ================================================================================
# INSELE CAPITAL PARTNERS - ENHANCED THEME CONFIGURATION MODULE v3.0
# Ultra-Sophisticated Theme Management System with Dynamic Switching
# ================================================================================

# ================================================================================
# Required libraries for theme system
# Note: These packages are already loaded by the main server (enhanced_bond_server.R)
# This validation ensures they're available when this module is sourced.
# ================================================================================

suppressPackageStartupMessages({
    # Validate required packages are loaded
    required_theme_packages <- c("ggplot2", "scales", "stringr", "jsonlite", "R6")

    for (pkg in required_theme_packages) {
        if (!requireNamespace(pkg, quietly = TRUE)) {
            stop(sprintf("Required package '%s' not available. Please load it in the main server file.", pkg))
        }
    }

    # Quietly attach if not already attached
    if (!"package:ggplot2" %in% search()) library(ggplot2, quietly = TRUE)
    if (!"package:scales" %in% search()) library(scales, quietly = TRUE)
    if (!"package:stringr" %in% search()) library(stringr, quietly = TRUE)
    if (!"package:jsonlite" %in% search()) library(jsonlite, quietly = TRUE)
    if (!"package:R6" %in% search()) library(R6, quietly = TRUE)
})

# ================================================================================
# NULL COALESCING OPERATOR
# ================================================================================

`%||%` <- function(x, y) {
    if (is.null(x)) y else x
}

# ================================================================================
# THEME CONFIGURATION MANAGER
# ================================================================================

#' @title Theme Configuration Manager
#' @description Singleton pattern for managing global theme state
#' @export
ThemeManager <- R6::R6Class(
    classname = "ThemeManager",

    public = list(
        #' @description Initialize theme manager with default configurations
        initialize = function() {
            private$.current_theme <- "light"
            private$.themes <- private$load_default_themes()
            private$.responsive_config <- private$load_responsive_config()
            private$.animation_config <- private$load_animation_config()
            private$.export_config <- private$load_export_config()
            private$.cache <- new.env(parent = emptyenv())
            private$.performance_log <- list()
            invisible(self)
        },

        #' @description Get current theme
        get_theme = function(mode = NULL, base_size = NULL, device = NULL) {
            start_time <- proc.time()[["elapsed"]]

            # Use current theme if mode not specified
            mode <- mode %||% private$.current_theme

            # Determine base size based on device
            if (is.null(base_size)) {
                base_size <- private$get_responsive_size(device)
            }

            cache_items <- ls(envir = private$.cache)
            if(length(cache_items) > 50) {
                # Remove oldest items (simple FIFO for now)
                to_remove <- cache_items[1:10]
                rm(list = to_remove, envir = private$.cache)
            }

            # Check cache
            cache_key <- paste(mode, base_size, device, sep = "_")
            if (exists(cache_key, envir = private$.cache)) {
                theme <- get(cache_key, envir = private$.cache)
            } else {
                # Generate theme
                theme <- private$build_theme(mode, base_size)
                assign(cache_key, theme, envir = private$.cache)
            }

            # Log performance
            elapsed <- (proc.time()[["elapsed"]] - start_time) * 1000  # Convert to milliseconds
            private$log_performance("get_theme", elapsed)

            # Ensure <50ms performance constraint
            if (elapsed > 50) {
                warning(sprintf("Theme generation took %.2fms (>50ms threshold)", elapsed))
            }

            return(theme)
        },

        #' @description Clear theme cache (call periodically or on low memory)
        clear_cache = function(max_age_minutes = NULL) {
            if(is.null(max_age_minutes)) {
                # Clear all cache
                rm(list = ls(envir = private$.cache), envir = private$.cache)
                message("Theme cache cleared completely")
            } else {
                # Clear old cache entries
                # Note: This requires tracking cache timestamps
                # For now, just clear everything if called with parameter
                rm(list = ls(envir = private$.cache), envir = private$.cache)
                message(sprintf("Theme cache cleared (entries older than %d minutes)", max_age_minutes))
            }
            invisible(NULL)
        },

        #' @description Get cache size and statistics
        get_cache_stats = function() {
            cache_items <- ls(envir = private$.cache)
            list(
                count = length(cache_items),
                keys = cache_items,
                size_bytes = object.size(private$.cache)
            )
        },

        #' @description Switch theme mode
        switch_theme = function(mode) {
            if (!mode %in% names(private$.themes)) {
                stop(sprintf("Unknown theme mode: %s", mode))
            }

            old_mode <- private$.current_theme
            private$.current_theme <- mode

            # Clear cache when switching themes
            rm(list = ls(envir = private$.cache), envir = private$.cache)

            # Emit theme change event (for reactive UIs)
            if (exists("session", envir = .GlobalEnv)) {
                session <- get("session", envir = .GlobalEnv)
                if (!is.null(session)) {
                    session$sendCustomMessage("themeChanged", list(
                        from = old_mode,
                        to = mode
                    ))
                }
            }

            invisible(mode)
        },

        #' @description Get current theme mode
        get_current_mode = function() {
            return(private$.current_theme)
        },

        #' @description Get theme colors
        get_colors = function(mode = NULL) {
            mode <- mode %||% private$.current_theme
            return(private$.themes[[mode]]$colors)
        },

        #' @description Get animation specifications
        get_animations = function(element = NULL) {
            if (is.null(element)) {
                return(private$.animation_config)
            }
            return(private$.animation_config[[element]] %||% private$.animation_config$default)
        },

        #' @description Get export theme configuration
        get_export_theme = function(format, base_size = NULL) {
            config <- private$.export_config[[tolower(format)]]
            if (is.null(config)) {
                warning(sprintf("No export config for format: %s, using defaults", format))
                config <- private$.export_config$default
            }

            # Adjust base size for export
            if (is.null(base_size)) {
                base_size <- config$base_size %||% 12
            }

            # Build export-optimized theme
            theme <- private$build_theme("print", base_size)
            theme <- theme + theme(
                plot.background = element_rect(fill = config$background %||% "white"),
                panel.background = element_rect(fill = config$background %||% "white")
            )

            return(list(theme = theme, config = config))
        },

        #' @description Get performance metrics
        get_performance_metrics = function() {
            if (length(private$.performance_log) == 0) {
                return(NULL)
            }

            df <- do.call(rbind, lapply(private$.performance_log, as.data.frame))
            df$timestamp <- as.POSIXct(df$timestamp)

            summary_stats <- aggregate(time_ms ~ operation, data = df, FUN = function(x) {
                c(mean = mean(x), median = median(x), max = max(x), count = length(x))
            })

            return(list(
                raw = df,
                summary = summary_stats,
                total_calls = nrow(df),
                avg_time = mean(df$time_ms),
                compliance_rate = mean(df$time_ms < 50) * 100
            ))
        },

        #' @description Create theme preview
        generate_preview = function(output_file = NULL) {
            if (!require(gridExtra)) {
                warning("gridExtra package required for preview generation")
                return(NULL)
            }

            # Create sample plots for each theme
            sample_data <- data.frame(
                x = 1:10,
                y = rnorm(10),
                group = rep(c("A", "B"), 5)
            )

            plots <- list()
            for (mode in c("light", "dark", "print")) {
                p <- ggplot(sample_data, aes(x, y, color = group)) +
                    geom_line(size = 1.5) +
                    geom_point(size = 3) +
                    labs(
                        title = sprintf("%s Theme Preview", stringr::str_to_title(mode)),
                        subtitle = "Insele Capital Partners Theme System",
                        x = "Time Period",
                        y = "Value",
                        caption = "© 2024 Insele Capital Partners"
                    ) +
                    self$get_theme(mode = mode)

                plots[[mode]] <- p
            }

            # Arrange plots
            preview <- gridExtra::grid.arrange(
                grobs = plots,
                ncol = 1,
                top = grid::textGrob("Theme System Preview",
                                     gp = grid::gpar(fontsize = 16, fontface = "bold"))
            )

            # Save if output file specified
            if (!is.null(output_file)) {
                ggsave(output_file, preview, width = 12, height = 16, dpi = 150)
            }

            return(preview)
        }
    ),

    private = list(
        # Private fields
        .current_theme = NULL,
        .themes = NULL,
        .responsive_config = NULL,
        .animation_config = NULL,
        .export_config = NULL,
        .cache = NULL,
        .performance_log = NULL,

        # Private methods
        log_performance = function(operation, time_ms) {
            private$.performance_log[[length(private$.performance_log) + 1]] <- list(
                operation = operation,
                time_ms = time_ms,
                timestamp = Sys.time()
            )
        },

        load_default_themes = function() {
            list(
                light = private$create_light_theme(),
                dark = private$create_dark_theme(),
                print = private$create_print_theme()
            )
        },

        create_light_theme = function() {
            list(
                colors = list(
                    # Primary brand colors
                    primary = "#1B3A6B",
                    primary_light = "#2A4D8C",
                    primary_dark = "#0F2442",

                    # Secondary colors
                    secondary = "#5B8FA3",
                    secondary_light = "#7AA5B8",
                    secondary_dark = "#446B7C",

                    # Accent colors
                    accent = "#F39B3C",
                    accent_light = "#F5B165",
                    accent_dark = "#E08426",

                    # Semantic colors
                    success = "#28a745",
                    success_light = "#5DD879",
                    danger = "#dc3545",
                    danger_light = "#E66373",
                    warning = "#ffc107",
                    warning_light = "#FFD54F",
                    info = "#17a2b8",

                    # Neutral colors
                    light_gray = "#F8F9FA",
                    medium_gray = "#6C757D",
                    dark_gray = "#343A40",

                    # Additional colors
                    background = "#FFFFFF",
                    surface = "#F8F9FA",
                    text_primary = "#343A40",
                    text_secondary = "#6C757D",
                    border = "#DEE2E6",

                    # Gradient colors
                    gradient_start = "#E8F4F8",
                    gradient_end = "#1B3A6B",

                    # Categorical palette
                    categorical = c("#1B3A6B", "#5B8FA3", "#F39B3C", "#28a745",
                                    "#dc3545", "#17a2b8", "#6610F2", "#E83E8C",
                                    "#20C997", "#FD7E14", "#795548", "#607D8B",
                                    "#FF6B6B", "#4ECDC4", "#45B7D1", "#96CEB4",
                                    "#FFA07A", "#98D8C8", "#FFD700", "#DDA0DD")
                ),

                fonts = list(
                    primary = private$get_font_stack("sans"),
                    monospace = private$get_font_stack("mono"),
                    display = private$get_font_stack("display")
                ),

                components = list(
                    panel_bg = "white",
                    grid_color = "#F0F0F0",
                    grid_alpha = 0.5
                )
            )
        },

        create_dark_theme = function() {
            list(
                colors = list(
                    # Dark mode Insele colors
                    primary = "#4A7FBF",
                    primary_light = "#6FA0D4",
                    primary_dark = "#1B3A6B",

                    secondary = "#7BB0C6",
                    secondary_light = "#9BC5D6",
                    secondary_dark = "#5B8FA3",

                    accent = "#FFB366",
                    accent_light = "#FFC589",
                    accent_dark = "#F39B3C",

                    # Dark semantic colors
                    success = "#52C41A",
                    success_light = "#73D13D",
                    danger = "#FF4D4F",
                    danger_light = "#FF7875",
                    warning = "#FAAD14",
                    warning_light = "#FFC53D",
                    info = "#1890FF",

                    # Dark neutral colors
                    light_gray = "#3E4451",
                    medium_gray = "#B8BCC3",
                    dark_gray = "#E8E9EB",

                    background = "#1A1D23",
                    surface = "#262B34",
                    text_primary = "#E8E9EB",
                    text_secondary = "#B8BCC3",
                    border = "#3E4451",

                    gradient_start = "#1A1D23",
                    gradient_end = "#2A4D8C",

                    categorical = c("#4A7FBF", "#7BB0C6", "#FFB366", "#52C41A",
                                    "#FF4D4F", "#1890FF", "#B37FEB", "#FF85C0",
                                    "#36CFC9", "#FFA940", "#AD8B7F", "#8196A7",
                                    "#FF9999", "#7FE4DC", "#69C9E3", "#B3DFC8",
                                    "#FFB899", "#B3E4DB", "#FFE066", "#E6B3E6")
                ),

                fonts = list(
                    primary = private$get_font_stack("sans"),
                    monospace = private$get_font_stack("mono"),
                    display = private$get_font_stack("display")
                ),

                components = list(
                    panel_bg = "#262B34",
                    grid_color = "#3E4451",
                    grid_alpha = 0.3
                )
            )
        },

        create_print_theme = function() {
            list(
                colors = list(
                    # High contrast for print
                    primary = "#000000",
                    primary_light = "#333333",
                    primary_dark = "#000000",

                    secondary = "#666666",
                    secondary_light = "#999999",
                    secondary_dark = "#333333",

                    accent = "#000000",
                    accent_light = "#333333",
                    accent_dark = "#000000",

                    success = "#000000",
                    danger = "#000000",
                    warning = "#666666",
                    info = "#333333",

                    light_gray = "#F0F0F0",
                    medium_gray = "#666666",
                    dark_gray = "#000000",

                    background = "#FFFFFF",
                    surface = "#FFFFFF",
                    text_primary = "#000000",
                    text_secondary = "#333333",
                    border = "#000000",

                    gradient_start = "#FFFFFF",
                    gradient_end = "#F0F0F0",

                    categorical = c("#000000", "#333333", "#666666", "#999999",
                                    "#CCCCCC", "#000000", "#333333", "#666666")
                ),

                fonts = list(
                    primary = private$get_font_stack("serif"),
                    monospace = private$get_font_stack("mono"),
                    display = private$get_font_stack("serif")
                ),

                components = list(
                    panel_bg = "white",
                    grid_color = "#CCCCCC",
                    grid_alpha = 1,
                    line_width_multiplier = 1.5
                )
            )
        },

        build_theme = function(mode, base_size) {
            config <- private$.themes[[mode]]
            if (is.null(config)) {
                stop(sprintf("Theme mode '%s' not found", mode))
            }

            colors <- config$colors
            fonts <- config$fonts
            components <- config$components

            theme_minimal(base_size = base_size) +
                theme(
                    # Typography
                    text = element_text(
                        family = fonts$primary,
                        color = colors$text_primary
                    ),

                    plot.title = element_text(
                        size = rel(1.3),
                        face = "bold",
                        color = colors$primary,
                        margin = ggplot2::margin(t = 0, r = 0, b = base_size * 0.5, l = 0)
                    ),

                    plot.subtitle = element_text(
                        size = rel(1),
                        color = colors$text_secondary,
                        margin = ggplot2::margin(t = 0, r = 0, b = base_size * 0.7, l = 0)
                    ),

                    plot.caption = element_text(
                        size = rel(0.8),
                        color = colors$text_secondary,
                        hjust = 1,
                        margin = ggplot2::margin(t = base_size * 0.5, r = 0, b = 0, l = 0)
                    ),

                    # Axes
                    axis.title = element_text(
                        size = rel(1),
                        face = "bold",
                        color = colors$text_primary
                    ),

                    axis.text = element_text(
                        size = rel(0.9),
                        color = colors$text_secondary
                    ),

                    axis.line = element_line(
                        color = colors$border,
                        linewidth = 0.5
                    ),

                    # Grid
                    panel.grid.major = element_line(
                        color = components$grid_color,
                        linewidth = 0.5,
                        linetype = "solid"
                    ),

                    panel.grid.minor = element_blank(),

                    # Legend
                    legend.position = "bottom",
                    legend.background = element_rect(
                        fill = colors$background,
                        color = NA
                    ),

                    legend.key = element_rect(
                        fill = colors$background,
                        color = NA
                    ),

                    legend.title = element_text(
                        face = "bold",
                        size = rel(0.9)
                    ),

                    legend.text = element_text(
                        size = rel(0.85)
                    ),

                    # Panel
                    panel.background = element_rect(
                        fill = components$panel_bg,
                        color = NA
                    ),

                    panel.border = element_rect(
                        fill = NA,
                        color = colors$border,
                        linewidth = 1
                    ),

                    # Plot background
                    plot.background = element_rect(
                        fill = colors$background,
                        color = NA
                    ),

                    plot.margin = ggplot2::margin(
                        t = base_size * 1.5,
                        r = base_size * 1.5,
                        b = base_size * 1.5,
                        l = base_size * 1.5
                    ),

                    # Strip (facet labels)
                    strip.background = element_rect(
                        fill = colors$surface,
                        color = colors$border
                    ),

                    strip.text = element_text(
                        face = "bold",
                        color = colors$text_primary
                    )
                )
        },

        load_responsive_config = function() {
            list(
                mobile = list(
                    breakpoint = 768,
                    base_size = 10,
                    title_scale = 1.2,
                    margin_scale = 0.8
                ),
                tablet = list(
                    breakpoint = 1024,
                    base_size = 11,
                    title_scale = 1.25,
                    margin_scale = 0.9
                ),
                desktop = list(
                    breakpoint = 1920,
                    base_size = 12,
                    title_scale = 1.3,
                    margin_scale = 1
                ),
                large = list(
                    breakpoint = Inf,
                    base_size = 14,
                    title_scale = 1.35,
                    margin_scale = 1.1
                )
            )
        },

        get_responsive_size = function(device = NULL) {
            if (!is.null(device)) {
                config <- private$.responsive_config[[device]]
                if (!is.null(config)) {
                    return(config$base_size)
                }
            }

            # Auto-detect based on graphics device size
            if (dev.cur() != 1) {
                dev_size <- dev.size("px")
                width <- dev_size[1]

                for (device_name in names(private$.responsive_config)) {
                    config <- private$.responsive_config[[device_name]]
                    if (width <= config$breakpoint) {
                        return(config$base_size)
                    }
                }
            }

            # Default to desktop
            return(private$.responsive_config$desktop$base_size)
        },

        load_animation_config = function() {
            list(
                default = list(
                    duration = "0.3s",
                    easing = "cubic-bezier(0.4, 0, 0.2, 1)",
                    delay = "0s",
                    properties = c("all")
                ),
                hover = list(
                    duration = "0.2s",
                    easing = "ease-out",
                    properties = c("transform", "box-shadow", "background-color")
                ),
                fade = list(
                    duration = "0.5s",
                    easing = "ease-in-out",
                    properties = c("opacity")
                ),
                slide = list(
                    duration = "0.4s",
                    easing = "cubic-bezier(0.25, 0.46, 0.45, 0.94)",
                    properties = c("transform", "opacity")
                ),
                chart = list(
                    duration = "0.8s",
                    easing = "cubic-bezier(0.65, 0, 0.35, 1)",
                    stagger = "50ms"
                )
            )
        },

        load_export_config = function() {
            list(
                pdf = list(
                    base_size = 10,
                    dpi = 300,
                    background = "white",
                    line_width_multiplier = 1.5,
                    embed_fonts = TRUE,
                    colorspace = "sRGB"
                ),
                png = list(
                    base_size = 12,
                    dpi = 150,
                    background = "white",
                    antialias = TRUE
                ),
                svg = list(
                    base_size = 12,
                    background = "transparent",
                    embed_fonts = FALSE,
                    standalone = TRUE
                ),
                pptx = list(
                    base_size = 14,
                    dpi = 150,
                    background = "white",
                    aspect_ratio = "16:9"
                ),
                html = list(
                    base_size = 11,
                    background = "transparent",
                    responsive = TRUE,
                    interactive = TRUE
                ),
                default = list(
                    base_size = 12,
                    dpi = 150,
                    background = "white"
                )
            )
        },

        get_font_stack = function(type = "sans") {
            # Check system and return appropriate font stack
            sys_name <- Sys.info()[["sysname"]]

            if (sys_name == "Windows") {
                switch(type,
                       sans = "sans",  # Use R's default sans font
                       serif = "serif",
                       mono = "mono",
                       display = "sans",
                       "sans-serif"
                )
            } else {
                switch(type,
                       sans = "Helvetica, Arial, sans-serif",
                       serif = "Georgia, Times, serif",
                       mono = "Consolas, Monaco, monospace",
                       display = "Helvetica, Arial, sans-serif",
                       "sans-serif"
                )
            }
        }
    )
)

# ================================================================================
# GLOBAL THEME INSTANCE & LEGACY COMPATIBILITY
# ================================================================================

# Create global theme manager instance
.theme_manager <- ThemeManager$new()

# Export legacy palette for backward compatibility
#' @export
insele_palette <- .theme_manager$get_colors("light")

# Legacy function for backward compatibility
#' @export
create_insele_theme <- function(base_size = 11) {
    .theme_manager$get_theme(base_size = base_size)
}

# Legacy theme object
#' @export
insele_theme <- create_insele_theme()

# Legacy label configuration
#' @export
LABEL_CONFIG <- list(
    max_labels_primary = 10,
    max_labels_secondary = 6,
    max_overlaps = 20,
    standard_size = 3.5,
    small_size = 3
)

# ================================================================================
# THEME UTILITY FUNCTIONS
# ================================================================================

#' @title Apply Theme to Plot
#' @description Enhanced function to apply theme with device detection
#' @export
apply_insele_theme <- function(plot, mode = NULL, device = NULL, export_format = NULL) {
    # Get appropriate theme
    if (!is.null(export_format)) {
        theme_config <- .theme_manager$get_export_theme(export_format)
        theme <- theme_config$theme
    } else {
        theme <- .theme_manager$get_theme(mode = mode, device = device)
    }

    # Apply theme
    plot + theme
}

#' @title Get Color Palette
#' @description Get color palette for current or specified theme
#' @export
get_insele_colors <- function(mode = NULL, color_type = "categorical", n = NULL) {
    colors <- .theme_manager$get_colors(mode)

    if (color_type == "categorical") {
        # Use categorical palette if available
        if (!is.null(colors$categorical)) {
            base_colors <- colors$categorical
        } else {
            base_colors <- c(
                colors$primary, colors$secondary, colors$accent,
                colors$success, colors$info, colors$warning
            )
        }

        # Handle n parameter
        if (is.null(n)) {
            # Return all base colors if n not specified
            return(base_colors)
        } else if (n > length(base_colors)) {
            # Generate additional colors using colorRampPalette
            ramp <- colorRampPalette(base_colors)
            return(ramp(n))
        } else {
            # Return first n colors
            return(base_colors[1:n])
        }
    } else if (color_type == "diverging") {
        return(c(colors$danger, "white", colors$success))
    } else if (color_type == "sequential") {
        return(c(colors$gradient_start, colors$gradient_end))
    } else {
        return(colors)
    }
}

#' @title Create Gradient Fill
#' @description Create gradient fill for advanced visualizations
#' @export
create_gradient_fill <- function(low = NULL, high = NULL, mid = NULL, mode = NULL) {
    colors <- get_insele_colors(mode = mode)

    if (is.null(mid)) {
        scale_fill_gradient(
            low = low %||% colors$gradient_start,
            high = high %||% colors$gradient_end,
            guide = guide_colorbar(
                barwidth = 15,
                barheight = 0.5,
                title.position = "top",
                title.hjust = 0.5
            )
        )
    } else {
        scale_fill_gradient2(
            low = low %||% colors$danger,
            mid = mid %||% "white",
            high = high %||% colors$success,
            midpoint = 0,
            guide = guide_colorbar(
                barwidth = 15,
                barheight = 0.5,
                title.position = "top",
                title.hjust = 0.5
            )
        )
    }
}

#' @title Get Animation CSS
#' @description Generate CSS for animations
#' @export
get_animation_css <- function(element = NULL) {
    anim <- .theme_manager$get_animations(element)

    css <- sprintf(
        "transition: %s %s %s;",
        paste(anim$properties, collapse = ", "),
        anim$duration,
        anim$easing
    )

    if (!is.null(anim$delay) && anim$delay != "0s") {
        css <- paste0(css, sprintf(" transition-delay: %s;", anim$delay))
    }

    return(css)
}

#' @title Theme Inheritance System
#' @description Create custom theme inheriting from base
#' @export
create_custom_theme <- function(base_mode = "light", modifications = list()) {
    base_theme <- .theme_manager$get_theme(mode = base_mode)

    # Apply modifications
    if (length(modifications) > 0) {
        base_theme <- base_theme + do.call(theme, modifications)
    }

    return(base_theme)
}

#' @title Validate WCAG Contrast
#' @description Check if color combination meets WCAG AA standards
#' @export
check_wcag_contrast <- function(foreground, background, level = "AA") {
    # Convert colors to RGB
    fg_rgb <- col2rgb(foreground) / 255
    bg_rgb <- col2rgb(background) / 255

    # Calculate relative luminance
    calc_luminance <- function(rgb) {
        rgb <- ifelse(rgb <= 0.03928, rgb / 12.92, ((rgb + 0.055) / 1.055) ^ 2.4)
        0.2126 * rgb[1] + 0.7152 * rgb[2] + 0.0722 * rgb[3]
    }

    L1 <- calc_luminance(fg_rgb)
    L2 <- calc_luminance(bg_rgb)

    # Calculate contrast ratio
    contrast_ratio <- (max(L1, L2) + 0.05) / (min(L1, L2) + 0.05)

    # Check against WCAG standards
    min_ratio <- if (level == "AAA") 7 else 4.5
    passes <- contrast_ratio >= min_ratio

    return(list(
        ratio = round(contrast_ratio, 2),
        passes = passes,
        level = level,
        recommendation = if (!passes) {
            sprintf("Increase contrast to at least %.1f:1", min_ratio)
        } else {
            "Contrast meets WCAG standards"
        }
    ))
}

# ================================================================================
# SIGNAL THRESHOLD CONSTANTS (Single Source of Truth)
# ================================================================================
# These thresholds define trading signal strength across the entire dashboard.
# Both the Market Intelligence card and Yield Curve panel use these values.
#
# Z-Score Interpretation:
#   - |Z| > 2.0 = Strong signal (statistically significant at 95% confidence)
#   - |Z| > 1.5 = Moderate signal (actionable, worth investigating)
#   - |Z| > 1.0 = Weak signal (normal variation)
#   - |Z| < 1.0 = No signal (within normal bounds)
# ================================================================================

#' @export
SIGNAL_THRESHOLDS <- list(
    strong = 2.0,      # |Z| > 2.0 = Strong signal (statistically significant)
    moderate = 1.5,    # |Z| > 1.5 = Moderate signal (actionable)
    weak = 1.0         # |Z| > 1.0 = Weak signal
)

#' Calculate unified trading signals from curve data
#' @description Single source of truth for signal calculations used across dashboard
#' @param curve_data Data frame with z_score, spread_bps/spread_to_curve, and bond columns
#' @param spread_col Name of spread column (default: "spread_bps")
#' @param bond_col Name of bond identifier column (default: "bond")
#' @return List containing signal counts and key trade recommendations
#' @export
calculate_unified_signals <- function(curve_data,
                                       spread_col = "spread_bps",
                                       bond_col = "bond") {
    # Validate input
    if (is.null(curve_data) || nrow(curve_data) == 0) {
        return(list(
            strong_count = 0,
            moderate_count = 0,
            weak_count = 0,
            actionable_bonds = character(0),
            cheapest = list(bond = NA, spread = NA),
            richest = list(bond = NA, spread = NA),
            highest_conviction = list(bond = NA, z_score = NA),
            signal_summary = "No data available"
        ))
    }

    # Handle alternative spread column names
    if (!spread_col %in% names(curve_data)) {
        if ("spread_to_curve" %in% names(curve_data)) {
            spread_col <- "spread_to_curve"
        } else {
            spread_col <- NULL
        }
    }

    # Calculate Z-scores if not present
    if (!"z_score" %in% names(curve_data)) {
        if (!is.null(spread_col) && spread_col %in% names(curve_data)) {
            spreads <- curve_data[[spread_col]]
            spread_sd <- sd(spreads, na.rm = TRUE)
            if (!is.na(spread_sd) && spread_sd > 0) {
                curve_data$z_score <- (spreads - mean(spreads, na.rm = TRUE)) / spread_sd
            } else {
                curve_data$z_score <- 0
            }
        } else {
            curve_data$z_score <- 0
        }
    }

    # Filter valid rows
    valid_data <- curve_data[!is.na(curve_data$z_score), ]

    if (nrow(valid_data) == 0) {
        return(list(
            strong_count = 0,
            moderate_count = 0,
            weak_count = 0,
            actionable_bonds = character(0),
            cheapest = list(bond = NA, spread = NA),
            richest = list(bond = NA, spread = NA),
            highest_conviction = list(bond = NA, z_score = NA),
            signal_summary = "No valid z-score data"
        ))
    }

    # Count signals by threshold using the centralized constants
    zscore_abs <- abs(valid_data$z_score)
    strong_count <- sum(zscore_abs > SIGNAL_THRESHOLDS$strong, na.rm = TRUE)
    moderate_count <- sum(zscore_abs > SIGNAL_THRESHOLDS$moderate, na.rm = TRUE)  # Includes strong
    weak_count <- sum(zscore_abs > SIGNAL_THRESHOLDS$weak, na.rm = TRUE)  # Includes moderate and strong

    # Identify actionable bonds (|Z| > 1.5 = moderate threshold)
    actionable_mask <- zscore_abs > SIGNAL_THRESHOLDS$moderate
    actionable_bonds <- if (any(actionable_mask)) {
        valid_data[[bond_col]][actionable_mask]
    } else {
        character(0)
    }

    # Find extremes (cheapest, richest, highest conviction)
    cheapest <- list(bond = NA, spread = NA)
    richest <- list(bond = NA, spread = NA)
    highest_conviction <- list(bond = NA, z_score = NA)

    if (!is.null(spread_col) && spread_col %in% names(valid_data)) {
        spread_data <- valid_data[[spread_col]]
        if (any(!is.na(spread_data))) {
            cheapest_idx <- which.max(spread_data)
            richest_idx <- which.min(spread_data)
            cheapest <- list(
                bond = valid_data[[bond_col]][cheapest_idx],
                spread = spread_data[cheapest_idx]
            )
            richest <- list(
                bond = valid_data[[bond_col]][richest_idx],
                spread = spread_data[richest_idx]
            )
        }
    }

    highest_conviction_idx <- which.max(zscore_abs)
    if (length(highest_conviction_idx) > 0) {
        highest_conviction <- list(
            bond = valid_data[[bond_col]][highest_conviction_idx],
            z_score = valid_data$z_score[highest_conviction_idx]
        )
    }

    # Create summary text
    signal_summary <- sprintf(
        "%d strong (|Z|>%.1f), %d moderate (|Z|>%.1f)",
        strong_count, SIGNAL_THRESHOLDS$strong,
        moderate_count - strong_count, SIGNAL_THRESHOLDS$moderate
    )

    return(list(
        strong_count = strong_count,
        moderate_count = moderate_count,
        weak_count = weak_count,
        actionable_bonds = actionable_bonds,
        cheapest = cheapest,
        richest = richest,
        highest_conviction = highest_conviction,
        signal_summary = signal_summary
    ))
}

# ================================================================================
# INITIALIZATION MESSAGE
# ================================================================================

message("✓ Insele Capital Partners Theme System v3.0 loaded")
message(sprintf("  Current theme: %s", .theme_manager$get_current_mode()))
message("  Themes available: light, dark, print")
message("  Use .theme_manager$switch_theme() to change themes")