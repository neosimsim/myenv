$env.config = {
  show_banner: false

  completions: {
    case_sensitive: true
    quick: true
    partial: true
    algorithm: "prefix"
  }

}

use std/config light-theme
$env.config.color_config = (light-theme)
$env.LS_COLORS = (vivid generate ayu)
