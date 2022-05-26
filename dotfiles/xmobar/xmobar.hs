import Xmobar

config :: Config
config =
  defaultConfig
    { font = "xft:Free Sans:pixelsize=26:antialias=true:autohint=true",
      bgColor = "#fdf6e3",
      fgColor = "#073642",
      position = TopW L 100,
      commands =
        [ Run $
            Cpu
              ["-L", "3", "-H", "50", "--normal", "black", "--high", "red"]
              10,
          Run $ Memory ["-t", "Mem: <usedratio>%"] 10,
          Run $ Date "%a %b %_d %H:%M" "date" 10,
          Run $ StdinReader,
          Run $
            Battery
              [ "--template",
                "Batt: <acstatus>",
                "--Low",
                "10", -- units: %
                "--High",
                "80", -- units: %
                "--low",
                "darkred",
                "--normal",
                "darkorange",
                "--high",
                "darkgreen",
                "--", -- battery specific options
                -- discharging status
                "-o",
                "<left> % (<timeleft>)",
                -- AC "on" status
                "-O",
                "Charging",
                -- charged status
                "-i",
                "Charged"
              ]
              50,
          Run $ Alsa "default" "Master" ["-t", "Vol: <volume>%<status>", "--", "--on", "", "--off", " [off]"],
          Run $ Alsa "default" "Capture" ["-t", "Mic: <volume>%<status>", "--", "--on", "", "--off", " [off]"],
          Run $ DynNetwork [] 50
        ],
      sepChar = "%",
      alignSep = "}{",
      template =
        "%StdinReader% }{ %battery% | %alsa:default:Master% %alsa:default:Capture% |  %cpu% | %memory% | %dynnetwork% || <fc=#073642>%date%</fc>"
    }

main :: IO ()
main = xmobar config
