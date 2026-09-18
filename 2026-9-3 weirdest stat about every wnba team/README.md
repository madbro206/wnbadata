# the weirdest stat about every wnba team

15 videos, september 2026. [TikTok](https://www.tiktok.com/@wnbadata) · [Instagram](https://www.instagram.com/wnbadata/)

One video per team, posted daily during the 2026 FIBA World Cup break. Each file
builds every chart and table used in that video, in the order they appear.

| day | team | file | the stat |
| --- | --- | --- | --- |
| 1 | Golden State Valkyries | `valks_charts.R` | starters fg% |
| 2 | Atlanta Dream | `dream_charts.R` | fourth quarter dominance |
| 3 | Washington Mystics | `mystics_charts.R` | young and lucky: close game record |
| 4 | Las Vegas Aces | `aces_charts.R` | blowouts in every direction |
| 5 | Toronto Tempo | `tempo_charts.R` | usage after the sykes injury |
| 6 | Seattle Storm | `storm_charts.R` | never winning a close game |
| 7 | Indiana Fever | `fever_charts.R` | high fg% but low ast rate |
| 8 | Connecticut Sun | `sun_charts.R` | most two pointers |
| 9 | Chicago Sky | `sky_charts.R` | three point guards over 34 |
| 10 | New York Liberty | `liberty_charts.R` | breanna stewart's july |
| 11 | Los Angeles Sparks | `sparks_charts.R` | first quarter defense |
| 12 | Portland Fire | `fire_charts.R` | 2000 vs 2026 expansion seasons |
| 13 | Dallas Wings | `wings_charts.R` | home vs away |
| 14 | Phoenix Mercury | `mercury_charts.R` | alyssa thomas set up a quarter of their baskets |
| 15 | Minnesota Lynx | `lynx_charts.R` | are the lynx worse with napheesa collier? |

## running these

```r
install.packages(c("wehoop", "dplyr", "tidyr", "ggplot2", "ggrepel", "gt"))
source("lynx_charts.R")
```

Each file is standalone. Most numbers compute live from
[wehoop](https://wehoop.sportsdataverse.org/). Figures that came from
Her Hoop Stats or elsewhere are typed in as small data frames with a comment saying what they
are and when they were verified.

Two wehoop filters show up in every file: the Commissioner's Cup final
sits inside `season_type == 2` but does not count toward the regular season, and
All-Star rosters appear as their own teams, so I filter those out.

## a note on the numbers

Everything is as of the FIBA break (through 8/30/2026) unless a comment says
otherwise, so these will not match end of season totals (each team had four more regular season games after this series ended). Where a number was cross checked against a second source, the comment says which one and on what
date. Where a stat is fragile or a small sample, the comment says that too.

## credits

Data from [wehoop](https://wehoop.sportsdataverse.org/) and
[Her Hoop Stats](https://herhoopstats.com/).

I built these with [Claude Code](https://claude.com/claude-code). It did much of the data verification (catching bad, inaccurate, or incomplete stats before they made it into a video concept), chart iteration (lots of this!), and cleanup. The analysis, the angles and everything said on camera is my script and my voice.
