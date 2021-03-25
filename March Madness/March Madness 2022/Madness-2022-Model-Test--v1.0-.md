---
title: "Madness 2022 Model Test"
author: "Ken Harmon"
date: "2021 March 25"
output:
  html_document:  
    keep_md: true
    code_folding: hide
    fig_height: 6
    fig_width: 12
    fig_align: 'center'
editor_options: 
  chunk_output_type: console
---

# {.tabset .tabset-fade}







## Seed History


```r
#Load in Historical Games from 1985 through 2022
AllGames <- read.csv("All Games.csv")
AllGames <- AllGames %>% filter(Round != "PI")
#Build Seed History
seed.history <- data.frame(rbind(table(AllGames$Winning.Seed,AllGames$Round)))%>%select(2:7)
seed.history$exp <- rowSums(seed.history)/144
gt(seed.history,,,TRUE)%>% 
   data_color(
    columns = 2:8, 
    colors = scales::col_numeric(
      palette = c("white","blue") %>% as.character(),
      domain = NULL
    )
  ) 
```

<!--html_preserve--><style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#ahqqjtqkhy .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#ahqqjtqkhy .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#ahqqjtqkhy .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#ahqqjtqkhy .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#ahqqjtqkhy .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ahqqjtqkhy .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#ahqqjtqkhy .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#ahqqjtqkhy .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#ahqqjtqkhy .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#ahqqjtqkhy .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#ahqqjtqkhy .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#ahqqjtqkhy .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#ahqqjtqkhy .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#ahqqjtqkhy .gt_from_md > :first-child {
  margin-top: 0;
}

#ahqqjtqkhy .gt_from_md > :last-child {
  margin-bottom: 0;
}

#ahqqjtqkhy .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#ahqqjtqkhy .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#ahqqjtqkhy .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ahqqjtqkhy .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#ahqqjtqkhy .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ahqqjtqkhy .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#ahqqjtqkhy .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#ahqqjtqkhy .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ahqqjtqkhy .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#ahqqjtqkhy .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#ahqqjtqkhy .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#ahqqjtqkhy .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#ahqqjtqkhy .gt_left {
  text-align: left;
}

#ahqqjtqkhy .gt_center {
  text-align: center;
}

#ahqqjtqkhy .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#ahqqjtqkhy .gt_font_normal {
  font-weight: normal;
}

#ahqqjtqkhy .gt_font_bold {
  font-weight: bold;
}

#ahqqjtqkhy .gt_font_italic {
  font-style: italic;
}

#ahqqjtqkhy .gt_super {
  font-size: 65%;
}

#ahqqjtqkhy .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="ahqqjtqkhy" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1"></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">X1</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">X2</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">X3</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">X4</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">X5</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">X6</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">exp</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr>
      <td class="gt_row gt_left gt_stub">1</td>
      <td class="gt_row gt_center" style="background-color: #0000FF; color: #FFFFFF;">143</td>
      <td class="gt_row gt_center" style="background-color: #0000FF; color: #FFFFFF;">123</td>
      <td class="gt_row gt_center" style="background-color: #0000FF; color: #FFFFFF;">97</td>
      <td class="gt_row gt_center" style="background-color: #0000FF; color: #FFFFFF;">57</td>
      <td class="gt_row gt_center" style="background-color: #0000FF; color: #FFFFFF;">34</td>
      <td class="gt_row gt_center" style="background-color: #0000FF; color: #FFFFFF;">22</td>
      <td class="gt_row gt_right" style="background-color: #0000FF; color: #FFFFFF;">3.305555556</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">2</td>
      <td class="gt_row gt_center" style="background-color: #3B1DFF; color: #FFFFFF;">135</td>
      <td class="gt_row gt_center" style="background-color: #8154FF; color: #FFFFFF;">91</td>
      <td class="gt_row gt_center" style="background-color: #9367FF; color: #000000;">64</td>
      <td class="gt_row gt_center" style="background-color: #B189FF; color: #000000;">29</td>
      <td class="gt_row gt_center" style="background-color: #C7A6FF; color: #000000;">13</td>
      <td class="gt_row gt_center" style="background-color: #DFCAFF; color: #000000;">5</td>
      <td class="gt_row gt_right" style="background-color: #895CFF; color: #000000;">2.340277778</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">3</td>
      <td class="gt_row gt_center" style="background-color: #6138FF; color: #FFFFFF;">122</td>
      <td class="gt_row gt_center" style="background-color: #9E72FF; color: #000000;">75</td>
      <td class="gt_row gt_center" style="background-color: #C9A9FF; color: #000000;">36</td>
      <td class="gt_row gt_center" style="background-color: #D5B9FF; color: #000000;">17</td>
      <td class="gt_row gt_center" style="background-color: #D1B4FF; color: #000000;">11</td>
      <td class="gt_row gt_center" style="background-color: #E6D4FF; color: #000000;">4</td>
      <td class="gt_row gt_right" style="background-color: #A87EFF; color: #000000;">1.840277778</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">4</td>
      <td class="gt_row gt_center" style="background-color: #7246FF; color: #FFFFFF;">114</td>
      <td class="gt_row gt_center" style="background-color: #AB81FF; color: #000000;">67</td>
      <td class="gt_row gt_center" style="background-color: #E1CCFF; color: #000000;">21</td>
      <td class="gt_row gt_center" style="background-color: #DFCAFF; color: #000000;">13</td>
      <td class="gt_row gt_center" style="background-color: #F3EAFF; color: #000000;">3</td>
      <td class="gt_row gt_center" style="background-color: #F9F4FF; color: #000000;">1</td>
      <td class="gt_row gt_right" style="background-color: #BA95FF; color: #000000;">1.520833333</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">5</td>
      <td class="gt_row gt_center" style="background-color: #9669FF; color: #000000;">93</td>
      <td class="gt_row gt_center" style="background-color: #C5A2FF; color: #000000;">49</td>
      <td class="gt_row gt_center" style="background-color: #F3E9FF; color: #000000;">9</td>
      <td class="gt_row gt_center" style="background-color: #EEE2FF; color: #000000;">7</td>
      <td class="gt_row gt_center" style="background-color: #F3EAFF; color: #000000;">3</td>
      <td class="gt_row gt_center" style="background-color: #FFFFFF; color: #000000;">0</td>
      <td class="gt_row gt_right" style="background-color: #CFB0FF; color: #000000;">1.118055556</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">6</td>
      <td class="gt_row gt_center" style="background-color: #9C70FF; color: #000000;">89</td>
      <td class="gt_row gt_center" style="background-color: #CDAEFF; color: #000000;">43</td>
      <td class="gt_row gt_center" style="background-color: #EBDDFF; color: #000000;">14</td>
      <td class="gt_row gt_center" style="background-color: #F8F3FF; color: #000000;">3</td>
      <td class="gt_row gt_center" style="background-color: #F7F1FF; color: #000000;">2</td>
      <td class="gt_row gt_center" style="background-color: #F9F4FF; color: #000000;">1</td>
      <td class="gt_row gt_right" style="background-color: #D2B5FF; color: #000000;">1.055555556</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">7</td>
      <td class="gt_row gt_center" style="background-color: #9F73FF; color: #000000;">87</td>
      <td class="gt_row gt_center" style="background-color: #DFCAFF; color: #000000;">28</td>
      <td class="gt_row gt_center" style="background-color: #F1E7FF; color: #000000;">10</td>
      <td class="gt_row gt_center" style="background-color: #F8F3FF; color: #000000;">3</td>
      <td class="gt_row gt_center" style="background-color: #FBF8FF; color: #000000;">1</td>
      <td class="gt_row gt_center" style="background-color: #F9F4FF; color: #000000;">1</td>
      <td class="gt_row gt_right" style="background-color: #D9BFFF; color: #000000;">0.902777778</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">8</td>
      <td class="gt_row gt_center" style="background-color: #B48DFF; color: #000000;">71</td>
      <td class="gt_row gt_center" style="background-color: #F0E4FF; color: #000000;">14</td>
      <td class="gt_row gt_center" style="background-color: #F4ECFF; color: #000000;">8</td>
      <td class="gt_row gt_center" style="background-color: #F3EAFF; color: #000000;">5</td>
      <td class="gt_row gt_center" style="background-color: #F3EAFF; color: #000000;">3</td>
      <td class="gt_row gt_center" style="background-color: #F9F4FF; color: #000000;">1</td>
      <td class="gt_row gt_right" style="background-color: #E2CDFF; color: #000000;">0.708333333</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">9</td>
      <td class="gt_row gt_center" style="background-color: #B28AFF; color: #000000;">73</td>
      <td class="gt_row gt_center" style="background-color: #F7F2FF; color: #000000;">7</td>
      <td class="gt_row gt_center" style="background-color: #FAF5FF; color: #000000;">4</td>
      <td class="gt_row gt_center" style="background-color: #FDFBFF; color: #000000;">1</td>
      <td class="gt_row gt_center" style="background-color: #FFFFFF; color: #000000;">0</td>
      <td class="gt_row gt_center" style="background-color: #FFFFFF; color: #000000;">0</td>
      <td class="gt_row gt_right" style="background-color: #E7D5FF; color: #000000;">0.590277778</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">10</td>
      <td class="gt_row gt_center" style="background-color: #C5A3FF; color: #000000;">57</td>
      <td class="gt_row gt_center" style="background-color: #E5D3FF; color: #000000;">23</td>
      <td class="gt_row gt_center" style="background-color: #F4ECFF; color: #000000;">8</td>
      <td class="gt_row gt_center" style="background-color: #FDFBFF; color: #000000;">1</td>
      <td class="gt_row gt_center" style="background-color: #FFFFFF; color: #000000;">0</td>
      <td class="gt_row gt_center" style="background-color: #FFFFFF; color: #000000;">0</td>
      <td class="gt_row gt_right" style="background-color: #E6D3FF; color: #000000;">0.618055556</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">11</td>
      <td class="gt_row gt_center" style="background-color: #C9A8FF; color: #000000;">54</td>
      <td class="gt_row gt_center" style="background-color: #E4D1FF; color: #000000;">24</td>
      <td class="gt_row gt_center" style="background-color: #F4ECFF; color: #000000;">8</td>
      <td class="gt_row gt_center" style="background-color: #F6EEFF; color: #000000;">4</td>
      <td class="gt_row gt_center" style="background-color: #FFFFFF; color: #000000;">0</td>
      <td class="gt_row gt_center" style="background-color: #FFFFFF; color: #000000;">0</td>
      <td class="gt_row gt_right" style="background-color: #E5D3FF; color: #000000;">0.625000000</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">12</td>
      <td class="gt_row gt_center" style="background-color: #CCADFF; color: #000000;">51</td>
      <td class="gt_row gt_center" style="background-color: #E6D5FF; color: #000000;">22</td>
      <td class="gt_row gt_center" style="background-color: #FEFDFF; color: #000000;">1</td>
      <td class="gt_row gt_center" style="background-color: #FFFFFF; color: #000000;">0</td>
      <td class="gt_row gt_center" style="background-color: #FFFFFF; color: #000000;">0</td>
      <td class="gt_row gt_center" style="background-color: #FFFFFF; color: #000000;">0</td>
      <td class="gt_row gt_right" style="background-color: #EADBFF; color: #000000;">0.513888889</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">13</td>
      <td class="gt_row gt_center" style="background-color: #E2CDFF; color: #000000;">31</td>
      <td class="gt_row gt_center" style="background-color: #F9F3FF; color: #000000;">6</td>
      <td class="gt_row gt_center" style="background-color: #FFFFFF; color: #000000;">0</td>
      <td class="gt_row gt_center" style="background-color: #FFFFFF; color: #000000;">0</td>
      <td class="gt_row gt_center" style="background-color: #FFFFFF; color: #000000;">0</td>
      <td class="gt_row gt_center" style="background-color: #FFFFFF; color: #000000;">0</td>
      <td class="gt_row gt_right" style="background-color: #F5EDFF; color: #000000;">0.256944444</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">14</td>
      <td class="gt_row gt_center" style="background-color: #EBDCFF; color: #000000;">22</td>
      <td class="gt_row gt_center" style="background-color: #FDFBFF; color: #000000;">2</td>
      <td class="gt_row gt_center" style="background-color: #FFFFFF; color: #000000;">0</td>
      <td class="gt_row gt_center" style="background-color: #FFFFFF; color: #000000;">0</td>
      <td class="gt_row gt_center" style="background-color: #FFFFFF; color: #000000;">0</td>
      <td class="gt_row gt_center" style="background-color: #FFFFFF; color: #000000;">0</td>
      <td class="gt_row gt_right" style="background-color: #F9F4FF; color: #000000;">0.166666667</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">15</td>
      <td class="gt_row gt_center" style="background-color: #F8F2FF; color: #000000;">9</td>
      <td class="gt_row gt_center" style="background-color: #FDFBFF; color: #000000;">2</td>
      <td class="gt_row gt_center" style="background-color: #FFFFFF; color: #000000;">0</td>
      <td class="gt_row gt_center" style="background-color: #FFFFFF; color: #000000;">0</td>
      <td class="gt_row gt_center" style="background-color: #FFFFFF; color: #000000;">0</td>
      <td class="gt_row gt_center" style="background-color: #FFFFFF; color: #000000;">0</td>
      <td class="gt_row gt_right" style="background-color: #FCFAFF; color: #000000;">0.076388889</td>
    </tr>
    <tr>
      <td class="gt_row gt_left gt_stub">16</td>
      <td class="gt_row gt_center" style="background-color: #FFFFFF; color: #000000;">1</td>
      <td class="gt_row gt_center" style="background-color: #FFFFFF; color: #000000;">0</td>
      <td class="gt_row gt_center" style="background-color: #FFFFFF; color: #000000;">0</td>
      <td class="gt_row gt_center" style="background-color: #FFFFFF; color: #000000;">0</td>
      <td class="gt_row gt_center" style="background-color: #FFFFFF; color: #000000;">0</td>
      <td class="gt_row gt_center" style="background-color: #FFFFFF; color: #000000;">0</td>
      <td class="gt_row gt_right" style="background-color: #FFFFFF; color: #000000;">0.006944444</td>
    </tr>
  </tbody>
  
  
</table></div><!--/html_preserve-->

## Team History


```r
losers <- data.frame(rbind(table(AllGames$Loser,AllGames$Year)))
losers <- losers %>% add_rownames()
winners <- data.frame(rbind(table(AllGames$Winner,AllGames$Year)))
winners[winners > 0] <- winners[winners > 0] + 1
winners <- winners %>% add_rownames()



b2b <- left_join(losers,winners,by=c("rowname"="rowname"))
b2b[is.na(b2b)] <- 0
team.history <- cbind(b2b$rowname,b2b[,2:37]+b2b[,38:73])
colnames(team.history) <- c("team",seq(1985,2019,1),2021)
rownames(team.history) <- team.history$team
team.history <- team.history[-1,-1]
scale.v <- seq(.5,1,.5/35)
team.history$Exp <- colSums(t(team.history) * scale.v)
team.history[,1:36] <- team.history[,1:36] - 1
team.history[team.history == -1] <- NA
datatable(team.history)
```

<!--html_preserve--><div id="htmlwidget-73292f30f3d6be5b948c" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-73292f30f3d6be5b948c">{"x":{"filter":"none","data":[["Abilene Christian","Air Force","Akron","Alabama","Alabama St.","Albany","Alcorn St.","American","Appalachian St.","Arizona","Arizona St.","Arkansas","Arkansas Pine Bluff","Arkansas St.","Auburn","Austin Peay","Ball St.","Baylor","Belmont","Binghamton","Boise St.","Boston College","Boston University","Bradley","Brown","Bucknell","Buffalo","Butler","BYU","Cal Poly","Cal St. Bakersfield","Cal St. Fullerton","Cal St. Northridge","California","Campbell","Canisius","Central Connecticut","Central Michigan","Charleston","Charleston Southern","Charlotte","Chattanooga","Cincinnati","Clemson","Cleveland St.","Coastal Carolina","Colgate","Colorado","Colorado St.","Connecticut","Coppin St.","Cornell","Creighton","Davidson","Dayton","Delaware","Delaware St.","DePaul","Detroit","Drake","Drexel","Duke","East Carolina","East Tennessee St.","Eastern Illinois","Eastern Kentucky","Eastern Michigan","Eastern Washington","Evansville","Fairfield","Fairleigh Dickinson","FIU","Florida","Florida A&amp;M","Florida Atlantic","Florida Gulf Coast","Florida St.","Fordham","Fresno St.","Gardner Webb","George Mason","George Washington","Georgetown","Georgia","Georgia Southern","Georgia St.","Georgia Tech","Gonzaga","Grand Canyon","Green Bay","Hampton","Hartford","Harvard","Hawaii","Hofstra","Holy Cross","Houston","Howard","Idaho","Idaho St.","Illinois","Illinois Chicago","Illinois St.","Indiana","Indiana St.","Iona","Iowa","Iowa St.","IUPUI","Jackson St.","Jacksonville","Jacksonville St.","James Madison","Kansas","Kansas St.","Kent St.","Kentucky","La Salle","Lafayette","Lamar","Lehigh","Liberty","Lipscomb","Little Rock","LIU Brooklyn","Long Beach St.","Louisiana Lafayette","Louisiana Monroe","Louisiana Tech","Louisville","Loyola Chicago","Loyola Marymount","Loyola MD","LSU","Manhattan","Marist","Marquette","Marshall","Maryland","Massachusetts","McNeese St.","Memphis","Mercer","Miami FL","Miami OH","Michigan","Michigan St.","Middle Tennessee","Milwaukee","Minnesota","Mississippi","Mississippi St.","Mississippi Valley St.","Missouri","Missouri St.","Monmouth","Montana","Montana St.","Morehead St.","Morgan St.","Mount St. Mary's","Murray St.","Navy","Nebraska","Nevada","New Mexico","New Mexico St.","New Orleans","Niagara","Nicholls St.","Norfolk St.","North Carolina","North Carolina A&amp;T","North Carolina Central","North Carolina St.","North Dakota","North Dakota St.","North Texas","Northeastern","Northern Arizona","Northern Colorado","Northern Illinois","Northern Iowa","Northern Kentucky","Northwestern","Northwestern St.","Notre Dame","Oakland","Ohio","Ohio St.","Oklahoma","Oklahoma St.","Old Dominion","Oral Roberts","Oregon","Oregon St.","Pacific","Penn","Penn St.","Pepperdine","Pittsburgh","Portland","Portland St.","Prairie View A&amp;M","Princeton","Providence","Purdue","Radford","Rhode Island","Richmond","Rider","Robert Morris","Rutgers","Saint Joseph's","Saint Louis","Saint Mary's","Saint Peter's","Sam Houston St.","Samford","San Diego","San Diego St.","San Francisco","San Jose St.","Santa Clara","Seton Hall","Siena","SMU","South Alabama","South Carolina","South Carolina St.","South Dakota St.","South Florida","Southeast Missouri St.","Southeastern Louisiana","Southern","Southern Illinois","Southern Miss","Southern Utah","St. Bonaventure","St. Francis PA","St. John's","Stanford","Stephen F. Austin","Stony Brook","Syracuse","TCU","Temple","Tennessee","Tennessee St.","Texas","Texas A&amp;M","Texas A&amp;M Corpus Chris","Texas Southern","Texas St.","Texas Tech","Towson","Troy","Tulane","Tulsa","UAB","UC Davis","UC Irvine","UC Santa Barbara","UCF","UCLA","UMBC","UNC Asheville","UNC Greensboro","UNC Wilmington","UNLV","USC","UT Arlington","Utah","Utah St.","UTEP","UTSA","Valparaiso","Vanderbilt","VCU","Vermont","Villanova","Virginia","Virginia Tech","Wagner","Wake Forest","Washington","Washington St.","Weber St.","West Virginia","Western Carolina","Western Kentucky","Western Michigan","Wichita St.","Winthrop","Wisconsin","Wofford","Wright St.","Wyoming","Xavier","Yale"],[null,null,null,3,null,null,null,null,null,0,null,2,null,null,3,null,null,null,null,null,null,3,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,0,null,null,0,null,null,null,2,null,null,null,null,null,null,null,null,0,null,null,null,null,null,null,null,null,null,null,null,6,2,null,null,4,null,null,null,null,null,null,null,null,null,null,null,null,null,3,null,2,null,null,0,0,0,null,null,null,null,null,2,null,null,3,null,null,null,0,null,null,null,null,null,null,null,3,null,3,null,null,0,null,null,null,0,3,null,null,5,0,null,0,2,0,0,null,null,null,null,null,null,null,null,null,null,null,null,null,null,2,null,0,null,null,null,null,null,null,4,0,null,4,null,null,null,0,null,null,null,null,null,null,null,2,null,0,2,4,null,0,null,null,0,null,0,null,0,0,null,null,null,null,null,0,null,null,null,null,null,null,null,null,null,null,null,null,null,0,null,null,null,null,null,2,null,null,null,null,null,null,null,0,null,null,null,null,null,5,null,null,null,2,null,2,null,null,null,null,null,null,null,0,null,null,null,0,2,null,null,null,null,null,null,null,null,null,2,0,null,null,null,2,null,null,null,2,null,6,null,0,null,null,0,null,null,null,null,null,null,0,null,null,null,null,null,null,null],[null,null,0,3,null,null,null,null,null,0,null,null,null,null,4,null,0,null,null,null,null,null,null,2,0,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,3,null,null,null,null,null,null,null,null,0,null,null,null,3,null,null,0,6,null,null,null,null,null,null,null,0,null,null,null,null,null,null,null,null,null,null,null,null,2,null,null,null,3,null,null,null,null,null,null,null,null,null,null,null,null,null,2,null,null,0,null,null,0,3,null,null,0,null,null,5,null,null,4,null,null,null,null,null,null,2,null,null,null,0,null,6,null,null,null,5,null,0,null,null,2,null,null,2,null,null,0,2,3,null,null,null,null,null,0,0,null,null,null,0,null,null,null,null,4,0,null,null,null,null,null,null,null,3,0,null,4,null,null,null,0,null,null,null,null,null,null,null,0,null,null,null,2,null,2,null,null,null,null,null,null,0,null,null,null,null,null,null,0,null,null,0,null,null,null,2,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,2,null,null,null,2,null,2,null,null,null,null,null,null,null,0,null,null,null,0,2,null,null,null,null,null,null,null,null,null,3,null,null,0,null,0,null,null,null,null,null,2,0,0,null,null,0,null,null,0,null,2,null,null,null,null,null,null,null,0,null],[null,null,null,3,null,null,null,null,null,0,null,null,null,null,2,2,null,null,null,null,null,null,null,null,null,0,null,null,0,null,null,null,null,null,null,null,null,0,null,null,null,null,null,0,null,null,null,null,null,null,null,null,null,null,null,null,null,3,null,null,null,3,null,null,null,null,null,null,null,0,null,null,3,null,null,null,null,null,null,null,null,null,4,0,0,null,0,null,null,null,null,null,null,null,null,null,0,null,null,0,0,null,null,6,null,null,4,null,null,null,null,null,null,3,2,null,0,null,null,null,null,null,null,null,null,null,null,null,0,null,null,null,null,4,null,0,null,0,null,null,null,null,null,null,null,2,null,0,null,null,null,null,null,0,2,null,null,null,null,null,null,null,0,null,null,null,null,2,null,null,null,4,0,null,0,null,null,null,0,null,null,null,null,null,null,null,3,null,null,2,3,null,null,null,null,null,null,0,null,null,2,null,null,null,null,5,2,null,null,null,null,null,null,null,null,null,null,null,null,0,null,null,null,0,null,null,null,null,null,null,null,null,null,null,0,null,null,null,null,null,2,null,null,null,6,2,2,null,null,null,0,null,null,null,null,null,null,null,0,0,null,null,null,null,2,null,null,null,null,5,null,null,null,null,2,null,null,null,null,null,null,0,null,null,null,null,null,null,0,null,2,null,0,null,null,null,null,3,2,null],[null,null,null,null,null,null,null,null,null,5,null,0,null,null,2,null,null,0,null,null,0,null,0,0,null,null,null,null,2,null,null,null,null,null,null,null,null,null,null,null,0,0,null,null,null,null,null,null,null,null,null,0,null,null,null,null,null,2,null,null,null,5,null,null,null,null,0,null,null,null,0,null,2,null,null,null,0,null,null,null,null,null,2,null,null,null,2,null,null,null,null,null,null,null,null,null,null,null,null,null,2,null,null,0,null,null,3,0,null,null,null,null,null,6,4,null,3,0,null,null,0,null,null,null,null,null,null,null,null,3,null,2,null,0,null,null,null,null,2,null,null,2,null,null,null,3,null,null,null,null,null,null,null,0,0,null,null,null,null,null,null,2,null,null,null,null,null,null,null,null,null,4,0,null,0,null,null,0,null,null,null,null,null,null,null,null,0,null,null,null,6,null,null,null,null,0,null,null,null,null,2,null,null,null,null,null,3,null,3,3,null,null,null,null,null,null,null,null,null,null,null,null,null,null,2,null,2,null,null,null,null,null,null,null,0,null,null,null,null,null,0,null,null,null,2,null,4,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,0,null,null,null,null,null,null,2,null,null,null,0,0,0,null,3,null,null,4,null,null,null,null,null,null,null,null,null,null,null,0,null,null,null,null,0,0,null],[null,null,null,0,null,null,null,null,null,3,null,2,null,null,null,null,2,null,null,null,null,null,null,null,null,0,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,2,null,null,null,null,2,null,null,null,0,null,null,null,null,2,null,null,null,5,null,0,null,null,null,null,2,null,null,null,0,null,null,null,0,null,null,null,0,null,4,null,null,null,0,null,null,null,null,null,null,null,null,null,null,null,0,null,5,null,null,3,null,null,2,0,null,null,null,null,null,null,0,null,null,0,null,null,null,null,null,0,null,null,null,null,2,3,null,0,null,0,null,null,null,null,null,null,0,0,null,null,null,6,null,2,null,3,null,null,null,3,0,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,3,null,null,3,null,null,null,null,null,null,null,null,null,null,null,2,null,null,null,3,null,null,null,null,0,null,null,null,null,0,null,null,null,0,0,null,null,null,null,null,0,0,null,null,0,null,null,null,null,null,null,null,null,6,2,null,2,0,0,null,null,null,null,0,null,null,null,null,null,null,0,null,null,4,null,null,0,null,2,null,null,null,null,null,null,null,null,null,null,null,null,null,null,2,null,null,null,null,4,null,null,null,null,2,null,null,0,null,null,null,4,null,null,null,null,null,null,2,null,null,null,null,null,null,null,null,null,0,null],[null,null,null,3,null,null,null,null,null,2,null,5,null,null,null,null,3,null,null,null,null,null,0,null,null,null,null,null,0,null,null,null,null,2,null,null,null,null,null,null,null,null,null,3,null,null,null,null,0,4,0,null,null,null,2,null,null,null,null,null,null,6,null,0,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,2,0,null,null,5,null,null,null,null,null,null,null,null,null,0,null,0,null,0,null,0,0,null,null,null,null,null,null,null,null,null,2,0,null,null,2,null,null,null,null,null,0,null,null,null,0,null,2,null,4,null,2,null,null,null,null,null,null,null,null,null,null,null,2,3,null,null,4,null,null,null,0,0,null,null,null,null,null,null,0,null,null,null,null,0,null,null,null,null,3,null,null,null,null,null,null,null,null,null,null,2,null,null,null,0,null,null,2,2,null,null,null,null,0,null,null,null,null,null,null,null,null,0,0,2,null,null,0,null,0,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,0,null,null,null,null,0,null,null,null,2,null,null,null,3,null,0,null,null,4,null,null,0,null,null,0,null,null,null,0,null,null,2,null,3,null,null,null,null,6,null,null,null,null,0,null,null,null,null,null,0,2,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,3,null],[null,null,null,3,null,null,null,null,null,3,2,4,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,2,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,0,null,null,null,3,null,null,2,null,null,null,null,0,null,null,null,6,null,0,null,null,3,null,null,null,null,null,null,null,null,null,2,null,null,null,null,null,2,0,null,0,2,null,null,0,null,null,null,null,null,null,null,null,null,null,null,null,null,3,null,null,2,null,null,null,null,null,null,6,null,null,null,null,null,null,null,null,null,null,null,null,null,0,0,null,null,null,null,0,null,null,null,null,null,null,null,null,null,null,null,null,2,null,null,null,null,0,null,null,null,null,0,null,null,null,null,0,null,0,null,0,0,0,null,null,null,5,null,null,2,null,null,null,0,null,null,0,null,null,null,null,null,null,null,3,null,3,null,null,null,null,null,null,2,0,2,null,null,null,0,null,0,null,null,2,null,null,0,null,null,null,0,null,null,null,null,null,null,null,4,null,null,0,null,null,null,null,null,null,null,null,0,null,null,0,4,null,null,null,0,null,4,null,null,2,null,null,null,null,null,0,null,null,null,null,null,null,null,null,0,null,null,null,null,5,0,null,3,null,null,null,null,0,null,null,2,0,null,null,2,null,null,null,null,null,null,null,null,null,null,null,null,null,2,null],[null,null,null,2,null,null,null,null,null,0,null,2,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,0,null,null,null,null,null,0,null,null,null,null,null,0,null,5,null,null,null,null,null,null,2,null,null,null,null,null,0,null,0,null,null,null,6,null,2,0,null,null,null,0,null,null,null,null,null,null,null,3,0,null,null,null,null,2,null,0,null,3,null,null,null,null,null,null,null,null,null,0,0,null,null,null,null,null,5,null,null,2,2,null,null,null,null,null,2,null,null,4,0,0,null,null,null,null,null,null,null,1,0,null,2,null,null,null,2,null,null,null,null,null,3,null,4,null,null,0,6,2,null,null,null,null,null,0,2,0,null,0,null,null,null,null,0,null,0,null,null,3,null,null,null,null,3,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,4,0,3,0,null,null,null,null,null,null,0,null,null,null,null,0,null,null,null,null,null,null,0,null,null,null,null,null,null,null,null,null,null,null,null,3,null,null,null,null,null,null,0,null,null,null,null,null,null,null,null,0,0,null,null,2,null,0,null,null,0,null,null,null,null,null,null,null,2,null,null,null,null,null,null,4,null,null,null,null,null,2,null,null,null,3,null,null,null,null,null,null,null,null,null,0,null,null,null,0,null,null,null,null,null,null,null,null,null,null,null],[null,null,null,null,null,null,null,null,null,0,null,3,null,null,null,null,0,null,null,null,0,null,null,null,null,null,null,null,2,null,null,null,null,3,null,null,null,null,null,null,null,0,4,null,null,0,null,null,null,null,0,null,null,null,null,0,null,null,null,null,null,2,0,null,null,null,null,null,0,null,null,null,null,null,null,null,4,null,null,null,null,3,null,null,null,null,0,null,null,null,null,null,null,null,null,0,null,null,null,null,2,null,null,4,null,null,2,0,null,null,null,null,null,5,0,null,5,null,null,null,null,null,null,null,null,0,null,0,null,3,null,null,null,0,0,null,0,null,null,2,null,0,null,null,null,6,null,null,null,null,null,null,null,0,null,null,null,null,null,null,null,null,null,0,null,0,2,0,null,null,null,6,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,2,null,null,null,null,null,0,null,null,0,null,null,null,null,null,0,null,2,null,0,null,null,null,null,null,null,null,null,null,null,null,null,2,2,null,0,null,null,null,null,null,null,null,2,0,null,null,null,null,2,null,null,null,null,null,4,null,0,null,null,null,null,null,0,null,null,2,null,null,null,null,null,null,2,null,null,null,null,null,null,null,2,null,null,null,null,3,null,null,null,3,null,null,3,null,null,null,null,null,3,null,null,null,null,null,0,null,2,null],[null,null,null,2,null,null,null,null,null,5,null,6,null,null,null,null,null,null,null,null,0,4,null,null,null,null,null,null,null,null,null,null,null,0,null,null,null,null,null,0,null,0,0,null,null,null,null,null,null,3,null,null,null,null,null,null,null,null,null,null,0,6,null,null,null,null,null,null,null,null,null,null,5,null,null,null,null,null,null,null,null,2,2,null,null,null,null,null,null,2,null,null,null,0,null,null,null,null,null,null,0,null,null,3,null,null,null,null,null,null,null,null,0,3,null,null,2,null,0,null,null,0,null,null,null,null,null,null,null,3,null,null,0,null,null,null,3,null,3,2,null,null,null,null,null,4,2,null,null,2,null,null,null,4,null,null,null,null,null,null,null,null,0,0,null,0,0,null,null,null,null,2,0,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,0,null,null,2,null,null,null,null,null,2,null,0,null,null,null,null,null,0,4,null,null,null,0,null,null,null,0,null,null,null,null,null,null,null,null,null,0,null,null,null,null,null,null,null,null,null,null,0,null,null,null,null,null,null,null,null,3,null,2,null,0,2,null,null,0,0,null,null,null,null,3,0,null,null,null,0,0,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,2,null,null,2,null,0,null,null,null,0,null,null,null,2,null,null,null,null,null],[null,null,null,2,null,null,null,null,null,0,3,6,null,null,null,null,0,null,null,null,null,null,null,null,null,null,null,null,0,null,null,null,null,null,null,null,null,null,null,null,0,0,2,null,null,null,0,null,null,4,null,null,null,null,null,null,null,null,null,null,0,null,null,null,null,null,null,null,null,null,null,0,0,null,null,null,null,null,null,null,null,null,3,null,null,null,null,0,null,0,null,null,null,null,null,null,null,null,null,null,0,null,null,0,null,null,null,2,null,null,null,null,null,3,null,null,4,null,null,null,null,null,null,null,null,0,null,null,null,0,null,null,null,null,2,null,null,null,3,4,null,3,null,null,2,0,0,null,null,0,null,3,null,2,null,null,null,null,null,null,0,0,null,null,null,null,null,null,null,0,null,5,0,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,0,5,2,null,0,null,null,0,null,null,null,null,null,null,null,null,2,null,null,null,null,null,null,null,2,null,0,null,null,null,null,null,null,0,null,null,null,null,null,null,null,null,null,null,null,0,null,null,null,null,null,2,null,null,2,null,0,null,null,2,null,null,0,null,null,null,null,2,3,null,null,null,null,null,6,null,null,null,null,null,null,null,2,null,null,null,null,null,null,null,0,4,null,null,3,null,null,2,null,null,2,null,null,null,null,null,null,null,0,null],[null,null,null,null,null,null,null,null,null,3,null,3,null,null,null,0,null,null,null,null,null,2,null,0,null,null,null,null,null,null,null,null,null,0,null,0,null,null,null,null,null,null,4,0,null,null,0,null,null,3,null,null,null,null,null,null,null,null,null,null,2,0,null,null,null,null,2,null,null,null,null,null,null,null,null,null,null,null,null,null,null,0,4,3,null,null,3,null,null,0,null,null,null,null,null,null,null,null,null,null,null,null,null,0,null,null,2,2,null,null,null,null,null,4,0,null,6,null,null,null,null,null,null,null,null,null,null,0,null,3,null,null,null,null,null,null,2,null,0,5,null,0,null,null,null,0,null,null,null,null,null,5,0,null,null,0,null,0,null,null,null,null,null,null,null,2,null,0,null,null,null,2,null,null,null,null,null,null,null,null,null,0,null,null,null,null,null,null,null,null,0,null,null,null,null,null,null,null,0,null,null,0,null,null,2,null,2,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,0,2,null,null,null,null,null,0,null,null,null,null,null,null,null,null,null,null,null,2,null,null,6,null,2,null,null,2,null,null,null,null,3,null,null,null,0,null,null,null,null,0,0,null,null,0,null,null,null,null,3,null,null,null,0,null,0,null,2,null,2,null,4,null,null,null,null,0,null,null,null,null,null,null,null,null,null,null],[null,null,null,null,null,null,null,null,null,6,null,null,null,null,null,null,null,null,null,null,null,2,0,null,null,null,null,0,null,null,null,null,null,3,null,null,null,null,1,1,2,3,2,3,null,null,null,2,null,null,2,null,null,null,null,null,null,null,null,null,null,2,null,null,null,null,null,null,null,0,null,null,null,null,null,null,null,null,null,null,null,null,0,0,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,2,null,0,0,null,null,2,3,null,0,null,null,null,3,null,null,6,null,null,null,null,null,null,null,0,null,null,null,null,4,null,null,null,null,null,null,0,null,0,0,null,null,null,null,0,null,null,null,null,5,0,null,null,null,null,null,0,null,null,null,null,0,0,null,null,2,null,null,null,null,null,5,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,0,null,0,null,null,null,0,null,null,null,null,null,null,null,0,4,2,null,0,null,null,null,null,3,null,0,null,null,null,null,null,null,null,null,null,null,null,0,0,null,null,null,null,null,null,null,null,null,null,null,null,3,null,null,null,null,2,null,null,3,null,null,null,0,null,null,null,null,2,null,null,null,null,null,4,null,null,null,null,null,0,null,4,null,null,null,0,0,null,null,2,0,null,null,2,null,null,null,null,null,null,null,null,null,0,null,null,null,2,null],[null,null,null,null,null,null,null,null,null,4,null,2,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,0,null,null,null,null,null,null,null,null,null,null,null,0,2,null,2,0,null,null,null,null,null,4,null,null,null,0,null,0,null,null,2,null,null,4,null,null,null,null,0,null,null,null,0,null,null,null,null,null,2,null,null,null,null,0,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,2,0,2,2,null,0,null,null,null,null,null,null,null,2,null,null,6,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,3,0,null,null,null,0,null,2,3,null,null,null,0,null,null,null,null,null,null,null,null,null,null,0,0,0,null,2,null,null,null,0,null,5,null,null,null,null,null,null,null,0,null,null,null,null,null,null,null,null,null,null,0,2,null,null,null,null,null,null,null,null,null,null,null,0,2,null,3,0,4,2,null,null,null,null,2,null,null,null,null,null,null,0,null,null,null,null,null,0,0,0,null,null,null,null,null,null,null,null,null,null,0,5,null,null,3,0,0,0,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,3,null,null,null,null,0,null,null,6,0,null,null,3,null,null,null,null,null,null,null,null,3,null,null,3,null,null,2,null,null,null,null,null,null,0,null],[null,null,null,null,null,null,0,null,null,0,null,2,null,0,3,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,0,2,null,2,null,null,null,null,null,null,6,null,null,2,null,null,0,null,null,2,null,null,6,null,null,null,null,null,null,0,null,null,null,3,0,null,null,null,null,null,null,0,0,null,null,null,null,null,4,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,2,null,null,3,null,null,null,null,null,null,2,null,0,4,null,0,null,null,null,null,null,null,null,null,null,null,0,null,null,null,null,null,null,null,null,3,null,null,null,null,2,3,null,5,null,null,0,2,null,null,0,3,null,null,null,null,null,0,0,null,null,null,2,0,null,null,null,null,0,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,5,3,2,null,null,null,null,null,0,null,null,null,null,null,null,null,null,3,null,0,null,null,null,null,null,null,null,null,null,0,null,null,null,null,null,null,0,null,null,null,null,null,null,null,null,null,null,null,null,null,null,4,2,null,null,0,null,4,2,null,0,null,null,null,null,null,null,null,null,2,0,null,null,null,null,0,null,null,null,null,null,null,null,2,null,null,0,0,null,null,null,0,null,null,null,null,0,null,2,null,null,null,null,null,0,0,null,null,null,null,null],[null,null,null,null,null,null,null,null,0,2,null,0,null,null,2,null,0,null,null,null,null,null,null,null,null,null,null,0,null,null,null,null,null,null,null,null,0,null,null,null,null,null,2,null,null,null,null,null,null,2,null,null,0,null,0,null,null,0,null,null,null,3,null,null,null,null,null,null,null,null,null,null,6,null,null,null,null,null,0,null,null,null,null,null,null,null,null,3,null,null,null,null,null,null,0,null,null,null,null,null,2,null,null,0,0,0,null,4,null,0,null,null,null,2,null,null,2,null,0,0,null,null,null,null,null,null,0,null,null,0,null,null,null,3,null,null,null,null,2,null,null,null,null,3,null,null,6,null,null,null,null,null,null,0,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,5,null,null,null,null,null,null,null,0,null,null,null,null,null,null,null,null,null,2,2,4,null,null,0,null,null,0,null,2,null,null,null,null,null,null,4,null,null,null,null,null,null,null,0,null,null,null,0,null,null,null,null,null,3,null,null,null,null,0,null,null,0,null,null,null,null,null,0,null,2,2,null,null,3,null,2,3,null,2,null,null,null,null,null,null,null,null,4,null,null,null,null,null,3,null,null,null,0,0,null,null,2,0,null,null,0,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,0,5,null,null,null,null,null],[null,null,null,null,0,null,null,null,null,6,null,0,null,null,null,null,null,null,null,null,null,2,null,null,null,null,null,2,0,null,null,null,0,0,null,null,null,null,null,null,2,null,3,null,null,null,null,null,null,null,null,null,0,null,null,null,null,null,null,null,null,6,null,null,0,null,null,null,null,null,null,null,2,null,null,null,null,null,2,null,0,null,3,0,null,2,0,3,null,null,2,null,null,0,0,0,null,null,null,null,4,null,null,0,2,0,2,0,null,null,null,null,null,3,null,2,3,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,5,null,null,null,null,null,null,null,5,null,null,null,3,null,null,2,null,0,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,2,null,null,null,null,null,null,null,null,null,null,null,null,null,0,2,null,null,0,0,0,null,null,null,null,null,null,3,null,null,null,null,null,0,0,null,null,null,null,null,null,null,2,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,0,null,null,null,4,null,null,2,null,4,0,null,0,null,null,null,null,null,null,null,null,null,null,null,null,null,null,3,null,null,0,null,null,4,null,null,2,null,null,null,null,null,null,null,0,null,null,0,null,null,null,null,null,0,null,null,null,0,null,null,null,0,null],[null,null,null,2,null,null,null,null,null,3,null,null,null,null,null,null,null,null,null,null,null,0,0,null,null,null,null,null,null,null,null,null,null,2,null,null,0,null,null,null,0,null,2,null,null,null,null,null,null,4,null,null,2,0,null,null,null,null,null,null,null,3,null,null,null,null,null,null,null,null,null,null,0,null,0,null,null,null,null,null,null,null,null,2,null,null,null,0,null,null,0,null,null,0,null,0,null,null,null,null,3,0,null,6,null,null,null,null,null,null,null,null,null,5,null,4,3,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,0,null,6,null,0,null,null,0,null,null,0,null,null,null,0,2,null,4,null,null,0,null,null,null,null,0,null,null,null,null,null,null,null,null,null,null,null,null,2,null,null,null,null,null,null,null,null,null,null,null,2,null,null,2,5,0,null,null,4,null,null,0,null,0,3,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,0,null,null,null,null,0,null,null,null,null,null,null,null,null,null,3,null,null,null,null,0,2,null,null,null,null,null,null,null,3,null,null,null,null,0,null,null,null,2,null,null,null,0,null,3,null,null,null,2,null,0,null,0,null,null,null,0,null,null,null,null,null,null,null,2,null,null,null,null,null,0,null,null,0,2,null,null,2,2,null],[null,null,null,0,null,null,null,null,null,4,2,null,null,null,3,0,null,null,null,null,null,null,null,null,null,null,null,3,0,null,null,null,null,2,null,null,null,2,null,null,null,null,0,null,null,null,null,0,0,3,null,null,0,null,0,null,null,null,null,null,null,3,null,0,null,null,null,null,null,null,null,null,2,null,null,null,null,null,null,null,null,null,null,null,null,null,null,2,null,null,null,null,null,null,null,0,null,null,null,null,2,null,null,2,null,null,null,null,0,null,null,null,null,6,null,null,4,null,null,null,null,null,null,null,null,null,null,null,null,2,null,null,null,0,0,null,5,null,3,null,null,0,null,null,null,null,4,null,0,null,null,0,null,2,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,0,null,null,null,null,null,null,null,null,null,null,null,3,null,null,null,4,2,null,null,0,null,null,0,null,null,3,null,null,null,null,null,2,null,null,null,null,null,null,0,null,null,null,0,null,0,null,null,null,null,null,null,null,null,null,0,null,null,null,null,null,0,null,null,null,null,null,2,null,null,6,null,null,null,null,5,null,null,null,null,null,null,0,null,2,null,null,null,null,null,null,null,0,null,0,null,null,null,2,0,null,null,null,null,null,0,null,null,null,0,2,null,null,0,null,null,0,null,null,null,3,null,null,null,2,null],[null,0,null,4,0,null,null,null,null,0,null,null,null,null,null,null,null,null,null,null,null,2,null,null,null,null,null,null,0,null,null,null,null,null,null,null,null,null,null,null,0,null,2,null,null,null,null,null,null,6,null,null,null,null,0,null,null,2,null,null,null,5,null,0,null,null,null,0,null,null,null,null,0,0,null,null,null,null,null,null,null,null,null,null,null,null,6,2,null,null,null,null,null,null,null,null,null,null,null,null,3,0,null,null,null,null,null,null,null,null,null,null,null,4,null,null,2,null,0,null,null,0,null,null,null,null,null,null,null,0,null,null,null,null,2,null,null,null,2,null,null,2,null,null,null,null,0,null,null,null,null,2,null,null,null,0,null,null,null,null,null,0,null,null,3,null,null,null,null,null,null,2,null,null,2,null,null,null,null,null,null,null,0,null,null,null,null,null,null,null,null,5,null,null,null,null,2,null,null,null,3,null,null,null,0,0,null,null,null,0,null,null,null,4,null,null,null,null,null,null,null,null,null,null,2,null,null,null,0,null,null,null,null,null,null,0,null,null,null,null,null,2,null,null,3,null,null,null,null,3,null,null,null,null,2,null,null,null,null,3,null,null,null,0,null,null,null,null,null,null,null,null,0,null,0,0,0,3,0,0,null,null,null,null,3,0,null,null,null,null,null,0,null,null,2,null,null,null,4,null],[null,null,null,0,null,null,null,null,null,4,null,null,null,null,null,null,null,null,null,null,null,2,null,null,null,2,null,null,null,null,null,null,null,null,null,null,null,null,null,null,0,0,2,null,null,null,null,null,null,2,null,null,0,null,null,null,0,null,null,null,null,3,null,null,null,0,null,null,null,null,0,null,2,null,null,null,null,null,null,null,null,0,null,null,null,null,2,2,null,null,null,null,null,null,null,null,null,null,null,null,6,null,null,null,null,null,0,2,null,null,null,null,null,0,null,null,4,null,0,null,null,null,null,null,null,null,null,null,null,5,null,null,null,0,null,null,null,null,null,null,null,null,null,null,null,null,5,null,3,0,null,2,null,null,null,null,0,null,null,null,null,null,null,null,2,0,null,null,0,null,null,6,null,null,3,null,null,null,null,null,null,null,0,null,null,null,null,0,0,null,2,3,0,null,null,null,2,0,null,null,0,null,null,null,null,null,null,null,null,null,null,null,null,null,null,0,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,0,null,2,null,null,null,null,null,0,null,null,0,null,null,null,null,0,null,null,null,null,3,null,null,null,null,2,null,null,null,0,0,null,null,null,null,null,null,null,3,0,0,null,null,null,null,2,3,null,null,null,2,3,null,null,4,null,null,null,null,0,4,null,null,null,null,null],[null,0,null,2,null,0,null,null,null,2,null,0,null,null,null,null,null,null,0,null,null,3,null,3,null,2,null,null,null,null,null,null,null,0,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,4,null,null,null,0,null,null,null,null,null,null,null,3,null,null,null,null,null,null,null,null,null,null,6,null,null,null,null,null,null,null,5,2,3,null,null,null,null,3,null,null,null,null,null,null,null,null,null,null,null,null,2,null,null,2,null,0,0,null,null,null,null,null,null,0,null,0,2,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,5,null,null,0,null,null,null,null,4,null,null,null,null,0,null,2,null,null,null,null,null,null,0,2,null,null,null,null,0,null,null,0,null,null,null,null,null,null,2,null,null,2,null,null,null,null,null,null,null,0,null,null,2,null,null,null,2,0,null,null,0,null,null,0,0,null,null,2,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,0,null,null,null,0,null,null,0,null,null,null,null,null,null,0,0,null,null,null,null,null,null,null,null,0,null,null,2,null,4,2,null,null,null,null,null,null,null,null,0,null,null,null,null,6,null,null,null,0,null,null,null,null,0,null,null,null,null,null,null,4,null,null,null,null,3,null,null,3,null,null,null,3,0,0,null,null,null,0,null],[null,null,null,null,null,0,null,null,null,0,null,0,null,null,null,null,null,null,0,null,null,2,null,null,null,null,null,3,0,null,null,null,null,null,null,null,0,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,0,0,null,null,null,null,null,null,null,0,null,null,null,0,null,null,null,null,null,null,6,null,null,null,null,null,null,null,null,0,5,null,null,null,0,0,null,null,null,null,null,null,null,0,null,null,null,null,0,null,null,2,null,null,null,null,null,0,null,null,null,4,null,null,2,null,null,null,null,null,null,null,null,0,null,null,null,2,null,null,null,null,null,null,0,null,2,null,null,4,null,null,0,null,2,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,2,null,0,null,0,null,null,4,null,null,null,null,null,0,null,null,null,null,null,null,null,null,0,null,null,6,null,null,0,0,4,null,null,0,null,null,3,null,null,null,null,null,2,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,3,null,null,null,null,null,0,null,null,null,null,null,3,null,2,3,0,null,null,0,null,null,null,null,null,null,null,null,null,5,null,null,null,null,3,3,null,null,null,null,null,null,3,2,null,0,2,2,null,null,null,2,0,null,null,null,null,null,2,2,null,0,null,2,null],[null,null,null,null,null,null,null,0,null,0,null,2,null,null,null,0,null,0,0,null,0,null,null,null,null,null,null,2,0,null,null,0,null,null,null,null,null,null,null,null,null,null,null,0,null,null,null,null,null,0,null,0,null,4,null,null,null,null,null,0,null,2,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,0,null,2,0,null,null,null,0,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,0,null,null,null,null,null,null,null,null,null,6,2,0,0,null,null,null,null,null,null,null,null,null,null,null,null,4,null,null,null,null,null,null,2,null,null,null,null,6,null,2,null,null,3,null,null,null,null,2,0,null,null,null,null,null,null,null,0,null,null,null,null,null,null,null,null,null,null,5,null,null,null,null,null,null,null,null,null,null,null,null,null,null,2,null,null,null,2,null,null,0,0,null,null,null,null,null,2,null,0,null,null,null,2,null,null,null,null,null,null,0,null,0,null,null,null,2,null,null,null,null,null,2,null,0,null,null,null,null,null,null,null,null,null,null,null,null,null,3,null,null,null,null,0,3,null,4,2,null,null,null,null,null,null,null,null,null,null,null,null,null,5,0,null,null,null,2,0,0,null,null,null,null,null,0,null,null,3,null,null,null,null,null,3,null,3,null,3,null,null,0,3,null,null,null,4,null],[null,null,0,null,null,null,null,0,null,3,2,null,null,null,null,null,null,null,null,0,null,0,null,null,null,null,null,0,0,null,null,null,0,0,null,null,null,null,null,null,null,0,null,0,2,null,null,null,null,5,null,0,null,null,2,null,null,null,null,null,null,3,null,0,null,null,null,null,null,null,null,null,null,null,null,null,0,null,null,null,null,null,null,null,null,null,null,3,null,null,null,null,null,null,null,null,null,null,null,null,0,null,null,null,null,null,null,null,null,null,null,null,null,3,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,4,null,null,null,2,null,null,2,null,2,null,null,3,null,null,null,2,6,null,null,0,null,0,null,4,null,null,null,null,0,0,null,null,null,null,null,null,null,null,null,null,null,6,null,null,null,null,0,null,null,null,null,null,0,null,null,null,null,null,null,0,4,2,null,null,null,null,null,null,null,null,4,null,0,null,null,null,3,0,null,null,null,0,null,null,null,null,null,null,null,null,null,null,null,null,null,2,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,0,null,3,null,0,0,null,2,2,null,null,null,null,null,null,null,null,null,null,null,null,null,2,null,null,null,null,null,2,null,0,0,null,null,null,null,0,null,5,null,null,null,0,2,null,null,0,null,2,null,null,null,2,null,null,null,3,null],[null,null,null,null,null,null,null,null,null,null,null,null,0,null,null,null,null,4,null,null,null,null,null,null,null,null,null,6,2,null,null,null,null,2,null,null,null,null,null,null,null,null,null,0,null,null,null,null,null,null,null,3,null,null,null,null,null,null,null,null,null,6,null,0,null,null,null,null,null,null,null,null,0,null,null,null,0,null,null,null,null,null,0,null,null,null,2,2,null,null,null,null,null,null,null,null,0,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,2,4,null,4,null,null,null,0,null,null,null,null,null,null,null,null,0,null,null,null,null,null,null,0,null,2,null,null,null,null,null,null,null,5,null,null,0,null,null,null,2,null,null,0,null,null,0,null,2,null,null,null,2,0,null,null,null,null,null,null,null,null,null,null,0,null,null,null,null,3,null,null,null,0,0,2,3,null,0,2,null,null,null,null,null,null,null,2,null,null,null,null,null,3,null,null,0,null,0,null,null,null,3,null,0,null,null,0,null,null,null,null,0,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,3,null,0,4,null,0,2,null,null,null,null,null,null,null,null,null,null,null,0,null,null,null,null,null,null,0,null,null,null,0,0,null,null,0,null,0,2,null,null,null,2,3,null,null,5,null,null,null,null,null,2,0,null,null,3,null],[null,null,0,null,null,null,null,null,null,4,null,null,null,null,null,null,null,null,0,null,null,null,0,null,null,0,null,6,3,null,null,null,null,null,null,null,null,null,null,null,null,null,2,0,null,null,null,null,null,6,null,null,null,null,null,null,null,null,null,null,null,3,null,null,null,null,null,null,null,null,null,null,4,null,null,null,3,null,null,null,2,null,0,0,null,null,null,2,null,null,0,null,null,null,null,null,null,null,null,null,2,null,null,null,0,null,null,null,null,null,null,null,null,4,2,null,5,null,null,null,null,null,null,null,0,null,null,null,null,0,null,null,null,null,null,null,3,null,null,null,null,0,null,null,null,2,0,null,null,null,null,null,null,0,null,null,null,null,2,null,null,null,null,null,null,null,null,null,null,null,null,4,null,null,null,null,null,null,null,null,0,null,null,null,null,null,2,0,null,3,null,null,0,null,null,null,null,null,0,null,2,null,null,null,0,null,2,null,null,3,null,null,null,null,null,null,0,null,null,null,3,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,0,null,null,null,2,null,2,0,null,2,0,null,null,null,null,null,null,null,null,null,null,null,0,null,2,null,0,null,null,0,null,null,null,0,null,0,null,0,5,null,0,null,null,null,null,2,null,null,2,null,null,null,null,null,3,0,null,null,0,null],[null,null,null,0,null,null,null,null,null,null,null,null,null,null,null,null,null,4,0,null,null,null,null,null,null,null,null,null,0,null,null,null,null,null,null,null,null,null,null,null,null,null,3,null,null,null,null,2,0,0,null,null,2,0,null,null,null,null,0,null,null,0,null,null,null,null,null,null,null,null,null,null,4,null,null,null,2,null,null,null,null,null,2,null,null,null,null,2,null,null,null,null,0,null,null,null,null,null,null,null,null,null,null,3,null,null,null,2,null,null,null,null,null,6,2,null,6,null,null,null,2,null,null,null,0,0,null,null,null,5,null,null,0,null,null,null,3,null,null,null,null,0,null,null,null,0,3,null,null,null,null,null,null,0,null,null,0,null,null,null,null,2,null,null,null,2,0,null,null,null,2,4,null,null,3,null,null,null,null,null,null,null,null,null,null,null,0,null,3,5,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,2,null,null,null,null,null,null,null,2,0,null,null,null,null,0,null,null,null,null,null,null,null,null,null,0,2,null,null,null,null,0,null,0,null,null,null,null,null,4,null,0,null,null,0,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,0,null,null,0,null,null,null,null,null,null,null,2,2,0,null,0,null,null,null,null,null,null,0,null,0,null,0,null,3,null,null,null,3,null],[null,null,0,null,null,0,null,null,null,3,null,null,null,null,null,null,null,null,0,null,null,null,null,null,null,0,null,2,null,null,null,null,null,2,null,null,null,null,null,null,null,null,0,null,null,null,null,0,2,null,null,null,2,0,null,null,null,null,null,null,null,4,null,null,null,null,null,null,null,null,null,null,4,null,null,3,null,null,null,null,null,null,0,null,null,null,null,2,null,null,null,null,2,null,null,null,null,null,null,null,2,null,null,3,null,0,null,2,null,null,null,null,0,3,0,null,null,3,null,null,null,null,null,null,null,null,null,null,null,6,null,null,null,null,null,null,4,null,null,null,null,2,null,3,null,6,3,null,null,2,2,null,null,0,null,null,0,null,null,null,null,null,null,null,null,0,0,null,null,null,null,2,0,null,0,null,null,null,null,null,null,null,null,null,null,0,0,null,null,4,0,0,null,null,3,null,0,null,null,null,0,null,null,null,null,null,null,null,null,null,null,null,null,null,2,0,null,null,null,null,2,null,null,null,null,null,null,null,null,null,0,null,null,null,0,null,null,null,null,null,null,null,null,null,5,null,2,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,0,null,null,null,null,0,null,null,null,null,null,null,0,null,2,null,0,null,null,null,null,null,null,null,null,null,0,null,5,null,0,null,null,null,null,null],[null,null,null,null,null,0,null,0,null,4,0,null,null,null,null,null,null,3,null,null,null,null,null,null,null,null,null,null,0,0,null,null,null,null,null,null,null,null,null,null,null,null,0,null,null,0,null,0,null,6,null,null,2,null,4,0,null,null,null,null,null,0,null,null,null,0,null,null,null,null,null,null,5,null,null,null,null,null,null,null,null,0,null,null,null,null,null,2,null,null,null,null,2,null,null,null,null,null,null,null,null,null,null,null,null,null,null,3,null,null,null,null,null,2,0,null,6,null,0,null,null,null,null,null,null,null,null,null,null,3,null,null,null,null,0,null,null,null,null,0,null,2,2,null,null,4,4,null,0,null,null,null,null,null,null,null,null,null,null,null,null,null,null,0,null,0,0,null,null,null,null,2,null,0,0,null,2,null,null,null,null,null,null,null,null,null,null,null,null,0,0,0,null,null,2,null,null,null,null,null,2,null,null,null,null,0,null,null,null,null,null,null,null,0,2,null,null,null,null,null,3,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,3,2,null,2,null,null,3,null,2,null,null,null,null,null,null,null,null,0,null,null,null,null,null,3,null,null,null,null,null,null,null,null,null,null,null,null,null,0,null,2,3,null,null,null,null,null,0,null,null,null,0,2,null,5,0,null,null,null,null],[null,null,null,null,null,0,null,null,null,4,null,2,null,null,null,null,null,0,0,null,null,null,null,null,null,null,0,2,null,null,null,null,null,null,null,null,null,null,null,null,null,null,2,null,null,0,null,null,null,null,null,null,null,0,2,null,null,null,null,null,null,6,null,null,null,null,null,0,null,null,null,null,null,null,null,null,null,null,null,null,null,null,2,0,null,2,null,4,null,null,0,null,0,null,null,null,null,null,null,null,null,null,null,0,null,null,2,0,null,null,null,null,null,2,null,null,5,null,0,null,null,null,null,null,null,null,null,null,null,4,null,null,null,0,null,null,null,null,2,null,null,null,null,null,null,null,5,null,null,null,0,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,0,null,null,null,null,3,null,null,3,null,0,null,0,null,null,null,2,null,null,null,4,null,null,2,3,0,null,null,2,null,null,null,null,null,null,null,null,null,null,0,0,null,null,null,null,0,null,null,null,null,null,null,null,null,2,null,null,null,null,null,0,null,null,null,null,null,null,null,null,null,null,null,null,null,0,null,0,null,null,null,null,null,null,0,null,null,0,null,null,null,null,null,null,2,null,0,null,null,3,null,null,null,null,null,null,null,3,null,null,null,0,null,0,null,2,2,null,null,null,null,null,null,3,null,null,null,3,null,6,0,null,0,3,null],[null,null,null,null,null,null,null,null,null,0,null,null,null,null,null,0,null,0,null,null,null,null,null,null,null,null,0,2,null,null,0,null,null,0,null,null,null,null,null,null,null,0,0,null,null,null,null,0,null,2,null,null,null,null,0,null,null,null,null,null,null,3,null,null,null,null,null,null,null,null,null,null,null,null,null,0,null,null,0,null,null,null,null,null,null,null,null,3,null,0,0,null,null,2,null,0,null,null,null,null,null,null,null,3,null,0,2,3,null,null,null,null,null,4,null,null,2,null,null,null,null,null,null,2,null,null,null,null,null,null,null,null,null,null,null,null,null,null,3,null,null,null,null,3,null,0,0,2,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,6,null,null,null,null,null,null,null,null,null,null,2,null,null,null,4,null,null,null,5,null,null,null,4,0,null,null,null,null,0,null,null,null,null,2,0,null,null,null,null,null,null,2,null,null,null,null,null,null,null,null,null,null,0,null,null,null,null,null,0,null,null,null,null,null,null,null,null,null,null,null,2,0,5,null,0,null,null,0,3,null,null,null,0,null,null,null,null,null,null,null,null,null,null,null,0,null,0,null,0,null,2,null,null,null,null,null,2,null,6,4,null,null,null,null,null,0,0,null,null,null,2,null,3,null,null,null,2,2],[null,null,null,null,null,null,null,null,null,3,null,2,null,null,null,null,null,3,null,null,null,null,null,null,null,0,null,3,null,null,null,null,null,null,null,null,null,null,null,null,null,null,2,null,null,null,null,null,null,null,null,null,0,null,0,null,null,null,null,null,null,2,null,0,null,null,null,null,null,null,null,null,4,null,null,0,2,null,null,null,null,null,null,null,null,null,null,6,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,0,null,2,null,null,null,0,null,4,0,0,4,null,null,null,null,null,null,null,null,null,null,null,null,2,null,null,null,null,null,null,0,null,0,null,null,null,null,0,null,3,2,2,null,0,null,null,null,null,null,null,null,null,null,null,0,null,null,null,0,null,0,null,null,null,null,6,null,null,null,0,null,null,null,null,null,null,null,0,2,null,2,null,null,null,null,0,null,null,5,null,null,null,null,null,null,null,null,null,0,null,3,null,2,null,null,null,null,null,null,2,null,null,null,null,0,null,null,null,0,null,0,null,5,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,0,null,null,null,0,null,null,null,0,null,null,null,3,null,null,null,0,null,2,null,null,null,null,null,null,0,0,0,2,2,0,null,null,null,null,null,3,null,null,null,2,0,3,null,null,null,4,null],[null,null,null,2,null,null,null,null,null,0,null,0,null,null,2,null,null,null,null,null,null,null,null,null,null,0,3,2,null,null,null,0,null,null,null,null,null,null,0,null,null,null,2,3,null,null,null,null,null,null,null,null,0,0,null,null,null,null,null,null,null,4,null,null,null,null,null,null,null,null,null,null,2,null,null,null,4,null,null,null,null,null,null,null,null,0,null,3,null,null,null,null,null,null,null,null,2,null,null,null,null,null,null,null,null,0,null,null,null,null,null,null,null,6,3,null,2,null,null,null,null,null,0,null,null,null,null,null,null,null,5,null,null,null,null,null,null,2,null,null,null,null,null,0,null,6,2,null,null,null,null,null,null,0,null,null,0,null,null,null,null,0,null,null,3,null,0,null,null,null,null,2,null,null,0,null,null,null,null,null,null,null,null,null,null,null,null,null,null,2,0,null,null,null,null,null,null,0,null,null,null,null,null,null,null,0,3,0,2,null,null,null,null,null,null,null,null,null,null,null,0,null,null,null,2,null,null,null,null,null,0,null,null,null,null,null,null,null,0,null,null,null,0,null,3,0,null,2,null,0,3,null,0,null,4,null,null,null,null,null,null,null,null,null,null,2,null,0,null,null,null,null,null,null,null,null,null,null,null,null,6,0,0,null,null,null,null,null,3,null,null,null,0,null,null,null,0,null,2,null],[0,null,null,null,null,null,null,null,null,null,0,null,null,null,5,null,null,2,0,null,null,null,null,0,null,null,2,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,0,null,null,null,0,null,null,null,null,null,null,null,null,null,null,null,null,null,null,4,null,null,null,null,null,null,null,null,0,null,2,null,null,null,3,null,null,0,null,null,null,null,null,0,null,4,null,null,null,null,null,null,null,null,3,null,null,null,null,null,null,null,null,0,2,0,null,null,null,null,null,2,0,null,4,null,null,null,null,2,null,null,null,null,null,null,null,0,null,null,null,3,null,null,0,null,2,null,null,null,null,null,null,3,5,null,null,2,0,0,null,null,null,null,0,null,null,null,null,2,null,null,0,null,0,null,null,null,null,3,null,null,null,null,0,null,0,null,null,null,null,0,null,null,null,null,null,2,2,null,0,null,3,null,null,null,null,null,null,null,null,null,null,null,4,null,null,null,null,null,null,null,0,0,null,null,null,null,null,null,null,null,0,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,0,null,null,3,null,null,null,null,null,null,6,null,null,null,null,null,null,2,null,2,null,null,null,null,null,null,null,null,null,0,null,null,null,null,0,0,2,6,3,null,null,2,null,null,null,null,null,null,null,null,0,2,null,null,null,0],[2,null,null,2,null,null,null,null,null,null,null,2,null,null,null,null,null,2,null,null,null,null,null,null,null,null,null,null,0,null,null,null,null,null,null,null,null,null,null,null,null,null,null,0,0,null,0,2,null,0,null,null,2,null,null,null,null,null,null,0,0,null,null,null,null,null,null,0,null,null,null,null,2,null,null,null,2,null,null,null,null,null,0,null,null,null,0,2,0,null,null,0,null,null,null,null,2,null,null,null,2,null,null,null,null,0,2,null,null,null,null,null,null,2,null,null,null,null,null,null,null,0,null,null,null,null,null,null,null,null,2,null,null,2,null,null,null,null,2,null,null,null,null,null,null,2,null,null,null,null,null,null,null,0,null,null,null,null,0,null,null,null,null,null,null,null,null,null,null,null,0,0,null,null,null,null,null,2,null,null,null,null,null,null,null,null,null,null,2,0,2,2,null,2,2,2,null,null,null,null,null,null,null,null,null,null,0,null,null,null,null,null,2,null,null,null,null,null,null,null,0,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,0,null,null,null,null,null,2,null,null,0,null,0,null,null,0,null,2,null,null,null,null,null,null,null,0,null,2,null,null,0,null,null,2,null,null,0,null,null,null,null,0,null,2,0,0,null,null,null,null,null,2,null,null,null,null,0,2,null,null,null,null,null],[3.98571428571429,1.57142857142857,3.12857142857143,33.8,1.5,4.35714285714286,0.7,2.58571428571429,0.714285714285714,77.2,11.0285714285714,45.1857142857143,0.857142857142857,0.7,24.5857142857143,4.77142857142857,6.44285714285714,25.4,7.01428571428571,0.842857142857143,2.61428571428571,23.2142857142857,3.4,6.92857142857143,0.514285714285714,9.54285714285714,8.71428571428572,41.0285714285714,21.1714285714286,0.914285714285714,0.942857142857143,1.8,1.57142857142857,21.2285714285714,0.6,0.657142857142857,2.27142857142857,2.8,2.31428571428571,3.35714285714286,12.4428571428571,7.68571428571429,49.8714285714286,16.8,5.58571428571429,3.04285714285714,3.28571428571429,11.1857142857143,6.58571428571429,67.4285714285714,3.2,5.64285714285714,23.4714285714286,11.3857142857143,16.2428571428571,3.51428571428571,0.785714285714286,12.1857142857143,5.04285714285714,1.82857142857143,4.75714285714286,113.671428571429,0.614285714285714,7.7,1.32857142857143,2.51428571428571,5.54285714285714,2.7,3.58571428571429,1.71428571428571,3.5,0.642857142857143,69.4285714285714,1.47142857142857,0.742857142857143,5.5,32.9,0.6,3.84285714285714,0.985714285714286,10.2285714285714,11.3,48.3428571428571,12.0714285714286,1.12857142857143,7.51428571428571,30.9714285714286,66.0285714285714,1,4.71428571428571,5.67142857142857,1,7.25714285714286,4.92857142857143,1.44285714285714,4.6,12.4142857142857,0.6,1.12857142857143,0.528571428571429,48.7857142857143,2.2,4.8,51.6428571428571,3.77142857142857,9.18571428571429,35.0285714285714,36.8857142857143,0.757142857142857,2.2,0.514285714285714,0.957142857142857,1.52857142857143,115.028571428571,26.3857142857143,9.18571428571429,103.528571428571,7.01428571428571,6.04285714285714,0.714285714285714,4.55714285714286,5.35714285714286,0.971428571428571,5.5,2.42857142857143,2.95714285714286,1.91428571428571,3.54285714285714,4.78571428571429,68.5857142857143,10.8285714285714,5.04285714285714,1.51428571428571,31.6428571428571,6.52857142857143,1.04285714285714,32.0142857142857,3.94285714285714,55.2,15.5571428571429,1.3,41.0857142857143,3.24285714285714,18.1714285714286,7.82857142857143,62.8285714285714,85.6571428571429,8.4,7.21428571428571,21.4428571428571,11.7285714285714,19.0714285714286,2.6,32.8714285714286,6.65714285714286,2.95714285714286,10.3857142857143,1.17142857142857,4.45714285714286,1.7,3.12857142857143,17.5571428571429,6.58571428571429,4.54285714285714,15.0142857142857,17.8,14.9428571428571,3.44285714285714,1.6,1.32857142857143,3.65714285714286,111.928571428571,4.25714285714286,0.914285714285714,31.0142857142857,0.957142857142857,5.5,5.21428571428571,4.04285714285714,1.4,0.871428571428571,1.24285714285714,13.9571428571429,1.94285714285714,2.87142857142857,4.02857142857143,35.1428571428571,2.51428571428571,11.0285714285714,55.6857142857143,59.0428571428571,41.6571428571429,11.2714285714286,5.44285714285714,37.2571428571429,6.11428571428571,7.04285714285714,10.4571428571429,6.2,5.71428571428571,38.6428571428571,0.657142857142857,1.67142857142857,0.685714285714286,10.3428571428571,15.4285714285714,60.6428571428571,2.5,14.6,12.1857142857143,1.24285714285714,4.35714285714286,4.14285714285714,15.6,14.4142857142857,11.9142857142857,2.1,1.61428571428571,1.41428571428571,3.77142857142857,19.3428571428571,0.685714285714286,0.657142857142857,4.98571428571429,25.1,8.98571428571429,5.62857142857143,5.24285714285714,8.42857142857143,3.37142857142857,3.7,3.82857142857143,0.714285714285714,0.785714285714286,5.67142857142857,12.8,2.04285714285714,0.728571428571429,3.57142857142857,0.585714285714286,22.6285714285714,35.1285714285714,8.31428571428571,0.942857142857143,78.8428571428571,3.24285714285714,40.6714285714286,33.4142857142857,1.24285714285714,54.3714285714286,22.3,0.814285714285714,5.7,1.3,26.9714285714286,1.15714285714286,1.71428571428571,5.57142857142857,20.3857142857143,14.5,0.957142857142857,3.88571428571429,5.72857142857143,5.8,68.7857142857143,3.74285714285714,3.45714285714286,3.35714285714286,6.4,29.3142857142857,21.3714285714286,0.828571428571429,35.9714285714286,11.0285714285714,11.2,2.88571428571429,8.82857142857143,18.9571428571429,24.4142857142857,7.57142857142857,63.8428571428571,41.3142857142857,12.3,0.757142857142857,28.6571428571429,23.1,6.38571428571429,7.45714285714286,41.3571428571429,0.657142857142857,18,3.74285714285714,24.1857142857143,8.97142857142857,62.9285714285714,6.52857142857143,2.4,5.81428571428571,54.3571428571429,3.81428571428571]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>1985<\/th>\n      <th>1986<\/th>\n      <th>1987<\/th>\n      <th>1988<\/th>\n      <th>1989<\/th>\n      <th>1990<\/th>\n      <th>1991<\/th>\n      <th>1992<\/th>\n      <th>1993<\/th>\n      <th>1994<\/th>\n      <th>1995<\/th>\n      <th>1996<\/th>\n      <th>1997<\/th>\n      <th>1998<\/th>\n      <th>1999<\/th>\n      <th>2000<\/th>\n      <th>2001<\/th>\n      <th>2002<\/th>\n      <th>2003<\/th>\n      <th>2004<\/th>\n      <th>2005<\/th>\n      <th>2006<\/th>\n      <th>2007<\/th>\n      <th>2008<\/th>\n      <th>2009<\/th>\n      <th>2010<\/th>\n      <th>2011<\/th>\n      <th>2012<\/th>\n      <th>2013<\/th>\n      <th>2014<\/th>\n      <th>2015<\/th>\n      <th>2016<\/th>\n      <th>2017<\/th>\n      <th>2018<\/th>\n      <th>2019<\/th>\n      <th>2021<\/th>\n      <th>Exp<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"className":"dt-right","targets":[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

## PASE


```r
team.seed.w <- AllGames %>% filter(Round == 1) %>% select(c(2,5,6))
colnames(team.seed.w) <- c("Year","Seed","Team")
team.seed.l <- AllGames %>% filter(Round == 1) %>% select(c(2,9,10))
colnames(team.seed.l) <- c("Year","Seed","Team")
team.seed.long <- rbind(team.seed.w,team.seed.l)
team.seed.wide <- spread(team.seed.long, Year, Seed)
x <- team.seed.wide
old <- 1:16
new <- seed.history$exp
for (i in old) {
x[x == i] <- new[i]
}

#setdiff(x$Team,rownames(team.history))

team.exp.wide <- x 
team.pase.wide <- cbind(team.history[,1:36]-x[,2:37])
team.pase <- data.frame(cbind(rowMeans(team.pase.wide, na.rm=TRUE)))
colnames(team.pase) <- "Pase"
datatable(team.pase)
```

<!--html_preserve--><div id="htmlwidget-13ce877adc1846aa0eb3" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-13ce877adc1846aa0eb3">{"x":{"filter":"none","data":[["Abilene Christian","Air Force","Akron","Alabama","Alabama St.","Albany","Alcorn St.","American","Appalachian St.","Arizona","Arizona St.","Arkansas","Arkansas Pine Bluff","Arkansas St.","Auburn","Austin Peay","Ball St.","Baylor","Belmont","Binghamton","Boise St.","Boston College","Boston University","Bradley","Brown","Bucknell","Buffalo","Butler","BYU","Cal Poly","Cal St. Bakersfield","Cal St. Fullerton","Cal St. Northridge","California","Campbell","Canisius","Central Connecticut","Central Michigan","Charleston","Charleston Southern","Charlotte","Chattanooga","Cincinnati","Clemson","Cleveland St.","Coastal Carolina","Colgate","Colorado","Colorado St.","Connecticut","Coppin St.","Cornell","Creighton","Davidson","Dayton","Delaware","Delaware St.","DePaul","Detroit","Drake","Drexel","Duke","East Carolina","East Tennessee St.","Eastern Illinois","Eastern Kentucky","Eastern Michigan","Eastern Washington","Evansville","Fairfield","Fairleigh Dickinson","FIU","Florida","Florida A&amp;M","Florida Atlantic","Florida Gulf Coast","Florida St.","Fordham","Fresno St.","Gardner Webb","George Mason","George Washington","Georgetown","Georgia","Georgia Southern","Georgia St.","Georgia Tech","Gonzaga","Grand Canyon","Green Bay","Hampton","Hartford","Harvard","Hawaii","Hofstra","Holy Cross","Houston","Howard","Idaho","Idaho St.","Illinois","Illinois Chicago","Illinois St.","Indiana","Indiana St.","Iona","Iowa","Iowa St.","IUPUI","Jackson St.","Jacksonville","Jacksonville St.","James Madison","Kansas","Kansas St.","Kent St.","Kentucky","La Salle","Lafayette","Lamar","Lehigh","Liberty","Lipscomb","Little Rock","LIU Brooklyn","Long Beach St.","Louisiana Lafayette","Louisiana Monroe","Louisiana Tech","Louisville","Loyola Chicago","Loyola Marymount","Loyola MD","LSU","Manhattan","Marist","Marquette","Marshall","Maryland","Massachusetts","McNeese St.","Memphis","Mercer","Miami FL","Miami OH","Michigan","Michigan St.","Middle Tennessee","Milwaukee","Minnesota","Mississippi","Mississippi St.","Mississippi Valley St.","Missouri","Missouri St.","Monmouth","Montana","Montana St.","Morehead St.","Morgan St.","Mount St. Mary's","Murray St.","Navy","Nebraska","Nevada","New Mexico","New Mexico St.","New Orleans","Niagara","Nicholls St.","Norfolk St.","North Carolina","North Carolina A&amp;T","North Carolina Central","North Carolina St.","North Dakota","North Dakota St.","North Texas","Northeastern","Northern Arizona","Northern Colorado","Northern Illinois","Northern Iowa","Northern Kentucky","Northwestern","Northwestern St.","Notre Dame","Oakland","Ohio","Ohio St.","Oklahoma","Oklahoma St.","Old Dominion","Oral Roberts","Oregon","Oregon St.","Pacific","Penn","Penn St.","Pepperdine","Pittsburgh","Portland","Portland St.","Prairie View A&amp;M","Princeton","Providence","Purdue","Radford","Rhode Island","Richmond","Rider","Robert Morris","Rutgers","Saint Joseph's","Saint Louis","Saint Mary's","Saint Peter's","Sam Houston St.","Samford","San Diego","San Diego St.","San Francisco","San Jose St.","Santa Clara","Seton Hall","Siena","SMU","South Alabama","South Carolina","South Carolina St.","South Dakota St.","South Florida","Southeast Missouri St.","Southeastern Louisiana","Southern","Southern Illinois","Southern Miss","Southern Utah","St. Bonaventure","St. Francis PA","St. John's","Stanford","Stephen F. Austin","Stony Brook","Syracuse","TCU","Temple","Tennessee","Tennessee St.","Texas","Texas A&amp;M","Texas A&amp;M Corpus Chris","Texas Southern","Texas St.","Texas Tech","Towson","Troy","Tulane","Tulsa","UAB","UC Davis","UC Irvine","UC Santa Barbara","UCF","UCLA","UMBC","UNC Asheville","UNC Greensboro","UNC Wilmington","UNLV","USC","UT Arlington","Utah","Utah St.","UTEP","UTSA","Valparaiso","Vanderbilt","VCU","Vermont","Villanova","Virginia","Virginia Tech","Wagner","Wake Forest","Washington","Washington St.","Weber St.","West Virginia","Western Carolina","Western Kentucky","Western Michigan","Wichita St.","Winthrop","Wisconsin","Wofford","Wright St.","Wyoming","Xavier","Yale"],[0.878472222222222,-0.440972222222222,-0.230902777777778,0.788807189542484,-0.00694444444444444,-0.102777777777778,-0.0763888888888889,-0.106481481481481,-0.166666666666667,0.485894097222222,0.709490740740741,0.849206349206349,-0.00694444444444444,-0.0763888888888889,1.75385802469136,0.265277777777778,0.418981481481482,0.323611111111111,-0.247395833333333,-0.0763888888888889,-0.166666666666667,0.970328282828283,-0.122222222222222,0.493055555555556,-0.0763888888888889,0.243055555555556,0.751736111111111,1.44351851851852,-0.152777777777778,-0.00694444444444444,-0.0763888888888889,-0.121527777777778,-0.166666666666667,0.352564102564103,-0.00694444444444444,-0.256944444444444,-0.0833333333333333,0.559027777777778,0.114583333333333,-0.116319444444444,0.0534722222222222,0.172743055555556,0.427234299516908,-0.0711805555555555,1.5,-0.0243055555555556,-0.0642361111111111,0.14484126984127,0.252777777777778,1.36342592592593,0.590277777777778,0.536458333333333,0.147685185185185,0.10625,0.245138888888889,-0.220833333333333,-0.00694444444444444,0.466049382716049,0.930555555555556,-0.871527777777778,0.177777777777778,1.06985294117647,-0.00694444444444444,0.0385802469135802,-0.0763888888888889,-0.0532407407407407,0.890625,-0.166666666666667,-0.03125,-0.0902777777777778,-0.0708333333333333,-0.00694444444444444,1.31159420289855,-0.00694444444444444,-0.0763888888888889,0.916666666666667,0.730654761904762,-0.166666666666667,0.217592592592593,-0.00694444444444444,0.790509259259259,0.176697530864198,0.651515151515152,-0.253787878787879,-0.0763888888888889,0.591666666666667,0.766059027777778,0.900966183574879,-0.0763888888888889,-0.0138888888888889,0.365277777777778,-0.00694444444444444,0.637152777777778,0.0885416666666667,-0.211805555555556,-0.128472222222222,-0.0476190476190476,-0.00694444444444444,-0.256944444444444,-0.00694444444444444,0.423007246376812,-0.30787037037037,0.506944444444445,0.481666666666667,0.354166666666667,-0.183712121212121,0.648148148148148,0.491228070175439,-0.00694444444444444,-0.00694444444444444,-0.708333333333333,-0.0763888888888889,-0.0868055555555556,0.988492063492064,0.259722222222222,0.538194444444445,1.43703703703704,0.4,-0.196428571428571,-0.00694444444444444,0.475694444444444,0.303819444444444,-0.0763888888888889,0.763888888888889,-0.113425925925926,-0.477430555555556,0.243055555555556,-0.136574074074074,0.652777777777778,1.06944444444444,2.38194444444444,1.41435185185185,-0.0763888888888889,0.686683006535948,0.436111111111111,-0.121527777777778,0.389322916666667,0.469907407407407,0.996843434343434,0.21875,-0.0868055555555556,0.732253086419753,0.878472222222222,0.112654320987654,0.293650793650794,1.18118686868687,1.15445402298851,0.9125,0.817708333333333,0.502893518518518,-0.0121527777777777,0.467361111111111,-0.0243055555555556,0.0760582010582011,0.450231481481482,-0.0868055555555556,0.0271464646464647,-0.131944444444444,0.523148148148148,-0.0763888888888889,-0.00694444444444444,0.214351851851852,0.673611111111111,-0.866071428571429,0.535590277777778,-0.0810185185185185,-0.117592592592593,-0.100694444444444,-0.0868055555555556,-0.131944444444444,0.958333333333333,1.23569023569024,-0.0396825396825397,-0.166666666666667,0.796875,-0.0763888888888889,0.309027777777778,0.378472222222222,-0.170138888888889,-0.0763888888888889,-0.0763888888888889,-0.211805555555556,0.572048611111111,-0.121527777777778,1.29166666666667,0.553240740740741,0.446759259259259,-0.143518518518519,0.896990740740741,0.625330687830688,0.367026748971193,0.610416666666667,0.195833333333333,0.373263888888889,0.990384615384615,-0.453703703703704,0.4375,-0.155982905982906,0.526041666666667,-0.125992063492063,0.183845029239766,-0.166666666666667,-0.131944444444444,-0.00694444444444444,-0.0454545454545454,0.262626262626263,0.461934156378601,-0.00694444444444444,1.18948412698413,0.833333333333333,-0.0416666666666667,-0.0416666666666667,0.178240740740741,0.459201388888889,0.4765625,-0.0871913580246913,-0.252314814814815,-0.121527777777778,-0.211805555555556,0.298611111111111,0.00173611111111112,-0.166666666666667,-0.00694444444444444,0.678819444444444,0.785790598290598,0.744212962962963,-0.15,-0.0729166666666667,-0.243055555555555,-0.0486111111111111,-0.362847222222222,0.261574074074074,-0.256944444444444,-0.0763888888888889,0.213293650793651,0.189043209876543,-0.490740740740741,-0.166666666666667,-0.473958333333333,-0.0763888888888889,0.199404761904762,0.453993055555556,0.494444444444444,-0.256944444444444,1.11254789272031,-0.564814814814815,0.619263285024155,0.397685185185185,-0.121527777777778,0.556584362139918,0.909722222222222,-0.0763888888888889,-0.0595238095238095,-0.0416666666666667,0.582175925925926,-0.00694444444444444,-0.121527777777778,1.38888888888889,0.822337962962963,0.440972222222222,-0.00694444444444444,0.743055555555556,-0.00694444444444442,0.230555555555556,1.0491452991453,0.958333333333333,-0.0243055555555556,-0.149305555555556,-0.0532407407407407,0.644444444444444,0.245949074074074,-0.00694444444444444,0.904513888888889,-0.303819444444444,0.275,-0.046875,0.135802469135802,0.113425925925926,0.271329365079365,0.161706349206349,0.831018518518519,0.174342105263158,0.0451388888888889,-0.0763888888888889,0.389384920634921,0.551767676767677,0.310185185185185,0.498842592592593,0.825980392156863,-0.00694444444444444,0.69849537037037,0.194444444444444,0.513888888888889,-0.00848765432098764,1.03291062801932,0.0166666666666667,-0.113425925925926,0.611111111111111,0.823888888888889,0.659722222222222]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>Pase<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"className":"dt-right","targets":1},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

