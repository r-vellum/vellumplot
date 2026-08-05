# scatter with a discrete colour legend and a line overlay

    Code
      scene_digest(p)
    Output
      $n_elements
      [1] 63
      
      $marks
      $marks$rect
      [1] 2
      
      $marks$segment
      [1] 26
      
      $marks$point
      [1] 35
      
      
      $panels
      [1] "legend"          "panel-1-1"       "plot-background"
      
      $scales
      $scales[[1]]
      $scales[[1]]$panel
      [1] "panel-1-1"
      
      $scales[[1]]$x
      [1] 1.317 5.620
      
      $scales[[1]]$y
      [1]  9.225 35.075
      
      
      

# bar chart (count stat)

    Code
      scene_digest(p)
    Output
      $n_elements
      [1] 21
      
      $marks
      $marks$rect
      [1] 5
      
      $marks$segment
      [1] 16
      
      
      $panels
      [1] "panel-1-1"       "plot-background"
      
      $scales
      $scales[[1]]
      $scales[[1]]$panel
      [1] "panel-1-1"
      
      $scales[[1]]$x
      [1] 0.5 3.5
      
      $scales[[1]]$y
      [1] -0.7 14.7
      
      
      

# histogram

    Code
      scene_digest(p)
    Output
      $n_elements
      [1] 61
      
      $marks
      $marks$segment
      [1] 29
      
      $marks$rect
      [1] 32
      
      
      $panels
      [1] "panel-1-1"       "plot-background"
      
      $scales
      $scales[[1]]
      $scales[[1]]$panel
      [1] "panel-1-1"
      
      $scales[[1]]$x
      [1]  9.656 34.644
      
      $scales[[1]]$y
      [1] -0.2  4.2
      
      
      

# boxplot grouped by a discrete x

    Code
      scene_digest(p)
    Output
      $n_elements
      [1] 36
      
      $marks
      $marks$point
      [1] 3
      
      $marks$rect
      [1] 5
      
      $marks$segment
      [1] 28
      
      
      $panels
      [1] "panel-1-1"       "plot-background"
      
      $scales
      $scales[[1]]
      $scales[[1]]$panel
      [1] "panel-1-1"
      
      $scales[[1]]$x
      [1] 0.5 3.5
      
      $scales[[1]]$y
      [1]  9.225 35.075
      
      
      

# loess smooth over a scatter

    Code
      scene_digest(p)
    Output
      $n_elements
      [1] 66
      
      $marks
      $marks$rect
      [1] 2
      
      $marks$point
      [1] 32
      
      $marks$segment
      [1] 32
      
      
      $panels
      [1] "panel-1-1"       "plot-background"
      
      $scales
      $scales[[1]]
      $scales[[1]]$panel
      [1] "panel-1-1"
      
      $scales[[1]]$x
      [1] 1.317 5.620
      
      $scales[[1]]$y
      [1]  6.857 37.419
      
      
      

# facet_wrap over a discrete variable

    Code
      scene_digest(p)
    Output
      $n_elements
      [1] 117
      
      $marks
      $marks$rect
      [1] 7
      
      $marks$point
      [1] 32
      
      $marks$segment
      [1] 78
      
      
      $panels
      [1] "panel-1-1"       "panel-1-2"       "panel-2-1"       "plot-background"
      [5] "strip-1-1"       "strip-1-2"       "strip-2-1"      
      
      $scales
      $scales[[1]]
      $scales[[1]]$panel
      [1] "panel-1-1"
      
      $scales[[1]]$x
      [1] 1.317 5.620
      
      $scales[[1]]$y
      [1]  9.225 35.075
      
      
      $scales[[2]]
      $scales[[2]]$panel
      [1] "panel-1-2"
      
      $scales[[2]]$x
      [1] 1.317 5.620
      
      $scales[[2]]$y
      [1]  9.225 35.075
      
      
      $scales[[3]]
      $scales[[3]]$panel
      [1] "panel-2-1"
      
      $scales[[3]]$x
      [1] 1.317 5.620
      
      $scales[[3]]$y
      [1]  9.225 35.075
      
      
      

