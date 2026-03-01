library(plotly)
library(mirai)
library(dplyr)
library(rgl2gltf)
library(rgl)


source('functions.R')

set.seed(2)

# Extract candy cane model =====================================================

candy_cane = rgl2gltf::readGLB("assets/Glb_ChristmasCandyCane.glb")
meshes = rgl::as.mesh3d(candy_cane)
# plot3d(candy_cane)

cc_orig = extract_mesh_points(meshes)
point_lengths = lapply(cc_orig, nrow)
cc_orig = do.call(rbind, cc_orig)
colors = c(
    rep('white', point_lengths[[1]]),
    rep('red', point_lengths[[2]])
)

# Create a second candy cane and rotate it =====================================

# Three options:
# 1. Use the full object and just copy it twice
# 2. Use one random subset  of the full object and copy it
# 3. Use two random subsets of the full object and copy it.

create_two_candy_canes = function(cc_orig, colors, option = c(1,2,3)) {
    switch(option,
           "1" = {
               # No sampling -- copy the original
               rand1 = rand2 = 1:nrow(cc_orig)
               },
           "2" = {
               # Sample once and copy the subsampled candy cane
               rand1 = rand2 = sample(1:nrow(cc_orig), size = 500)
               },
           "3" = {
               # Sample the original two different times
               rand1 = sample(1:nrow(cc_orig), size = 500)
               rand2 = sample(1:nrow(cc_orig), size = 500)
               }
           )
    cc  = cc_orig[rand1, ]
    cc_colors = colors[rand1]
    cc2 = cc_orig[rand1, ]
    cc2_colors = colors[rand2]
    cc2_colors = replace(cc_colors, cc_colors == "red", "green")
    return(list(
        cc1 = cc,
        cc2 = cc2,
        cc1_colors = cc_colors,
        cc2_colors = cc2_colors
    ))
}

cc_data = create_two_candy_canes(cc_orig, colors, option = '3')

# Unpack the two candy canes and their colors
cc = cc_data$cc1
cc2 = cc_data$cc2
cc_colors = cc_data$cc1_colors
cc2_colors = cc_data$cc2_colors

# So far the candy cane and its copy are oriented the same way
# Here we rotate the second one randomly
true_rotate_param = c('x' = runif(1,-10,10), 'y' = runif(1,-10,10), 'z' = runif(1,-10,10))
true_rotate_angle = plogis(true_rotate_param) * 2 * pi
cc2 = rotate3(cc2, true_rotate_angle['x'], true_rotate_angle['y'], true_rotate_angle['z'])

# # Save this rotated mesh to a glb file so we can look at it in blender
# new_meshes = reestablish_mesh_points(list(
#     cc2[1:point_lengths[[1]], ],
#     cc2[(point_lengths[[1]]+1) : (point_lengths[[1]] + point_lengths[[2]]), ]
# ), meshes)
# f = file("assets/candy_cane_rotated.glb", open = "wb")
# rgl2gltf::writeGLB(rgl2gltf::as.gltf(new_meshes), f)
# close(f)

plotly::plot_ly() %>%
    add_markers(
        x=cc[,1], y=cc[,2], z=cc[,3], color = I(cc_colors)
    ) %>%
    add_markers(
        x=cc2[,1], y=cc2[,2], z=cc2[,3], color = I(cc2_colors)
    ) %>%
    layout(
        scene = list(
            xaxis = list(range = c(-0.20, 0.20)),
            yaxis = list(range = c(-0.20, 0.20)),
            zaxis = list(range = c(-0.20, 0.20))
        )
    )

# Optimization ============================================

# Produces 8 different starting values for possible rotation angles.
starts = function() {
    frac_2pi = c(0, 0.5)
    frac_2pi[1] = frac_2pi[1] + .Machine$double.eps
    theta_par = qlogis(frac_2pi)
    pars = expand.grid(
        x = theta_par,
        y = theta_par,
        z = theta_par
    )
    pars
}

starting_par = starts()

# This is the function we'll use to optimize and retrieve the estimates of the
# best rotation parameters

#' @param cc The first candy cane
#' @param cc The second candy cane, presumably out of alignment with the first
#' @param start Starting values for the rotation angles.
#' @param method Optimization method -- defaults to "conjugate gradients"
fit = function(cc, cc2, start, method='CG') {
    source('functions.R')
    optim(
        par = start,
        fn = \(par) rotate_and_metric(cc,cc2,par['x'],par['y'],par['z']),
        method = method
    )
}

# For each possible set of starting parameters, pass it to mirai for execution
# This first loop just cycles through starting parameters and *launches* execution.
# We capture "futures" (general term) that are sort of like placeholders for
# possibly unfinished work.

res = futures = list()
mirai::daemons(8)
for (i in 1:nrow(starting_par)) {
    futures[[i]] = mirai(fit(cc, cc2, starting_par[i,]), environment())
}
# Next, we cycle through the "futures" and use call_mirai to actually get the
# results out. `call_mirai` will wait until the worker is finished before
# returning a value.
for (f in seq_along(futures)) {
    res[[f]] = call_mirai(futures[[f]])$data
}

objective_values = sapply(res, \(x) x$value)
objective_values

best = which.min(objective_values)
best_res = res[[best]]

# Alignment ====================================================================

# Given the best result for rotation angles, now we apply that result to the
# original candy cane to line it up with the second one we made.
cc3 = rotate_3_param(cc, best_res$par[1], best_res$par[2], best_res$par[3])
cc3_colors = cc2_colors
cc3_colors[cc3_colors == "green"] = "blue" # make this one blue

# Want to line up the BLUE with the GREEN
# Hoping to see "shimmering", that the points exactly line up.
plotly::plot_ly() %>%
    add_markers(
        x=cc[,1], y=cc[,2], z=cc[,3], color = I(cc_colors)
    ) %>%
    add_markers(
        x=cc2[,1], y=cc2[,2], z=cc2[,3], color = I(cc2_colors)
    ) %>%
    add_markers(
        x=cc3[,1], y=cc3[,2], z=cc3[,3], color = I(cc3_colors)
    ) %>%
    layout(
        scene = list(
            xaxis = list(range = c(-0.20, 0.20)),
            yaxis = list(range = c(-0.20, 0.20)),
            zaxis = list(range = c(-0.20, 0.20))
        )
    )

print(rbind(
    plogis(best_res$par) * 2 * pi,
    true_rotate_angle
))

