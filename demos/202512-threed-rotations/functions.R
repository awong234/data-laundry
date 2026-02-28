# Functions --------------------------------------

xr = function(t) {
    rotate = matrix(c(
        1,      0,       0,
        0, cos(t), -sin(t),
        0, sin(t), cos(t)
    ), nrow=3, ncol=3)
    rotate
}

yr = function(t) {
    rotate = matrix(c(
         cos(t), 0, sin(t),
              0, 1,      0,
        -sin(t), 0, cos(t)
    ), nrow=3, ncol=3)
    rotate
}

zr = function(t) {
    rotate = matrix(c(
        cos(t),-sin(t), 0,
        sin(t), cos(t), 0,
             0,      0, 1
    ), nrow=3, ncol=3)
    rotate
}

rotate = function(mat, theta, axis = c('x', 'y', 'z')) {
    axis = match.arg(axis)
    rotation = switch(axis,
                      'x' = xr,
                      'y' = yr,
                      'z' = zr)
    r = rotation(theta)
    out = mat %*% r
    out
}

measure_closest = function(m1, m2) {
    dist = fields::rdist(m1, m2)
    # # Slow
    # closest = apply(dist, MARGIN=1, \(x) min(x)) |> sum()
    # # A little faster
    # val = vector(mode='numeric', length=ncol(dist))
    # for (i in 1:ncol(dist)) {
    #     val[i] = min(dist[,i])
    # }
    # closest = sum(val)'
    # # Basically the same
    # closest = sapply(1:ncol(dist), \(i) min(dist[,i])) |> sum()
    # # Fastest by far
    closest = matrixStats::colMins(dist) |> sum()
    closest
}

rotate3 = function(m, angle_x, angle_y, angle_z) {
    rotated = rotate(m, angle_x, 'x')
    rotated = rotate(rotated, angle_y, 'y')
    rotated = rotate(rotated, angle_z, 'z')
    rotated
}

rotate_3_param = function(m, x,y,z) {
    # angle_x = plogis(x) * 2 * pi
    # angle_y = plogis(y) * 2 * pi
    # angle_z = plogis(z) * 2 * pi
    angle_x = (atan(x) + pi/2) * 2
    angle_y = (atan(y) + pi/2) * 2
    angle_z = (atan(z) + pi/2) * 2
    rotated = rotate3(m, angle_x, angle_y, angle_z)
    rotated
}

rotate_and_metric = function(m1, m2, x,y,z) {
    rotated = rotate_3_param(m1, x,y,z)
    measure_closest(m2, rotated)
}


# Mesh functions ----------------------

extract_mesh_points = function(shapelist3d) {
    points = lapply(shapelist3d, \(x) {
        points = x$vb[1:3,] |> t()
        })
    return(points)
}

reestablish_mesh_points = function(points_list, shapes) {
    stopifnot(length(shapes) == length(points_list))

    for (i in 1:length(points_list)) {
        transformed_points = cbind(points_list[[i]], 1) |> t()
        shapes[[i]]$vb = transformed_points
    }
    return(shapes)
}
