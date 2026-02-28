library(plotly)

source('functions.R')

set.seed(2)

# Simulation ------------------------------------------------

Sigma = matrix(c(
    2.0,1.4,0.0,
    1.4,2.0,1.4,
    0.0,1.4,2.0
), nrow=3, byrow = TRUE)

threed = MASS::mvrnorm(n=100, mu = c(0,0,0), Sigma = Sigma) |> as.data.frame()

colnames(threed) = c("x", "y", "z")

range = c(-5, 5)

plotly::plot_ly(data = threed) |>
    add_markers(x=threed$x, y = threed$y, z = threed$z) |>
    layout(
        scene = list(
            xaxis = list(range = range),
            yaxis = list(range = range),
            zaxis = list(range = range)
        )
    )

true_rotate_param = c('x' = runif(1,-10,10), 'y' = runif(1,-10,10), 'z' = runif(1,-10,10))
true_rotate_angle = plogis(true_rotate_param) * 2 * pi

print(true_rotate_angle)

threed2 = rotate3(threed, true_rotate_angle['x'], true_rotate_angle['y'], true_rotate_angle['z'])

# Estimation -------------------------------------------------

plotly::plot_ly(data = threed) |>
    layout(
        scene = list(
            xaxis = list(range = range),
            yaxis = list(range = range),
            zaxis = list(range = range)
        )
    ) |>
    add_markers(x=threed$x, y = threed$y, z = threed$z) %>%
    add_markers(x=threed2$x, y = threed2$y, z = threed2$z) %>%
    {}

res = optim(
    par = c(x = 0, y = 0, z = 0),
    fn = \(par) rotate_and_metric(threed, threed2, par['x'],par['y'], par['z']),
    method = 'CG'
    # control = list(
    #     trace = 20,
    #     maxit = 20000,
    #     temp = 500,
    #     tmax = 10
    # )
)


threed_rotated = rotate_3_param(threed, res$par['x'], res$par['y'], res$par['z'])

range = c(-5, 5)

plotly::plot_ly(data = threed) |>
    layout(
        scene = list(
            xaxis = list(range = range),
            yaxis = list(range = range),
            zaxis = list(range = range)
        )
    ) |>
    add_markers(x=threed2$x, y = threed2$y, z = threed2$z) %>%
    add_markers(x=threed_rotated$x, y = threed_rotated$y, z = threed_rotated$z) %>%
    {}

res$par
sin(plogis(res$par) * 2 * pi)
true_rotate_param
sin(true_rotate_angle)

res = optimx::optimr(
    par = c(x = 0, y = 0, z = 0),
    fn = \(par) rotate_and_metric(threed, threed2, par['x'],par['y'], par['z']),
    method = c("spg")
)

threed_rotated = rotate_3_param(threed, res$par['x'], res$par['y'], res$par['z'])

range = c(-5, 5)

plotly::plot_ly(data = threed) |>
    layout(
        scene = list(
            xaxis = list(range = range),
            yaxis = list(range = range),
            zaxis = list(range = range)
        )
    ) |>
    add_markers(x=threed2$x, y = threed2$y, z = threed2$z) %>%
    add_markers(x=threed_rotated$x, y = threed_rotated$y, z = threed_rotated$z) %>%
    {}

res$par
sin(plogis(res$par) * 2 * pi)
true_rotate_param
sin(true_rotate_angle)

res = optimx::opm(
    par = c(x = 0, y = 0, z = 0),
    fn = \(par) rotate_and_metric(threed, threed2, par['x'],par['y'], par['z']),
    # method = c("spg", "anms", "bobyqa", "BFGS", 'hjn', 'nlm', 'nlnm', 'nvm')
    method = 'ALL'
)

res
{
    print(true_rotate_param['x'])
    print((plogis(res$x) * 2 * pi) |> sin())
    print(sin(true_rotate_angle['x']))
}
