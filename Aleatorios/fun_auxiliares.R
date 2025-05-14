lim_inf <- 0
lim_sup <- 1
funcion <- "sqrt(1-x^2)"

x_vals <- seq(lim_inf, lim_sup, length.out = 100)
y_vals <- sapply(x_vals, function(x){eval(parse(text = funcion))})
coords <- data.frame(x = x_vals, y = y_vals)
# Calculo del área bajo la curva usando una aproximación numérica (regla del trapecio)
f <- function(x) eval(parse(text = funcion))
delta_x <- (lim_sup - lim_inf) / (length(x_vals) - 1)
area <- (delta_x / 2) * (f(lim_inf) + 2 * sum(f(x_vals[2:(length(x_vals) - 1)])) + f(lim_sup))

coords %>% ggplot(aes(x = x, y = y)) + geom_line(color = "blue", linewidth = 1) +
  geom_area(mapping = aes(x = x, y = y), fill = "lightblue", alpha = 0.5) +
  geom_vline(xintercept = lim_inf, linetype = "dashed", color = "red") +
  geom_vline(xintercept = lim_sup, linetype = "dashed", color = "red") +
  labs(title = paste("Área bajo la curva de f(x) =", funcion),
       subtitle = paste("Intervalo:", lim_inf, "a", lim_sup),
       x = "x",
       y = "f(x)",
       caption = paste("Área aproximada:", round(area, 4))
  ) + theme_minimal()