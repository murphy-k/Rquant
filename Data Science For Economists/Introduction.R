# EC607 - Data Science for Economists

library(ggplot2)
library(gapminder)

# A simple ggplot
ggplot(data = gapminder, mapping = aes(x = gdpPercap, y = lifeExp)) +
  geom_point() + geom_smooth(method = 'loess')

# A more involved ggplot2
ggplot(data = gapminder,
       aes(
         x = log(gdpPercap),
         y = lifeExp,
         size = pop,
         col = continent
       )) +
  geom_point(alpha = 0.3) +
  ggtitle("Life Expectancy Vs. Log(GDP/Capita)") +
  xlab('Log GDP/Capita') +
  ylab('Life Expectancy')

# Density Geom
ggplot(data = gapminder) + ## i.e. No "global" aesthetic mappings"
  geom_density(aes(x = gdpPercap, fill = continent), alpha = 0.3)

p2 =
  p +
  geom_point(aes(size = pop, col = continent), alpha = 0.3) +
  scale_color_brewer(name = "Continent", palette = "Set1") + ## Different colour scale
  scale_size(name = "Population", labels = scales::comma) + ## Different point (i.e. legend) scale
  scale_x_log10(labels = scales::dollar) + ## Switch to logarithmic scale on x-axis. Use dollar units.
  labs(x = "Log (GDP per capita)", y = "Life Expectancy") + ## Better axis titles
  theme_minimal() ## Try a minimal (b&w) plot theme
p2
