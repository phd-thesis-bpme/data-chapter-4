
ratio_indices <- (route_cont$index / point_cont$index) /50
plot(x = point_cont$year, y = ratio_indices, ylim = c(0,2),
     main = "Route Index / Point Index")
abline(h = 1)

ratio_indices <- (route_cont$index / detect_cont$index) / 50
plot(x = point_cont$year, y = ratio_indices, ylim = c(0,2),
     main = "Route Index / Detectability Index")
abline(h = 1)

ratio_indices <- (point_cont$index / detect_cont$index) 
plot(x = point_cont$year, y = ratio_indices, ylim = c(0,2),
     main = "Point Index / Detectability Index")
abline(h = 1)