#include <Rcpp.h>
#include <cmath>
#include <unordered_map>
#include <vector>
#include <string>
#include <algorithm>
#include <random>

// Helper function to calculate the haversine distance between two points
double haversineDistance(double lon1, double lat1, double lon2, double lat2, double R) {
  double dLat = (lat2 - lat1) * M_PI / 180.0;
  double dLon = (lon2 - lon1) * M_PI / 180.0;

  double a = sin(dLat / 2) * sin(dLat / 2) +
    cos(lat1 * M_PI / 180.0) * cos(lat2 * M_PI / 180.0) *
    sin(dLon / 2) * sin(dLon / 2);
  double c = 2 * atan2(sqrt(a), sqrt(1 - a));

  return R * c; // Distance in kilometers
}

// Helper function to calculate the Euclidean distance between two points
double euclideanDistance(double lon1, double lat1, double lon2, double lat2) {
  return sqrt(pow(lon2 - lon1, 2) + pow(lat2 - lat1, 2)); // Returns distance in degrees
}

extern "C" SEXP rounding_hashing_thinning(SEXP coordinatesSEXP, SEXP thin_distSEXP, SEXP trialsSEXP, SEXP all_trialsSEXP, SEXP distance_metricSEXP, SEXP RSEXP, SEXP seedSEXP) {
  Rcpp::NumericMatrix coordinates(coordinatesSEXP);
  double thin_dist = Rcpp::as<double>(thin_distSEXP);
  int trials = Rcpp::as<int>(trialsSEXP);
  bool all_trials = Rcpp::as<bool>(all_trialsSEXP);
  std::string distance_metric = Rcpp::as<std::string>(distance_metricSEXP);
  double R = Rcpp::as<double>(RSEXP);
  int seed = Rcpp::as<int>(seedSEXP);  // Get the seed from R

  int n = coordinates.nrow();
  double precision = thin_dist / 111.32; // km to degrees approximation

  // Hash map to store grid cells
  std::unordered_map<std::string, std::vector<int>> grid;

  // Populate the grid
  for (int i = 0; i < n; ++i) {
    int grid_lon = static_cast<int>(std::round(coordinates(i, 0) / precision));
    int grid_lat = static_cast<int>(std::round(coordinates(i, 1) / precision));
    std::string grid_cell = std::to_string(grid_lon) + "_" + std::to_string(grid_lat);
    grid[grid_cell].push_back(i);
  }

  Rcpp::LogicalVector keep_points(n, true); // Logical vector to track kept points
  int best_size = 0; // To keep track of the best size
  Rcpp::List all_keep_points; // List to hold all trials if needed

  // Random number generator setup with the provided seed
  std::mt19937 g(seed);

  // Loop for trials
  for (int t = 0; t < trials; ++t) {
    Rcpp::LogicalVector trial_keep_points = keep_points; // Create a copy for this trial

    // Randomly sample grid cells for thinning
    std::vector<std::string> grid_cells;
    for (const auto& cell : grid) {
      grid_cells.push_back(cell.first);
    }
    std::shuffle(grid_cells.begin(), grid_cells.end(), g);

    // Iterate through the sampled grid cells
    for (const std::string& grid_cell : grid_cells) {
      std::vector<int> points = grid[grid_cell];

      for (size_t i = 0; i < points.size(); ++i) {
        int point1_idx = points[i];
        if (!trial_keep_points[point1_idx]) continue;

        double lon1 = coordinates(point1_idx, 0);
        double lat1 = coordinates(point1_idx, 1);

        // Check neighboring grid cells
        for (int lat_offset = -1; lat_offset <= 1; ++lat_offset) {
          for (int lon_offset = -1; lon_offset <= 1; ++lon_offset) {
            int neighbor_lon = static_cast<int>(std::round(lon1 / precision)) + lon_offset;
            int neighbor_lat = static_cast<int>(std::round(lat1 / precision)) + lat_offset;
            std::string neighbor_cell = std::to_string(neighbor_lon) + "_" + std::to_string(neighbor_lat);

            if (grid.find(neighbor_cell) != grid.end()) {
              for (int point2_idx : grid[neighbor_cell]) {
                if (point2_idx != point1_idx && trial_keep_points[point2_idx]) {
                  double lon2 = coordinates(point2_idx, 0);
                  double lat2 = coordinates(point2_idx, 1);

                  // Calculate distance based on the specified metric
                  double distance;
                  if (distance_metric == "haversine") {
                    distance = haversineDistance(lon1, lat1, lon2, lat2, R);
                  } else {
                    distance = euclideanDistance(lon1, lat1, lon2, lat2);
                  }

                  // Check if within the thin distance
                  if (distance <= thin_dist) {
                    trial_keep_points[point2_idx] = false; // Mark as not kept
                  }
                }
              }
            }
          }
        }
      }
    }

    // Update the overall keep_points if this trial is better
    int current_size = Rcpp::sum(trial_keep_points);
    if (current_size > best_size) {
      keep_points = trial_keep_points; // Keep this trial's results
      best_size = current_size;
    }

    // Store the trial results if all_trials is true
    if (all_trials) {
      all_keep_points.push_back(trial_keep_points);
    }
  }

  // Return the logical vector indicating which points are kept, wrapped in a list
  if (all_trials) {
    return all_keep_points; // Return the list of all trials
  } else {
    return Rcpp::List::create(keep_points); // Return the best trial in a list
  }
}
