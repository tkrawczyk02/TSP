// Name        : TSP.cpp
// Author      : Tomasz Krawczyk
//===========================================================================

#include <iostream>
#include <climits>
#include <vector>
#include <cfloat>
#include <iomanip>
#include <limits>
#include <cmath>
#include <set>


#define INF DBL_MAX

void const print_matrix(std::vector<std::vector<double>> const &matrix) {

    for (std::size_t i = 0; i < matrix.size(); i++) {
        for (std::size_t j = 0; j < matrix.size(); j++) {
            if (matrix[i][j] == INF) {
                std::cout << std::setw(4) << "INF" << ' ';
            } else {
                std::cout << std::setw(4) << matrix[i][j] << ' ';
            }
        }
        std::cout << std::endl;
    }
    std::cout << std::endl;
}

void reduce_rows(std::vector<std::vector<double>> &matrix, double &lower_bound) {
    double minimum[matrix.size()];
    for (std::size_t i = 0; i < matrix.size(); i++) {
        double temp = INF;
        for (std::size_t j = 0; j < matrix.size(); j++) {
            if (matrix[i][j] < temp) {
                temp = matrix[i][j];
            }
        }
        minimum[i] = temp;
        if (temp != INF) lower_bound += temp;
    }
    for (std::size_t i = 0; i < matrix.size(); i++) {
        for (std::size_t j = 0; j < matrix.size(); j++) {
            if (matrix[i][j] != INT_MAX) {
                matrix[i][j] -= minimum[i];
            }
        }
    }
}


void reduce_cols(std::vector<std::vector<double>> &matrix, double &lower_bound) {
    double minimum[matrix.size()];
    for (std::size_t i = 0; i < matrix.size(); i++) {
        double temp = INF;
        for (std::size_t j = 0; j < matrix.size(); j++) {
            if (matrix[j][i] < temp) {
                temp = matrix[j][i];
            }
        }
        minimum[i] = temp;
        if (temp != INF) lower_bound += temp;
    }
    for (std::size_t i = 0; i < matrix.size(); i++) {
        for (std::size_t j = 0; j < matrix.size(); j++) {
            if (matrix[j][i] != INT_MAX) {
                matrix[j][i] -= minimum[i];
            }
        }
    }

}

std::vector<std::pair<int, int>> const find_pairs(std::vector<std::vector<double>> const &matrix) {
    std::vector<std::pair<int, int>> pairs;
    for (std::size_t i = 0; i < matrix.size(); i++) {
        for (std::size_t j = 0; j < matrix.size(); j++) {
            if (matrix[i][j] == 0) {
                pairs.push_back(std::make_pair(i, j));

            }
        }
    }
    return pairs;
}

std::vector<std::pair<std::pair<int, int>, double>> const
calculate_wages(std::vector<std::vector<double>> const &matrix, std::vector<std::pair<int, int>> const &pairs) {

    std::vector<std::pair<std::pair<int, int>, double>> pairs_with_wages;
    for (std::size_t x = 0; x < pairs.size(); x++) {
        double row_min = INF;
        double col_min = INF;
        double sum = 0;
        for (std::size_t i = 0; i < matrix.size(); i++) {
            if (matrix[pairs[x].first][i] < col_min && i != pairs[x].second && matrix[pairs[x].first][i] != INF) {
                col_min = matrix[pairs[x].first][i];
            }
        }

        for (std::size_t j = 0; j < matrix.size(); j++) {
            if (matrix[j][pairs[x].second] < row_min && j != pairs[x].first && matrix[j][pairs[x].second] != INF) {
                row_min = matrix[j][pairs[x].second];
            }
        }
        sum = col_min + row_min;
        pairs_with_wages.push_back(std::make_pair(pairs[x], sum));
    }


    return pairs_with_wages;
}


void choose_path(std::vector<std::pair<int, int >> &current_path,
                 std::vector<std::pair<std::pair<int, int>, double>> const &pairs,
                 std::vector<std::vector<double>> &matrix) {
    double value = 0;
    std::size_t temp = 0;

    for (std::size_t i = 0; i < pairs.size(); i++) {
        if (pairs[i].second > value) {
            value = pairs[i].second;
            temp = i;
        }
    }

    current_path.push_back(std::make_pair(((pairs[temp].first).first) + 1, ((pairs[temp].first).second) + 1));

    for (std::size_t i = 0; i < matrix.size(); i++) {
        matrix[(pairs[temp].first).first][i] = NAN;
    }
    for (std::size_t i = 0; i < matrix.size(); i++) {
        matrix[i][(pairs[temp].first).second] = NAN;
    }
    matrix[(pairs[temp].first).second][(pairs[temp].first).first] = NAN;
}

void cross_diagonal_out(std::vector<std::vector<double>> &matrix) {
    for (std::size_t i = 0; i < matrix.size(); i++) {
        for (std::size_t j = 0; j < matrix.size(); j++) {
            if (i == j) matrix[i][j] = INF;
        }

    }

}

bool const is_empty(std::vector<std::vector<double>> const &cost_matrix) {

    for (std::size_t i = 0; i < cost_matrix.size(); i++) {
        for (std::size_t j = 0; j < cost_matrix.size(); j++) {
            if (!std::isnan(cost_matrix[i][j])) {
                return false;
            }
        }
    }
    return true;
}

void reduce_matrix(std::vector<std::vector<double>> &matrix, double &lower_bound) {
    reduce_rows(matrix, lower_bound);
    reduce_cols(matrix, lower_bound);
}


std::vector<int> const sort_path(std::vector<std::pair<int, int >> const &path) {
    std::vector<int> final_path;
    final_path.push_back(path[0].first);
    final_path.push_back(path[0].second);
    int temp;
    temp = path[0].second;


    while (final_path.size() < path.size()) {

        for (std::size_t i = 1; i < path.size(); i++) {
            if (path[i].first == temp) {
                final_path.push_back(path[i].second);
                temp = path[i].second;
                break;
            }
        }
    }
    final_path.push_back(path[0].first);
    return final_path;
}


std::vector<int> tsp(std::vector<std::vector<double>> &cost_matrix) {
    double lower_bound = 0;
    std::vector<std::pair<int, int >> current_path;

    cross_diagonal_out(cost_matrix);
    while (!is_empty(cost_matrix)) {
        reduce_matrix(cost_matrix, lower_bound);
        std::vector<std::pair<int, int>> pairs = find_pairs(cost_matrix);
        std::vector<std::pair<std::pair<int, int>, double>> pairs_with_wages = calculate_wages(cost_matrix, pairs);
        choose_path(current_path, pairs_with_wages, cost_matrix);
    }
    return sort_path(current_path);

}

int main() {
    std::vector<std::vector<double>> cost_matrix
            {
                    {INF, 10, 8,   19, 12},
                    {10, INF, 20,  6,  3},
                    {8,   20, INF, 4,  2},
                    {19,  6,  4, INF,  7},
                    {12,  3,  2,   7, INF}
            };


    std::vector<int> best_path = tsp(cost_matrix);


    std::cout << "final path:" << std::endl;
    for (const auto i: best_path)
        std::cout << i << ' ';

};