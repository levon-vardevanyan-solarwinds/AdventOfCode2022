#include <algorithm>
#include <deque>
#include <fstream>
#include <iostream>
#include <set>
#include <stdexcept>
#include <string>
#include <vector>

using Matrix = std::vector<std::vector<char>>;
using Point = std::pair<int, int>;

Point find_and_replace(Matrix& matrix, char from, char to)
{
	for (std::size_t i = 0; i < matrix.size(); ++i)
	{
		for (std::size_t j = 0; j < matrix[i].size(); ++j)
		{
			if (matrix[i][j] == from)
			{
				matrix[i][j] = to;
				return Point{ i, j };
			}
		}
	}

	throw std::logic_error("not found");
}

std::vector<Point> find_all(Matrix& matrix, char needle)
{
	std::vector<Point> result;

	for (std::size_t i = 0; i < matrix.size(); ++i)
	{
		for (std::size_t j = 0; j < matrix[i].size(); ++j)
		{
			if (matrix[i][j] == needle)
			{
				result.push_back(Point{ i,j });
			}
		}
	}

	return result;
}


std::set<Point> neighbours(Matrix const& matrix, Point current)
{
	std::set<Point> result;

	auto is_valid = [&matrix](Point p)
	{
		return p.first >= 0 && p.first < matrix.size()
			&& p.second >= 0 && p.second < matrix.front().size();
	};

	auto diff = [&matrix, current](Point to)
	{
		return matrix[to.first][to.second] - matrix[current.first][current.second];
	};

	for (auto const& delta : { Point{1,0}, Point{0,1}, Point{-1,0}, Point{0,-1} })
	{
		Point neighbour{ current.first + delta.first, current.second + delta.second };
		if (is_valid(neighbour) && diff(neighbour) <= 1)
		{
			result.insert(std::move(neighbour));
		}
	}

	return result;
}

int BFS(Matrix const& matrix, Point start, Point finish)
{
	std::size_t const N = matrix.size();
	std::size_t const M = matrix.front().size();

	std::vector<std::vector<int>> distances;
	for (std::size_t i = 0; i < N; ++i)
	{
		distances.push_back(std::vector<int>(M, 0));
	}

	std::set<Point> visited;
	distances[start.first][start.second] = 0;
	for (std::deque<Point> queue{ start }; !queue.empty(); queue.pop_front())
	{
		if (queue.front() == finish)
		{
			return distances[queue.front().first][queue.front().second];
		}

		auto const adjacent = neighbours(matrix, queue.front());
		std::vector<Point> check_next;
		std::set_difference(adjacent.begin(), adjacent.end(),
			visited.begin(), visited.end(),
			std::back_inserter(check_next));


		int const distance = distances[queue.front().first][queue.front().second] + 1;
		for (auto const& point : check_next)
		{
			if (std::find(queue.begin(), queue.end(), point) == queue.end())
			{
				distances[point.first][point.second] = distance;
				queue.push_back(point);
			}
		}

		visited.insert(queue.front());
	}

	return -1;
}

int main()
{
	std::ifstream input("input.txt");
	Matrix matrix;
	for (std::string buffer; input >> buffer;)
	{
		matrix.push_back(std::vector(buffer.begin(), buffer.end()));
	}

	auto const start = find_and_replace(matrix, 'S', 'a');
	auto const finish = find_and_replace(matrix, 'E', 'z');

	std::cout << "Part 1: " << BFS(matrix, start, finish) << std::endl;

	auto const starts = find_all(matrix, 'a');
	std::vector<int> results;
	std::transform(starts.begin(), starts.end(), std::back_inserter(results),
		[&matrix, finish](Point from)
		{
			return BFS(matrix, from, finish);
		});


	std::vector<int> filtered;
    std::copy_if(results.begin(), results.end(), std::back_inserter(filtered),
            [](int x) { return x != -1; });

	std::cout << "Part 2: " << *std::min_element(filtered.begin(), filtered.end()) << std::endl;

	return 0;
}
