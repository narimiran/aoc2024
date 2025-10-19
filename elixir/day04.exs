defmodule Day04 do
  defp parse_input(filename) do
    import AOC

    filename
    |> read_input
    |> parse_lines(:chars)
    |> grid_to_point_map
  end

  defp part_1(grid) do
    letters = Enum.with_index(String.graphemes("XMAS"))

    for {{x, y}, c} <- grid,
        c == "X",
        dx <- [-1, 0, 1],
        dy <- [-1, 0, 1],
        Enum.all?(
          letters,
          fn {l, i} ->
            l == Map.get(grid, {x + i * dx, y + i * dy})
          end
        ) do
      1
    end
    |> Enum.count()
  end

  defp check_diags(grid, x, y) do
    diags = [["M", "A", "S"], ["S", "A", "M"]]
    deltas = [-1, 0, 1]
    d1 = for d <- deltas, do: Map.get(grid, {x + d, y + d})
    d2 = for d <- deltas, do: Map.get(grid, {x - d, y + d})

    d1 in diags and d2 in diags
  end

  defp part_2(grid) do
    Map.keys(grid)
    |> Enum.count(fn {x, y} -> check_diags(grid, x, y) end)
  end

  def solve(filename) do
    filename
    |> parse_input
    |> AOC.juxt([&part_1/1, &part_2/1])
  end
end

Day04.solve(4)
