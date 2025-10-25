defmodule Day10 do
  defp parse_input(input) do
    import AOC

    input
    |> parse_lines(:digits)
    |> grid_to_point_map
  end

  defp find_trailheads(grid) do
    import Map

    grid
    |> filter(fn {_, v} -> v == 0 end)
    |> keys
  end

  defp dfs_aux(_grid, [], ends) do
    [ends |> Enum.uniq() |> length, ends |> length]
  end

  defp dfs_aux(grid, [curr | rest], ends) do
    curr_h = Map.get(grid, curr)

    if curr_h == 9 do
      dfs_aux(grid, rest, [curr | ends])
    else
      nbs = AOC.neighbours_4(curr, &(Map.get(grid, &1, 0) == curr_h + 1))
      dfs_aux(grid, nbs ++ rest, ends)
    end
  end

  defp dfs(grid, start) do
    dfs_aux(grid, [start], [])
  end

  def solve(filename) do
    import Enum

    grid =
      filename
      |> AOC.read_input()
      |> parse_input

    grid
    |> find_trailheads
    |> map(&dfs(grid, &1))
    |> zip
    |> map(&Tuple.sum/1)
  end
end

Day10.solve(10)
