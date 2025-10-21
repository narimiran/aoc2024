defmodule Day06 do
  defp parse_input(filename) do
    input = AOC.read_input(filename) |> AOC.parse_lines(:chars)
    walls = input |> AOC.grid_to_point_set(&(&1 == "#"))
    guard = input |> AOC.grid_to_point_set(&(&1 == "^")) |> Enum.at(0)
    %{walls: walls, guard: guard, size: length(input)}
  end

  defp turn_right({dx, dy}), do: {-dy, dx}

  defp step(pos, dir, path, walls, size) do
    new_pos = AOC.pt_add(pos, dir)

    cond do
      not AOC.inside?(new_pos, size) -> path
      MapSet.member?(walls, new_pos) -> step(pos, turn_right(dir), path, walls, size)
      true -> step(new_pos, dir, MapSet.put(path, new_pos), walls, size)
    end
  end

  defp guard_path(%{walls: walls, guard: guard, size: size}) do
    step(guard, {0, -1}, MapSet.new([guard]), walls, size)
  end

  defp step_2(pos, dir, walls, size, turns) do
    new_pos = AOC.pt_add(pos, dir)

    cond do
      not AOC.inside?(new_pos, size) ->
        0

      MapSet.member?(turns, {pos, dir}) ->
        1

      MapSet.member?(walls, new_pos) ->
        step_2(pos, turn_right(dir), walls, size, MapSet.put(turns, {pos, dir}))

      true ->
        step_2(new_pos, dir, walls, size, turns)
    end
  end

  defp part_2(%{walls: walls, guard: guard, size: size}, visited) do
    visited
    |> Task.async_stream(fn x ->
      step_2(guard, {0, -1}, MapSet.put(walls, x), size, MapSet.new())
    end)
    |> Enum.sum_by(fn {:ok, num} -> num end)
  end

  def solve(filename) do
    input = parse_input(filename)
    visited = guard_path(input)

    {MapSet.size(visited), part_2(input, visited)}
  end
end

Day06.solve(6)
