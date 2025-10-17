defmodule Day02 do
  import Enum

  defp safe?(line) do
    differences =
      line
      |> zip(tl(line))
      |> map(fn {a, b} -> b - a end)

    all?(differences, &(&1 in 1..3)) or all?(differences, &(&1 in -3..-1))
  end

  defp single_bad_bit?(line) do
    line
    |> with_index
    |> any?(fn {_, i} -> line |> List.delete_at(i) |> safe?() end)
  end

  defp safe_2?(line), do: safe?(line) or single_bad_bit?(line)

  def solve(filename) do
    input =
      filename
      |> AOC.read_input()
      |> AOC.parse_lines(:ints)

    {input |> count(&safe?/1), input |> count(&safe_2?/1)}
  end
end

Day02.solve(2)
