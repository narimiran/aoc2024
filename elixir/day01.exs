defmodule Day01 do
  import Enum

  defp sort_cols(lines) do
    lines
    |> zip
    |> map(&sort(Tuple.to_list(&1)))
  end

  defp part_1(cols) do
    cols
    |> zip
    |> map(fn {x, y} -> abs(x - y) end)
    |> sum
  end

  defp part_2([left, right]) do
    freqs = frequencies(right)

    left
    |> map(fn x -> x * Map.get(freqs, x, 0) end)
    |> sum
  end

  def solve(filename) do
    import AOC

    filename
    |> read_input()
    |> parse_lines(:ints)
    |> sort_cols
    |> juxt([&part_1/1, &part_2/1])
  end
end

Day01.solve(1)
