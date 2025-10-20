defmodule Day05 do
  import Enum

  defp valid_line?(line, rules) do
    line
    |> chunk_every(2, 1, :discard)
    |> all?(&MapSet.member?(rules, &1))
  end

  defp middle_page(line) do
    at(line, div(length(line), 2))
  end

  defp sort_line(line, rules) do
    sort(line, &MapSet.member?(rules, [&1, &2]))
  end

  defp part_1([rules, updates]) do
    updates
    |> filter(&valid_line?(&1, rules))
    |> map(&middle_page/1)
    |> sum
  end

  defp part_2([rules, updates]) do
    updates
    |> reject(&valid_line?(&1, rules))
    |> map(&sort_line(&1, rules))
    |> map(&middle_page/1)
    |> sum
  end

  def solve(filename) do
    import AOC

    filename
    |> read_input
    |> parse_paragraphs(:ints)
    |> map(&MapSet.new/1)
    |> juxt([&part_1/1, &part_2/1])
  end
end

Day05.solve(5)
