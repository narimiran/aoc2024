defmodule Day09 do
  defp parse_data(input) do
    input
    |> AOC.parse_line(:digits)
    |> Enum.with_index()
    |> Enum.flat_map(fn {n, i} ->
      idx = if rem(i, 2) == 0, do: div(i, 2)
      List.duplicate(idx, n)
    end)
    |> List.to_tuple()
  end

  defp defrag(_disk, fi, bi, res) when fi > bi, do: Enum.reverse(res)

  defp defrag(disk, fi, bi, res) do
    dfi = elem(disk, fi)
    dbi = elem(disk, bi)

    cond do
      not is_nil(dfi) -> defrag(disk, fi + 1, bi, [dfi | res])
      not is_nil(dbi) -> defrag(disk, fi + 1, bi - 1, [dbi | res])
      true -> defrag(disk, fi, bi - 1, res)
    end
  end

  defp defragment(disk), do: defrag(disk, 0, tuple_size(disk) - 1, [])

  defp calc_checksum(disk) do
    import Enum

    disk
    |> with_index
    |> map(fn {n, i} -> n * i end)
    |> sum
  end

  def solve(filename) do
    filename
    |> AOC.read_input()
    |> parse_data
    |> defragment
    |> calc_checksum
  end
end

Day09.solve(9)
