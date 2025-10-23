defmodule Day08 do
  defp parse_input(filename) do
    import AOC
    lines = filename |> read_input |> parse_lines(:chars)
    size = length(lines)
    antennas = grid_to_point_map(lines, &(&1 != "."))
    {size, antennas}
  end

  defp antinodes({size, antennas}, multis) do
    for {pt1, freq1} <- antennas,
        {pt2, freq2} <- antennas,
        freq1 == freq2,
        pt1 != pt2,
        into: MapSet.new() do
      dist = AOC.pt_sub(pt2, pt1)

      Enum.reduce_while(multis, MapSet.new(), fn n, acc ->
        pt = AOC.pt_add(pt2, AOC.pt_mul(n, dist))

        if AOC.inside?(pt, size) do
          {:cont, MapSet.put(acc, pt)}
        else
          {:halt, acc}
        end
      end)
    end
    |> Enum.reduce(&MapSet.union/2)
    |> Enum.count()
  end

  def solve(filename) do
    data = filename |> parse_input
    {antinodes(data, [1]), antinodes(data, Stream.iterate(0, &(&1 + 1)))}
  end
end

Day08.solve(8)
