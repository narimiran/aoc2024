defmodule Day07 do
  defp conc(a, b), do: String.to_integer(to_string(a) <> to_string(b))

  defp valid_equation?(res, nums, concat_op?) do
    case nums do
      [] ->
        false

      [a] ->
        a == res

      [a | _] when a > res ->
        false

      [a, b | tail] ->
        ([a + b, a * b] ++ if(concat_op?, do: [conc(a, b)], else: []))
        |> Enum.any?(fn op -> valid_equation?(res, [op | tail], concat_op?) end)
    end
  end

  defp valid?(lines, concat_op?) do
    import Enum

    lines
    |> filter(fn [res | nums] -> valid_equation?(res, nums, concat_op?) end)
    |> map(&hd/1)
    |> sum
  end

  def solve(filename) do
    import AOC

    filename
    |> read_input
    |> parse_lines(:ints)
    |> then(fn x -> {valid?(x, false), valid?(x, true)} end)
  end
end

Day07.solve(7)
