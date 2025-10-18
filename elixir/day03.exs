defmodule Day03 do
  import Enum

  @pattern ~r/mul\((\d+),(\d+)\)|do\(\)|don't\(\)/

  defp get_instructions(line), do: Regex.scan(@pattern, line)

  def solve(filename) do
    filename
    |> AOC.read_input()
    |> get_instructions
    |> reduce({true, 0, 0}, fn
      ["don't()"], {_enabled?, p1, p2} ->
        {false, p1, p2}

      ["do()"], {_enabled?, p1, p2} ->
        {true, p1, p2}

      [_, a, b], {enabled?, p1, p2} ->
        mult = String.to_integer(a) * String.to_integer(b)

        if enabled? do
          {true, p1 + mult, p2 + mult}
        else
          {false, p1 + mult, p2}
        end
    end)
    |> then(fn {_, p1, p2} -> {p1, p2} end)
  end
end

Day03.solve(3)
