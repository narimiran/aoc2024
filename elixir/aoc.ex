defmodule AOC do
  import Enum

  def juxt(x, funcs) do
    funcs
    |> map(fn func -> func.(x) end)
  end

  def read_input(file) do
    file =
      if is_integer(file) do
        String.pad_leading(to_string(file), 2, "0")
      else
        file
      end

    File.read!("../inputs/#{file}.txt")
    |> String.trim()
  end

  def integers(s, negative \\ true) do
    pattern = if negative, do: ~r/\-?\d+/, else: ~r/\d+/

    Regex.scan(pattern, s)
    |> map(&hd/1)
    |> map(&String.to_integer/1)
  end

  def chars(s) do
    String.split(s, "", trim: true)
  end

  def digits(s) do
    s |> chars |> map(&String.to_integer/1)
  end

  def parse_line(line, parse_func \\ nil, word_sep \\ " ") do
    func =
      case parse_func do
        :int -> &String.to_integer/1
        :ints -> &integers/1
        :digits -> &digits/1
        :chars -> &String.graphemes/1
        :words -> &String.split(&1, word_sep)
        nil -> &Function.identity/1
        _ -> parse_func
      end

    func.(line)
  end

  def parse_lines(lines, parse_func \\ nil, opts \\ []) do
    word_sep = Keyword.get(opts, :word_sep, " ")
    line_sep = Keyword.get(opts, :line_sep, "\n")

    lines
    |> String.split(line_sep, trim: true)
    |> map(&parse_line(&1, parse_func, word_sep))
  end

  def parse_paragraphs(lines, parse_func \\ nil, word_sep \\ " ") do
    lines
    |> parse_lines(nil, line_sep: "\n\n")
    |> map(&parse_lines(&1, parse_func, word_sep: word_sep))
  end

  def grid_to_point_set(lines, pred \\ &Function.identity/1) do
    for {line, y} <- Enum.with_index(lines),
        {char, x} <- Enum.with_index(line),
        pred.(char),
        into: MapSet.new(),
        do: {x, y}
  end

  def grid_to_point_map(lines, pred \\ &Function.identity/1) do
    for {line, y} <- Enum.with_index(lines),
        {char, x} <- Enum.with_index(line),
        pred.(char),
        into: %{},
        do: {{x, y}, char}
  end

  def neighbours_4(pt) do
    neighbours_4(pt, &Function.identity/1)
  end

  def neighbours_4({x, y}, cnd) do
    for dy <- [-1, 0, 1],
        dx <- [-1, 0, 1],
        rem(dx - dy, 2) != 0,
        nb = {x + dx, y + dy},
        cnd.(nb),
        do: nb
  end

  def pt_add({x1, y1}, {x2, y2}) do
    {x1 + x2, y1 + y2}
  end

  def pt_sub({x1, y1}, {x2, y2}) do
    {x1 - x2, y1 - y2}
  end

  def pt_mul(magnitude, {x, y}) do
    {magnitude * x, magnitude * y}
  end

  def inside?({x, y}, size) do
    x >= 0 and x < size and y >= 0 and y < size
  end

  defmacro time(expr) do
    quote do
      {us, result} = :timer.tc(fn -> unquote(expr) end)
      IO.puts("Elapsed: #{Float.round(us / 1_000_000, 4)}s")
      result
    end
  end
end
