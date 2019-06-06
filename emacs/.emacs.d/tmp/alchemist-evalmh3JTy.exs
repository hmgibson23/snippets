require Database
require Logger

defmodule Analysis do
  use GenServer
  # runs analysis and caches the state in memory

  def get_lt_for(value, house) do
    vals = Database.get_values(house)
    # {_, vals} = store

    available = Enum.filter(
      vals,
      fn x ->
        {_,v} = x
        v.available?
      end
    )

    Enum.filter(
      available,
      fn x ->
        {_,v} = x
        v.price <= value end)
  end

  def yield(initial_cost) do
    4 * 3
  end

  def available_to_csv(auction_house) do
    {auction_house, vals} = Database.get_values(auction_house) |> Enum.at(0)
    available = Enum.filter(
      vals,
      fn x ->
        x.available?
      end
    )

    Helpers.to_csv(available)
  end

  def available_list_to_csv(houses) when is_list(houses) do
    vals = Database.get_all(houses)
    available = Enum.filter(
      vals,
      fn x ->
        x.available?
      end
    )

    Helpers.to_csv(available)
  end

  def all_to_csv do
    Analysis.available_list_to_csv([
      "allsop",
      "savills",
      "barnetross",
      "barnardmarcus",
      "auctionhouse",
      "philliparnold",
      "countrywide",
      "shobrook",
      "seels",
      "symonds",
      "brownco"
    ])
  end
end
