#---
# Excerpted from "Programming Ecto",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/wmecto for more book information.
#---
import ExUnit.Assertions

alias MusicDB.Repo

if Repo.using_postgres?() do
  result =
  Repo.insert_all("artists", [%{name: "Max Roach"},
    %{name: "Art Blakey"}], returning: [:id, :name])
  #=> {2, [%{id: 12, name: "Max Roach"}, %{id: 13, name: "Art Blakey"}]}

  assert {2, [%{}, %{}]} = result
end

result =
Ecto.Adapters.SQL.query(Repo, "select * from artists where id=1")
#=> {:ok,
#=>  %Postgrex.Result{
#=>    columns: ["id", "name", "birth_date", "death_date", "inserted_at",
#=>     "updated_at"],
#=>    command: :select,
#=>    connection_id: 3333,
#=>    messages: [],
#=>    num_rows: 1,
#=>    rows: [
#=>      [1, "Miles Davis", nil, nil, ~N[2018-1-05 23:32:31.000000],
#=>       ~N[2018-1-05 23:32:31.000000]]
#=>    ]
#=>  }}

if Repo.using_postgres?() do
  assert {:ok, %Postgrex.Result{}} = result
else
  assert {:ok, %Mariaex.Result{}} = result
end

result =
Repo.query("select * from artists where id=1")

if Repo.using_postgres?() do
  assert {:ok, %Postgrex.Result{}} = result
else
  assert {:ok, %Mariaex.Result{}} = result
end

