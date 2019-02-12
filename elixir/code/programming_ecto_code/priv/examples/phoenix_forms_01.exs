#---
# Excerpted from "Programming Ecto",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/wmecto for more book information.
#---
defmodule MyApp.User do
  import Ecto.Changeset
  use Ecto.Schema

  schema "users" do
    field :name, :string
    field :age, :integer
  end

  def changeset(user, params) do
    user
    |> cast(params, [:name, :age])
    |> validate_required(:name)
    |> validate_number(:age, greater_than: 0,
         message: "you are not yet born")
  end

end

_ = """
defmodule MyApp.FakeController do
  def new(conn, _params) do
    changeset = User.changeset(%User{}, %{})
    render(conn, changeset: changeset)
  end
end
"""

_ = """
<%= form_for @changeset, user_path(@conn, :create), fn f -> %>
  Name: <%= text_input f, :name %>
  Age: <%= number_input f, :age %>
  <%= submit "Submit" %>
<% end %>
"""

_ = """
def create(conn, %{"user" => user_params}) do
  case Accounts.create_user(user_params) do
    {:ok, user} ->
      conn
      |> put_flash(:info, "User created successfully.")
      |> redirect(to: user_path(conn, :show, user))
    {:error, %Ecto.Changeset{} = changeset} ->
      render(conn, "new.html", changeset: changeset)
  end
end
"""

_ = """
def create_user(attrs \\ %{}) do
  %User{}
  |> User.changeset(attrs)
  |> Repo.insert()
end
"""

