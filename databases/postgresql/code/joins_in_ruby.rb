# * Implement SQL joins for Ruby Array of Arrays.
# * The Ruby code is designed to help me understand the algorithms rather than be idiomatic Ruby
# * I'm only confident that these algorithms are correct to the extent that the tests inlined below pass. There are probably edge cases I'm not aware of.
# All SQL tested on Postgres 11.3

def cross_join(lhs_table, rhs_table)
  output = []

  # TODO: the rhs and lhs are iterated the opposite way to what I would expect?
  rhs_table.each do |rhs_row|
    lhs_table.each do |lhs_row|
        output << lhs_row + rhs_row
    end
  end

  output
end

def inner_join(lhs_table, rhs_table, &block)
  output = []

  lhs_table.each do |lhs_row|
    rhs_table.each do |rhs_row|
        if block.call(lhs_row, rhs_row)
          output << lhs_row + rhs_row
        else
          # do nothing
        end
    end
  end

  output
end

def left_join(lhs_table, rhs_table, &block)
  output = []
  unmatched_lhs_rows = []

  lhs_table.each do |lhs_row|
    matching_rhs_rows = rhs_table.select do |rhs_row|
      block.call(lhs_row, rhs_row)
    end

    matching_rhs_rows.each do |rhs_row|
      output << lhs_row + rhs_row
    end

    unmatched_lhs_rows << lhs_row if matching_rhs_rows.length == 0
  end

  # only add the unmatched rows after we have added all the matching ones
  unmatched_lhs_rows.each do |lhs_row|
    output << lhs_row + Array.new(rhs_table.first.length, nil)
  end

  output
end

def right_join(lhs_table, rhs_table, &block)
  output = []
  unmatched_rhs_rows = []

  rhs_table.each do |rhs_row|
    matching_lhs_rows = lhs_table.select do |lhs_row|
      block.call(lhs_row, rhs_row)
    end

    matching_lhs_rows.each do |lhs_row|
      output << lhs_row + rhs_row
    end

    unmatched_rhs_rows << rhs_row if matching_lhs_rows.length == 0
  end

  # only add the unmatched rows after we have added all the matching ones
  unmatched_rhs_rows.each do |rhs_row|
    output << Array.new(lhs_table.first.length, nil) + rhs_row
  end

  output
end

def full_join(lhs_table, rhs_table, &block)
  # create a UNION of a LEFT JOIN and a RIGHT JOIN
  output = left_join(lhs_table, rhs_table, &block) + right_join(lhs_table, rhs_table, &block)

  # remove duplicates
  output.uniq
end

# create table users (id bigint primary key not null, name text, org_id bigint);
# create table organisations (id bigint primary key not null, name text);
# insert into users (id, name, org_id) values (1, 'joe', 1), (2, 'mary', 1), (3, 'brian', 3), (4, 'mike', null);
# insert into organisations (id, name) values (1, 'Foo Inc'), (2, 'Bar Inc'), (3, 'Baz Inc');

users = [
  # id, name, org_id
  [ 1, "joe", 1 ],
  [ 2, "mary", 1 ],
  [ 3, "brian", 3 ],
  [ 4, "mike", nil ],
]

organisations = [
  # id, name
  [ 1, "Foo Inc." ],
  [ 2, "Bar Inc." ],
  [ 3, "Baz Inc." ],
]


# ########################################################################### #

puts "*" * 80
puts "users CROSS JOIN organisations"
puts ""
cross_join_results = cross_join(users, organisations)
pp cross_join_results
puts "Found #{cross_join_results.length} rows."

# SQL output:
#
# postgres@eoin=#  select * from users CROSS JOIN organisations;
#  id │ name  │ org_id │ id │  name
# ════╪═══════╪════════╪════╪═════════
#   1 │ joe   │      1 │  1 │ Foo Inc
#   2 │ mary  │      1 │  1 │ Foo Inc
#   3 │ brian │      3 │  1 │ Foo Inc
#   4 │ mike  │      ¤ │  1 │ Foo Inc
#   1 │ joe   │      1 │  2 │ Bar Inc
#   2 │ mary  │      1 │  2 │ Bar Inc
#   3 │ brian │      3 │  2 │ Bar Inc
#   4 │ mike  │      ¤ │  2 │ Bar Inc
#   1 │ joe   │      1 │  3 │ Baz Inc
#   2 │ mary  │      1 │  3 │ Baz Inc
#   3 │ brian │      3 │  3 │ Baz Inc
#   4 │ mike  │      ¤ │  3 │ Baz Inc
#   (12 rows)
#
# Ruby output:
#
# ********************************************************************************
# users CROSS JOIN organisations
#
# [[1, "joe", 1, 1, "Foo Inc."],
#  [2, "mary", 1, 1, "Foo Inc."],
#  [3, "brian", 3, 1, "Foo Inc."],
#  [4, "mike", nil, 1, "Foo Inc."],
#  [1, "joe", 1, 2, "Bar Inc."],
#  [2, "mary", 1, 2, "Bar Inc."],
#  [3, "brian", 3, 2, "Bar Inc."],
#  [4, "mike", nil, 2, "Bar Inc."],
#  [1, "joe", 1, 3, "Baz Inc."],
#  [2, "mary", 1, 3, "Baz Inc."],
#  [3, "brian", 3, 3, "Baz Inc."],
#  [4, "mike", nil, 3, "Baz Inc."]]
# Found 12 rows.


# ########################################################################### #

puts "*" * 80
puts "users INNER JOIN organisations"
puts ""

inner_join_results = inner_join(users, organisations) do |user, org|
  user[2] == org[0]
end

pp inner_join_results
puts "Found #{inner_join_results.length} rows."

# postgres@eoin=#  select * from users AS u INNER JOIN organisations AS o ON u.org_id = o.id;
#  id │ name  │ org_id │ id │  name
# ════╪═══════╪════════╪════╪═════════
#   1 │ joe   │      1 │  1 │ Foo Inc
#   2 │ mary  │      1 │  1 │ Foo Inc
#   3 │ brian │      3 │  3 │ Baz Inc
# (3 rows)
#
# Time: 6.303 ms
#
# Ruby output:
#
# ********************************************************************************
# users INNER JOIN organisations
#
# [[1, "joe", 1, 1, "Foo Inc."],
#  [2, "mary", 1, 1, "Foo Inc."],
#  [3, "brian", 3, 3, "Baz Inc."]]
# Found 3 rows.


# ########################################################################### #
puts "*" * 80
puts "users LEFT JOIN organisations"
puts ""

left_join_results = left_join(users, organisations) do |user, org|
  user[2] == org[0]
end

pp left_join_results
puts "Found #{left_join_results.length} rows."

# postgres@eoin=#  select * from users as u left join organisations as o on u.org_id = o.id;
#  id │ name  │ org_id │ id │  name
# ════╪═══════╪════════╪════╪═════════
#   1 │ joe   │      1 │  1 │ Foo Inc
#   2 │ mary  │      1 │  1 │ Foo Inc
#   3 │ brian │      3 │  3 │ Baz Inc
#   4 │ mike  │      ¤ │  ¤ │ ¤
# (4 rows)
#
# Time: 3.439 ms
#
# Ruby output:
#
# ********************************************************************************
# users LEFT JOIN organisations
#
# [[1, "joe", 1, 1, "Foo Inc."],
#  [2, "mary", 1, 1, "Foo Inc."],
#  [3, "brian", 3, 3, "Baz Inc."],
#  [4, "mike", nil, nil, nil]]
# Found 4 rows.

# ########################################################################### #
# select * from users as u right join organisations as o on u.org_id = o.id;
puts "*" * 80
puts "users RIGHT JOIN organisations"
puts ""

right_join_results = right_join(users, organisations) do |user, org|
  user[2] == org[0]
end

pp right_join_results
puts "Found #{right_join_results.length} rows."

# postgres@eoin=#  select * from users as u right join organisations as o on u.org_id = o.id;
#  id │ name  │ org_id │ id │  name
# ════╪═══════╪════════╪════╪═════════
#   1 │ joe   │      1 │  1 │ Foo Inc
#   2 │ mary  │      1 │  1 │ Foo Inc
#   3 │ brian │      3 │  3 │ Baz Inc
#   ¤ │ ¤     │      ¤ │  2 │ Bar Inc
# (4 rows)
#
# Time: 6.125 ms
#
# Ruby output:
#
# ********************************************************************************
# users RIGHT JOIN organisations
#
# [[1, "joe", 1, 1, "Foo Inc."],
#  [2, "mary", 1, 1, "Foo Inc."],
#  [3, "brian", 3, 3, "Baz Inc."],
#  [nil, nil, nil, 2, "Bar Inc."]]
# Found 4 rows.


# ########################################################################### #
# select * from organisations as o left join users as u on u.org_id = o.id;
puts "*" * 80
puts "organisations LEFT JOIN users"
puts ""

left_join_results = left_join(organisations, users) do |org, user|
  user[2] == org[0]
end

pp left_join_results
puts "Found #{left_join_results.length} rows."

# postgres@eoin=#  select * from organisations as o left join users as u on u.org_id = o.id;
#  id │  name   │ id │ name  │ org_id
# ════╪═════════╪════╪═══════╪════════
#   1 │ Foo Inc │  1 │ joe   │      1
#   1 │ Foo Inc │  2 │ mary  │      1
#   3 │ Baz Inc │  3 │ brian │      3
#   2 │ Bar Inc │  ¤ │ ¤     │      ¤
# (4 rows)
#
# Time: 3.810 ms
#
# Ruby output:
#
# ********************************************************************************
# organisations LEFT JOIN users
#
# [[1, "Foo Inc.", 1, "joe", 1],
#  [1, "Foo Inc.", 2, "mary", 1],
#  [3, "Baz Inc.", 3, "brian", 3],
#  [2, "Bar Inc.", nil, nil, nil]]
# Found 4 rows.


# ########################################################################### #
# select * from users as u full join organisations as o on u.org_id = o.id;
puts "*" * 80
puts "users FULL JOIN organisations"
puts ""

full_join_results = full_join(users, organisations) do |user, org|
  user[2] == org[0]
end

pp full_join_results
puts "Found #{full_join_results.length} rows."

# postgres@eoin=#  select * from users as u full join organisations as o on u.org_id = o.id;
#  id │ name  │ org_id │ id │  name
# ════╪═══════╪════════╪════╪═════════
#   1 │ joe   │      1 │  1 │ Foo Inc
#   2 │ mary  │      1 │  1 │ Foo Inc
#   3 │ brian │      3 │  3 │ Baz Inc
#   4 │ mike  │      ¤ │  ¤ │ ¤
#   ¤ │ ¤     │      ¤ │  2 │ Bar Inc
# (5 rows)
#
# Time: 1.893 ms
#
# Ruby output:
#
# ********************************************************************************
# users FULL JOIN organisations
#
# [[1, "joe", 1, 1, "Foo Inc."],
#  [2, "mary", 1, 1, "Foo Inc."],
#  [3, "brian", 3, 3, "Baz Inc."],
#  [4, "mike", nil, nil, nil],
#  [nil, nil, nil, 2, "Bar Inc."]]
# Found 5 rows.
