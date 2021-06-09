##
# * Implement SQL joins for Ruby Array of Arrays.
# * I'm only confident that these algorithms are correct to the extent that the
#   tests inlined below pass. There are probably edge cases I'm not aware of.
# * All SQL tested on Postgres 11.3
# * All tables are represented as Array of Array
# * Aside: You can add arrays in ruby with `+`
# * I use the following jargon below:
#     * LHS = left hand side
#     * RHS = right hand side
#

##
# Generate an array of nils of the given length
#
# @param length [Integer]
# @return [Array<nil>]
#
def array_of_nils(length:)
  Array.new(length, nil)
end

##
# @param lhs_table [Array]
# @param rhs_table [Array]
# @return [Array]
#
def cross_join(lhs_table, rhs_table)
  output = []

  # Note: CROSS JOIN iterates across the RHS table first. This may be
  # surprising to you - it was to me :-)
  rhs_table.each do |rhs_row|
    lhs_table.each do |lhs_row|
      output << lhs_row + rhs_row
    end
  end

  output
end

##
# @param lhs_table [Array]
# @param rhs_table [Array]
#
# @yield [lhs_row, rhs_row] a block implementing the join condition by
#   comparing the two given rows in some way
#
# @yieldparam lhs_row [Array]
# @yieldparam rhs_row [Array]
# @yieldreturn [Boolean]
#
# @return [Array]
#
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

##
# @param lhs_table [Array]
# @param rhs_table [Array]
#
# @yield [lhs_row, rhs_row] a block implementing the join condition by
#   comparing the two given rows in some way
#
# @yieldparam lhs_row [Array]
# @yieldparam rhs_row [Array]
# @yieldreturn [Boolean]
#
# @return [Array]
#
def left_join(lhs_table, rhs_table, &block)
  output = []
  unmatched_lhs_rows = []

  # for each row in the LHS table ...
  lhs_table.each do |lhs_row|
    # ... find all rows in the RHS table which match this row (using the join
    # condition block to test)
    matching_rhs_rows = rhs_table.select do |rhs_row|
      block.call(lhs_row, rhs_row)
    end

    # if we didn't find any RHS rows which match the current LHS row then we
    # store the current LHS row in our "unmatched" list so we can append those
    # rows to the output **after** all the matched rows.
    if matching_rhs_rows.empty?
      unmatched_lhs_rows << lhs_row
    else
      # for each matching row in the RHS table, build a new row by
      # concatenating the LHS and RHS rows and add it to the output.
      matching_rhs_rows.each do |rhs_row|
        output << lhs_row + rhs_row
      end
    end
  end

  # Add the unmatched rows from the LHS table **after** we have added all the
  # matching rows.
  unmatched_lhs_rows.each do |lhs_row|
    output << lhs_row + array_of_nils(length: rhs_table.first.length)
  end

  output
end

##
# @param lhs_table [Array]
# @param rhs_table [Array]
#
# @yield [lhs_row, rhs_row] a block implementing the join condition by
#   comparing the two given rows in some way
#
# @yieldparam lhs_row [Array]
# @yieldparam rhs_row [Array]
# @yieldreturn [Boolean]
#
# @return [Array]
#
def right_join(lhs_table, rhs_table, &block)
  output = []
  unmatched_rhs_rows = []

  # for each row in the RHS table ...
  rhs_table.each do |rhs_row|
    # ... find all rows in the LHS table which match this row (using the join
    # condition block to test)
    matching_lhs_rows = lhs_table.select do |lhs_row|
      block.call(lhs_row, rhs_row)
    end

    # if we didn't find any LHS rows which match the current RHS row then we
    # store the current RHS row in our "unmatched" list so we can append those
    # rows to the output **after** all the matched rows.
    if matching_lhs_rows.empty?
      unmatched_rhs_rows << rhs_row
    else
      # for each matching row in the LHS table, build a new row by
      # concatenating the LHS and RHS rows and add it to the output.
      matching_lhs_rows.each do |lhs_row|
        output << lhs_row + rhs_row
      end
    end
  end

  # Add the unmatched rows from the RHS table **after** we have added all the
  # matching rows.
  unmatched_rhs_rows.each do |rhs_row|
    output << array_of_nils(length: lhs_table.first.length) + rhs_row
  end

  output
end

##
# @param lhs_table [Array]
# @param rhs_table [Array]
#
# @yield [lhs_row, rhs_row] a block implementing the join condition by
#   comparing the two given rows in some way
#
# @yieldparam lhs_row [Array]
# @yieldparam rhs_row [Array]
# @yieldreturn [Boolean]
#
# @return [Array]
#
def full_join(lhs_table, rhs_table, &block)
  # create a UNION of a LEFT JOIN and a RIGHT JOIN
  output = left_join(lhs_table, rhs_table, &block) + right_join(lhs_table, rhs_table, &block)

  # remove duplicates
  output.uniq
end

##
# Create the test data as both SQL and Ruby arrays

# SQL
#
#   create table users (id bigint primary key not null, name text, org_id bigint);
#   create table organisations (id bigint primary key not null, name text);
#   insert into users (id, name, org_id) values (1, 'joe', 1), (2, 'mary', 1), (3, 'brian', 3), (4, 'mike', null);
#   insert into organisations (id, name) values (1, 'Foo Inc'), (2, 'Bar Inc'), (3, 'Baz Inc');

# Ruby
#
users = [
  # id, name, org_id
  [1, "joe", 1],
  [2, "mary", 1],
  [3, "brian", 3],
  [4, "mike", nil]
]

organisations = [
  # id, name
  [1, "Foo Inc."],
  [2, "Bar Inc."],
  [3, "Baz Inc."]
]

# ########################################################################### #

puts "*" * 80
puts "users CROSS JOIN organisations"
puts ""
cross_join_results = cross_join(users, organisations)
pp cross_join_results
puts "Found #{cross_join_results.length} rows."

# postgres@eoin=#  SELECT * FROM users CROSS JOIN organisations;
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

# postgres@eoin=#  SELECT * FROM users AS u INNER JOIN organisations AS o ON u.org_id = o.id;
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

# postgres@eoin=#  SELECT * FROM users AS u LEFT JOIN organisations AS o ON u.org_id = o.id;
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
puts "*" * 80
puts "users RIGHT JOIN organisations"
puts ""

right_join_results = right_join(users, organisations) do |user, org|
  user[2] == org[0]
end

pp right_join_results
puts "Found #{right_join_results.length} rows."

# postgres@eoin=#  SELECT * FROM users AS u RIGHT JOIN organisations AS o ON u.org_id = o.id;
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
puts "*" * 80
puts "organisations LEFT JOIN users"
puts ""

left_join_results = left_join(organisations, users) do |org, user|
  user[2] == org[0]
end

pp left_join_results
puts "Found #{left_join_results.length} rows."

# postgres@eoin=#  SELECT * FROM organisations AS o LEFT JOIN users AS u ON u.org_id = o.id;
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
puts "*" * 80
puts "users FULL JOIN organisations"
puts ""

full_join_results = full_join(users, organisations) do |user, org|
  user[2] == org[0]
end

pp full_join_results
puts "Found #{full_join_results.length} rows."

# postgres@eoin=#  SELECT * FROM users AS u FULL JOIN organisations AS o ON u.org_id = o.id;
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
