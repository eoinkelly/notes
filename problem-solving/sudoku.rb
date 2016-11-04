# frozen_string_literal: true

require "pry"

# rubocop:disable Style/SpaceInsideBrackets
unsolved_board = [
  [1,   nil, nil,     nil, 9,   nil,      nil, 5,   nil],
  [nil, nil, nil,     nil, nil, 4,        nil, nil, nil],
  [nil, nil, 9,       5,   nil, 6,        nil, nil, 1  ],

  [nil, 8,   nil,     2,   nil, nil,      7,   9,   nil],
  [7,   nil, 5,       nil, 4,   nil,      3,   nil, 2  ],
  [nil, 2,   3,       nil, nil, 8,        nil, 6,   nil],

  [6,   nil, nil,     4,   nil, 7,        9,   nil, nil],
  [nil, nil, nil,     3,   nil, nil,      nil, nil, nil],
  [nil, 4,   nil,     nil, 2,   nil,      nil, nil, 6  ]
]
# rubocop:enable Style/SpaceInsideBrackets

def print_spacer
  puts "-----------------------------------"
end

def print_board(board)
  print_spacer
  board.each_with_index do |row, idx|
    puts row.map { |cell| cell ? cell : " " }.join(" | ")
    print_spacer if ((idx + 1) % 3).zero?
  end
end

# QUESTION: is there an order to begin looking at empty cells or just do it sequentially?
#
# you could characterize the each cell based on triple of (num-filled-in-row,
# num-filled-in-col, num-filled-in-block) - that would indicate which cells
# would be more likely to have a single possible value
#
# for each empty cell
#     calculate array of possible values
#     if there is only one possible value then fill it in
#     shoudl we restart the search every time you fill a value in?

# return 3 x 3 array representing the block that contains x and y
def find_block_for(board, x, y)
  overshoot_x = x % 3
  block_top_x = if overshoot_x.zero?
                  x
                else
                  x - overshoot_x
                end

  overshoot_y = y % 3
  block_top_y = if overshoot_y.zero?
                  y
                else
                  y - overshoot_y
                end

  # return a 3x3 slice of the board starting at [block_top_x, block_top_y]
  board.slice(block_top_x, 3).map { |row| row.slice(block_top_y, 3) }
end

def calc_possible_values(row:, col:, block:)
  cannots = (row + col + block).flatten.uniq
  (1..9).to_a - cannots
end

def numbers_already_filled_in_row(board, row_idx)
  board[row_idx].compact.sort
end

def numbers_already_filled_in_column(board, col_idx)
  board.map { |row| row[col_idx] }.compact.sort
end

def numbers_already_filled_in_block(board, row_idx, col_idx)
  find_block_for(board, row_idx, col_idx).flatten.compact.sort
end

def fill_in_cells_with_only_one_possible_value(board) # rubocop:disable Metrics/MethodLength, Metrics/AbcSize
  mutation_operations = []

  board.each_with_index do |row, row_idx|
    row.each_with_index do |cell, col_idx|
      next unless cell.nil?

      filled_in_from_row = numbers_already_filled_in_row(board, row_idx)
      filled_in_from_col = numbers_already_filled_in_column(board, col_idx)
      filled_in_from_block = numbers_already_filled_in_block(board, row_idx, col_idx)
      possible_values = calc_possible_values(row: filled_in_from_row,
                                             col: filled_in_from_col,
                                             block: filled_in_from_block)

      if possible_values.length == 1
        mutation_operations << { row_idx: row_idx, col_idx: col_idx, value: possible_values.first }
      end
      puts <<~EOM
        Cell [#{row_idx}, #{col_idx}] possible_values = #{possible_values}
      EOM
      # puts <<~EOM
      #   Cell [#{row_idx}, #{col_idx}]:
      #     filled in from row   = #{filled_in_from_row}
      #     filled in from col   = #{filled_in_from_col}
      #     filled in from block = #{filled_in_from_block}
      #     possible_values      = #{possible_values}
      # EOM
    end
  end

  if mutation_operations.empty?
    puts "found no mutuations on this run"
    # return
    print_board board
    exit
  end

  # mutate the board
  mutation_operations.each do |op|
    puts "Filling in cell [#{op[:row_idx]}, #{op[:col_idx]}] with #{op[:value]}"
    board[op[:row_idx]][op[:col_idx]] = op[:value]
  end

  nil # be explicit about our mutation of the board argument
end

def board_is_unsolved?(board)
  board.any? { |row| row.any?(&:nil?) }
end

print_board unsolved_board

count = 0
while board_is_unsolved?(unsolved_board) && count < 1000
  # puts count
  print_board unsolved_board if (count % 100).zero?

  fill_in_cells_with_only_one_possible_value(unsolved_board)
  count += 1
end

print_board unsolved_board

# attempting to express the rule
# if 4 is NOT on the possibles-list for any other cell in the block then it must go in the current cell
# for each cell in the current block
#     for each value in the possibles-list for that cell
#         search the other possible lists in the block for the value
#           if it does not appear in any of them then fill it in
#
