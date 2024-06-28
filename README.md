# Connect Four Game Implementation using Fortran

This repository contains a Connect Four game implementation in Fortran, featuring customizable settings, advanced computer intelligence, and strategic gameplay mechanics.

## Features

1. **Customizable Board Size**:
   - Players can adjust the dimensions of the game board.
   - The `change_setting` subroutine allows users to specify the number of rows and columns.
   
2. **Flexible Computer Intelligence Level**:
   - Users can set the computer opponent's intelligence level using the `computerIQ` parameter.
   - The game offers various intelligence levels for different challenges.

3. **Advanced Computer Algorithm**:
   - The computer uses sophisticated algorithms, including backtracking and tree algorithms, to analyze potential moves.
   - Recursive functions are utilized to explore multiple future move possibilities.

4. **Strategic Point System**:
   - A point-based scoring system is integrated into the computer player's decision-making process.
   - Points are assigned based on factors such as column potential and remaining moves.

5. **Challenging Gameplay**:
   - Higher `computerIQ` settings make the computer a formidable opponent.
   - Engaging and competitive gameplay for players of all skill levels.

## Installation

1. Clone the repository:

    ```sh
    git clone https://github.com/your-username/connect-four-fortran.git
    cd connect-four-fortran
    ```

2. Compile the Fortran code using your preferred Fortran compiler. For example, with `gfortran`:

    ```sh
    gfortran -o connect_four connect_four.f90
    ```

## Usage

1. Run the compiled program:

    ```sh
    ./connect_four
    ```

2. Follow the on-screen instructions to configure the game settings:
    - Enter the computer IQ level.
    - Choose between default dimensions or customize the board size.

3. The game begins with player turns. Input your moves as prompted.

4. The game continues until a win condition is met or the game is terminated.

## Code Explanation

### Module: `subprograms`

This module contains all subroutines and functions necessary for the Connect Four game.

- **Variables**:
  - `board`: 2D array representing the game board.
  - `columns`, `rows`: Dimensions of the game board.
  - `current_column`, `current_row`: Track the current position of the player's move.
  - `set_to_make`: Number of tokens needed to make a set.
  - `input`: User input.
  - `total_turn`: Track the total number of turns in the game.
  - `computerIQ`: Intelligence level of the computer player.
  - `tokken`: Character representing the token used by players.
  - `game_status`: Indicates whether the game is ongoing.
  - `player`: Represents the first player.

- **Subroutines and Functions**:
  1. `change_setting`: Allows users to set game parameters like computer IQ level and board dimensions.
  2. `initialize`: Initializes the game by allocating memory for the game board and setting the game status to true.
  3. `display`: Displays the current state of the game board.
  4. `inputFromUser`: Handles user input for placing tokens and implements logic for both single-player and two-player modes.
  5. `computer`: Handles computer moves based on the selected intelligence.
  6. `bestMove`: Recursively checks for the best move up to `computerIQ` moves ahead.
  7. `winning`: Checks for win conditions.

### Main Program: `show`

- Initiates the game by calling `change_setting`, `initialize`, and `display`.
- Implements a loop for game turns where players take turns placing tokens until a win condition is met or the game is terminated.
- Checks for win conditions and ends the game if necessary.


## Contributing

Contributions are welcome! Please fork the repository and create a pull request with your changes.

## Contact

For any questions or issues, please open an issue on the GitHub repository.

