// Wall is 2
// Empty is 0
let gameBoard = [];
let gravityHook = [];
let gravityCollisionHook = [];
let lineClearHook = [];
let gameWhere = 0;
let gameHeight = 0;

for (let i = 0; i < 24; i++) {
  let row = [];
  for (let j = 0; j < 10; j++) {
    row.push(0);
  }
  gameBoard.push(row);
}

function gameIsOver() {
  for (let i = 0; i < 3; i++) {
    for (let j = 0; j < 10; j++) {
      if (gameBoard[i][j] == 2) {
        return true;
      }
    }
  }
  return false;
}

function collisionDetect(i, j) {
  if (i >= 24) {
    return true;
  }
  if (i < 0) {
    return true;
  }
  if (j < 0) {
    return true;
  }
  if (j >= 10) {
    return true;
  }
  if (gameBoard[i][j] == 2) {
    return true;
  }
  return false;
}

function checkCollisionLeft() {
  for (let i = 0; i < 24; i++) {
    for (let j = 0; j < 10; j++) {
      if (gameBoard[i][j] === 1) {
        if (collisionDetect(i, j - 1)) {
          return true;
        }
      }
    }
  }
  return false;
}

function checkCollisionRight() {
  for (let i = 0; i < 24; i++) {
    for (let j = 0; j < 10; j++) {
      if (gameBoard[i][j] === 1) {
        if (collisionDetect(i, j + 1)) {
          return true;
        }
      }
    }
  }
  return false;
}

function movePieceLeft() {
  if (!checkCollisionLeft()) {
    for (let j = 0; j < 9; j++) {
      for (let i = 0; i < 24; i++) {
        if (gameBoard[i][j + 1] === 1) {
          gameBoard[i][j + 1] = 0;
          gameBoard[i][j] = 1;
        }
      }
    }
    gameWhere -= 1;
    return true;
  }
  return false;
}

function movePieceRight() {
  if (!checkCollisionRight()) {
    for (let j = 9; j >= 1; j--) {
      for (let i = 0; i < 24; i++) {
        if (gameBoard[i][j - 1] === 1) {
          gameBoard[i][j - 1] = 0;
          gameBoard[i][j] = 1;
        }
      }
    }
    gameWhere += 1;
    return true;
  }
  return false;
}

function noActivePiece() {
  for (let i = 0; i < gameBoard.length; i++) {
    for (let j = 0; j < gameBoard[i].length; j++) {
      if (gameBoard[i][j] === 1) {
        return false;
      }
    }
  }
  return true;
}

function lineClearable(i) {
  for (let j = 0; j < gameBoard[i].length; j++) {
    if (gameBoard[i][j] !== 2) {
      return false;
    }
  }
  return true;
}

function anyLineClearable() {
  for (let i = 0; i < gameBoard.length; i++) {
    if (lineClearable(i)) {
      return true;
    }
  }
  return false;
}

function clearLines() {
  // First, count the number of clearable lines:
  while (anyLineClearable()) {
    out:
    for (let i = gameBoard.length - 1; i >= 0; i--) {
      if (lineClearable(i)) {
        for (let j = 0; j < gameBoard[i].length; j++) {
          gameBoard[i][j] = 0;
        }
        for (let k = i; k >= 0; k--) {
          for (let j = 0; j < gameBoard[i].length; j++) {
            if (k - 1 < 0) {
              gameBoard[k][j] = 0;
            } else {
              gameBoard[k][j] = gameBoard[k - 1][j];
            }
          }
        }
        for (const fn of lineClearHook) {
          fn(i);
        }
        break out;
      }
    }
  }
}

function gravity() {
  let exit = false;
  for (let i = 0; i < 24; i++) {
    for (let j = 0; j < 10; j++) {
      if (gameBoard[i][j] === 1) {
        if (collisionDetect(i + 1, j)) {
          exit = true;
        }
      }
    }
  }
  if (exit) {
    for (const hook of gravityCollisionHook) {
      hook();
    }
    for (let i = 0; i < 24; i++) {
      for (let j = 0; j < 10; j++) {
        if (gameBoard[i][j] == 1) {
          gameBoard[i][j] = 2;
        }
      }
    }
    return true;
  }

  let runhook = false;
  for (let i = 23; i >= 1; i--) {
    for (let j = 0; j < 10; j++) {
      if (gameBoard[i - 1][j] === 1) {
        gameBoard[i - 1][j] = 0;
        gameBoard[i][j] = 1;
        runhook = true;
      }
    }
  }
  if (runhook) {
    for (const hook of gravityHook) {
      hook();
    }
  }
  gameHeight += 1;
  return false;
}
