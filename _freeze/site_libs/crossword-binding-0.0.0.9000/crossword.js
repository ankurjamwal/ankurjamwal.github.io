HTMLWidgets.widget({

  name: "crossword",
  type: "output",

  factory: function(el, width, height) {

    injectStyleOnce();

    return {
      renderValue: function(x) {
        el.innerHTML = "";
        buildCrossword(el, x);
      },

      resize: function(width, height) {
        // grid is fixed-size in cells; nothing to do on container resize
      }
    };
  }
});

function injectStyleOnce() {
  if (document.getElementById("crosswordwidget-css")) return;
  var style = document.createElement("style");
  style.id = "crosswordwidget-css";
  style.textContent = [
    ".cwv-demo{display:flex;flex-wrap:wrap;gap:2rem;margin:0.5rem 0;font-family:inherit}",
    ".cwv-board{flex:0 0 auto}",
    ".cwv-grid{display:grid;gap:2px;background:#999;border:2px solid #444;width:fit-content}",
    ".cwv-cell{position:relative;background:#fff}",
    ".cwv-cell.block{background:#333}",
    ".cwv-cell input{width:100%;height:100%;border:none;text-align:center;",
    "font-size:1.1rem;font-weight:600;text-transform:uppercase;background:transparent;color:#111;padding:0}",
    ".cwv-cell input:focus{outline:2px solid #2b6cb0;background:#eaf2fb}",
    ".cwv-cell.correct input{background:#c6f6d5}",
    ".cwv-cell.incorrect input{background:#fed7d7}",
    ".cwv-num{position:absolute;top:1px;left:2px;font-size:0.55rem;color:#555;pointer-events:none}",
    ".cwv-side{flex:1 1 240px;min-width:220px}",
    ".cwv-controls{margin-bottom:1rem;display:flex;align-items:center;gap:0.5rem;flex-wrap:wrap}",
    ".cwv-controls button{padding:0.35rem 0.8rem;border-radius:6px;border:1px solid #888;background:#f5f5f5;cursor:pointer}",
    ".cwv-controls button:hover{background:#e8e8e8}",
    ".cwv-status{font-size:0.85rem;font-weight:600}",
    ".cwv-clues{display:flex;gap:1.5rem;flex-wrap:wrap}",
    ".cwv-clues h4{margin:0 0 0.3rem 0}",
    ".cwv-clues ul{list-style:none;padding-left:0;margin:0}",
    ".cwv-clues li{padding:0.12rem 0;cursor:pointer;font-size:0.88rem}",
    ".cwv-clues li:hover{text-decoration:underline}",
    ".cwv-warning{color:#b7791f;font-size:0.85rem;margin-bottom:0.5rem}"
  ].join("");
  document.head.appendChild(style);
}

function buildCrossword(el, x) {
  var cellSize = x.cellSize || 32;
  var words = x.words;

  var root = document.createElement("div");
  root.className = "cwv-demo";

  var unconnected = words.filter(function(w) { return !w.connected; });
  if (unconnected.length > 0) {
    var warn = document.createElement("div");
    warn.className = "cwv-warning";
    warn.textContent = "Note: " + unconnected.map(function(w) { return w.answer; }).join(", ") +
      " could not be connected to the rest of the grid (see the R warning) and " +
      (unconnected.length === 1 ? "is" : "are") + " shown on its own row.";
    root.appendChild(warn);
  }

  var board = document.createElement("div");
  board.className = "cwv-board";
  var grid = document.createElement("div");
  grid.className = "cwv-grid";
  grid.style.gridTemplateColumns = "repeat(" + x.ncol + "," + cellSize + "px)";
  grid.style.gridTemplateRows = "repeat(" + x.nrow + "," + cellSize + "px)";
  board.appendChild(grid);

  var side = document.createElement("div");
  side.className = "cwv-side";
  side.innerHTML =
    '<div class="cwv-controls">' +
      '<button data-action="check">Check</button>' +
      '<button data-action="reveal">Reveal</button>' +
      '<button data-action="clear">Clear</button>' +
      '<span class="cwv-status"></span>' +
    '</div>' +
    '<div class="cwv-clues">' +
      '<div><h4>Across</h4><ul class="cwv-across"></ul></div>' +
      '<div><h4>Down</h4><ul class="cwv-down"></ul></div>' +
    '</div>';

  root.appendChild(board);
  root.appendChild(side);
  el.appendChild(root);

  // ---- derive letter grid / numbering / direction membership from x.words ----
  var key = function(r, c) { return r + "_" + c; };
  var letterAt = {}, numberAt = {}, cellDir = {};

  words.forEach(function(w) {
    numberAt[key(w.row, w.col)] = w.number;
    var letters = w.answer.split("");
    for (var i = 0; i < letters.length; i++) {
      var r = w.direction === "across" ? w.row : w.row + i;
      var c = w.direction === "across" ? w.col + i : w.col;
      var k = key(r, c);
      letterAt[k] = letters[i];
      if (!cellDir[k]) cellDir[k] = {};
      cellDir[k][w.direction] = w;
    }
  });

  var inputs = {};
  for (var r = 1; r <= x.nrow; r++) {
    for (var c = 1; c <= x.ncol; c++) {
      var k = key(r, c);
      var cell = document.createElement("div");
      cell.className = "cwv-cell";
      if (!letterAt[k]) {
        cell.classList.add("block");
        grid.appendChild(cell);
        continue;
      }
      if (numberAt[k]) {
        var num = document.createElement("span");
        num.className = "cwv-num";
        num.textContent = numberAt[k];
        cell.appendChild(num);
      }
      var input = document.createElement("input");
      input.maxLength = 1;
      input.autocomplete = "off";
      input.dataset.row = r;
      input.dataset.col = c;
      cell.appendChild(input);
      grid.appendChild(cell);
      inputs[k] = { input: input, cell: cell };
    }
  }

  var currentDir = "across";
  var lastClickedKey = null;
  var statusEl = side.querySelector(".cwv-status");

  function focusCell(r, c, dir) {
    var k = key(r, c);
    if (!inputs[k]) return;
    if (dir) currentDir = dir;
    lastClickedKey = null;
    inputs[k].input.focus();
  }
  function clearStatus() { statusEl.textContent = ""; }

  Object.keys(inputs).forEach(function(k) {
    var input = inputs[k].input, cell = inputs[k].cell;

    // select existing letter on focus, so typing over an already-filled
    // intersection cell replaces it instead of being blocked by maxlength=1
    input.addEventListener("focus", function() { input.select(); });

    input.addEventListener("click", function() {
      var dirs = cellDir[k] || {};
      var has = { across: !!dirs.across, down: !!dirs.down };
      if (k === lastClickedKey && has.across && has.down) {
        currentDir = currentDir === "across" ? "down" : "across";
      } else if (!has[currentDir]) {
        currentDir = has.across ? "across" : "down";
      }
      lastClickedKey = k;
    });

    input.addEventListener("input", function() {
      input.value = input.value.toUpperCase().slice(0, 1);
      var r = +input.dataset.row, c = +input.dataset.col;
      if (input.value) {
        if (currentDir === "across" && inputs[key(r, c + 1)]) focusCell(r, c + 1);
        else if (currentDir === "down" && inputs[key(r + 1, c)]) focusCell(r + 1, c);
        else if (inputs[key(r, c + 1)]) focusCell(r, c + 1);
        else if (inputs[key(r + 1, c)]) focusCell(r + 1, c);
      }
      clearStatus();
      cell.classList.remove("correct", "incorrect");
    });

    input.addEventListener("keydown", function(e) {
      var r = +input.dataset.row, c = +input.dataset.col;
      if (e.key === "ArrowRight") focusCell(r, c + 1, "across");
      else if (e.key === "ArrowLeft") focusCell(r, c - 1, "across");
      else if (e.key === "ArrowDown") focusCell(r + 1, c, "down");
      else if (e.key === "ArrowUp") focusCell(r - 1, c, "down");
      else if (e.key === "Backspace" && !input.value) {
        if (currentDir === "across" && inputs[key(r, c - 1)]) focusCell(r, c - 1);
        else if (currentDir === "down" && inputs[key(r - 1, c)]) focusCell(r - 1, c);
        else if (inputs[key(r, c - 1)]) focusCell(r, c - 1);
        else if (inputs[key(r - 1, c)]) focusCell(r - 1, c);
      }
    });
  });

  var acrossEl = side.querySelector(".cwv-across");
  var downEl = side.querySelector(".cwv-down");
  words.forEach(function(w) {
    var li = document.createElement("li");
    li.textContent = w.number + ". " + w.clue;
    li.addEventListener("click", function() { focusCell(w.row, w.col, w.direction); });
    (w.direction === "across" ? acrossEl : downEl).appendChild(li);
  });

  side.querySelector('[data-action="check"]').addEventListener("click", function() {
    var correct = 0;
    words.forEach(function(w) {
      var letters = w.answer.split("");
      var filled = "";
      var cells = [];
      for (var i = 0; i < letters.length; i++) {
        var r = w.direction === "across" ? w.row : w.row + i;
        var c = w.direction === "across" ? w.col + i : w.col;
        var cellInfo = inputs[key(r, c)];
        filled += cellInfo.input.value;
        cells.push(cellInfo);
      }
      var isCorrect = filled === w.answer;
      if (isCorrect) correct++;
      cells.forEach(function(cellInfo) {
        cellInfo.cell.classList.remove("correct", "incorrect");
        if (cellInfo.input.value) {
          cellInfo.cell.classList.add(isCorrect ? "correct" : "incorrect");
        }
      });
    });
    statusEl.textContent = correct + " / " + words.length + " words correct";
  });

  side.querySelector('[data-action="reveal"]').addEventListener("click", function() {
    Object.keys(inputs).forEach(function(k) {
      inputs[k].input.value = letterAt[k];
      inputs[k].cell.classList.remove("incorrect");
      inputs[k].cell.classList.add("correct");
    });
    statusEl.textContent = "Revealed";
  });

  side.querySelector('[data-action="clear"]').addEventListener("click", function() {
    Object.keys(inputs).forEach(function(k) {
      inputs[k].input.value = "";
      inputs[k].cell.classList.remove("correct", "incorrect");
    });
    clearStatus();
  });
}
