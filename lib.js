
const letter_to_image_map = {
  "?": "sprites/blank.png",
  ".": "sprites/clicked.png",
  "B": "sprites/mine.png",
  "F": "sprites/flag.png",
  "0": "sprites/0.png",
  "1": "sprites/1.png",
  "2": "sprites/2.png",
  "3": "sprites/3.png",
  "4": "sprites/4.png",
  "5": "sprites/5.png",
  "6": "sprites/6.png",
  "7": "sprites/7.png",
  "8": "sprites/8.png",
};

document.addEventListener('contextmenu', event => event.preventDefault());

class Board {
  constructor(y, x) {
    this.canvas = document.getElementById('canvas');
    this.y = y;
    this.x = x;

    this.state = "Initial";
    this.bombs_left = 99;

    // this.x = x;
    this.cellSize = Math.min(this.canvas.width / x, this.canvas.height / y);
    this.boardPositions = [];
    for(var i = 0; i < y; i++){
      var boardPositionRow = [];
      for(var j = 0; j < x; j++){
        boardPositionRow.push([i * this.cellSize, j * this.cellSize]);
      }
      this.boardPositions.push(boardPositionRow);
    }

    this.boardState = [];
    this.boardImages = [];
    for(var i = 0; i < y; i++){
      var boardRow = [];
      var boardImageRow = [];
      for(var j = 0; j < x; j++){
        boardRow.push("?");

        var img = new Image();
        img.src = letter_to_image_map["?"];
        img.ctx = this.canvas.getContext('2d');
        img.dy = this.boardPositions[i][j][0];
        img.dx = this.boardPositions[i][j][1];
        img.dWidth = this.cellSize;
        img.dHeight = this.cellSize;
        img.onload = function(){
          this.ctx.drawImage(this, this.dx, this.dy, this.dWidth, this.dHeight);
        };

        boardImageRow.push(img);
      }
      this.boardState.push(boardRow);
      this.boardImages.push(boardImageRow);
    }

    this.mouseState = {
      "position": [-1, -1],
      "clicked": false,
      "button": null
    };
  }

  getCellCoordinates(mousePosition) {
    const mouseY = mousePosition[0];
    const mouseX = mousePosition[1];
    if(mouseY < 0 || mouseX < 0 || mouseY >= this.y * this.cellSize || mouseX >= this.x * this.cellSize){
      return [];
    }
    return [Math.floor(mouseY / this.cellSize), Math.floor(mouseX / this.cellSize)];
  }

  handleDown(e) {
    if(e.buttons == 1){
      this.mouseState["button"] = "left";
    } else if(e.buttons == 2){
      this.mouseState["button"] = "right";
    }
    this.mouseState["clicked"] = true;

    const oldMousePosition = this.mouseState["position"];
    const oldCellCoordinates = this.getCellCoordinates(oldMousePosition);

    if(oldCellCoordinates.length != 0 && this.boardState[oldCellCoordinates[0]][oldCellCoordinates[1]] == "."){
      this.boardState[oldCellCoordinates[0]][oldCellCoordinates[1]] = "?";
      this.updateSrc(oldCellCoordinates[0], oldCellCoordinates[1]);
    }

    this.mouseState["position"] = [e["layerY"], e["layerX"]];
    const cellCoordinates = this.getCellCoordinates(this.mouseState["position"]);

    if(cellCoordinates.length != 0 && this.boardState[cellCoordinates[0]][cellCoordinates[1]] == "?"){
      this.boardState[cellCoordinates[0]][cellCoordinates[1]] = ".";
      this.updateSrc(cellCoordinates[0], cellCoordinates[1]);
    }
  }

  handleUp(e) {
    this.mouseState["clicked"] = false;
    const oldMousePosition = this.mouseState["position"];
    const oldCellCoordinates = this.getCellCoordinates(oldMousePosition);

    if(oldCellCoordinates.length != 0 && this.boardState[oldCellCoordinates[0]][oldCellCoordinates[1]] == "."){
      this.boardState[oldCellCoordinates[0]][oldCellCoordinates[1]] = "?";
      this.updateSrc(oldCellCoordinates[0], oldCellCoordinates[1]);
    }

    this.mouseState["position"] = [e["layerY"], e["layerX"]];
    const newPosition = this.mouseState["position"];
    const newCellCoordinates = this.getCellCoordinates(newPosition);

    var response;
    var coordinatesY = -1;
    var coordinatesX = -1;
    var button = "N/A";
    if(newCellCoordinates.length == 2){
      coordinatesY = newCellCoordinates[0];
      coordinatesX = newCellCoordinates[1];
    }
    if(this.mouseState["button"] === "left" || this.mouseState["button"] === "right"){
      button = this.mouseState["button"];
    }

    response = {"coordinatesY": coordinatesY, "coordinatesX": coordinatesX, "button": button};
    this.mouseState["button"] = null;

    return response;
  }

  handleMove(e) {
    const oldPosition = this.mouseState["position"];
    this.mouseState["position"] = [e["layerY"], e["layerX"]];
    const newPosition = this.mouseState["position"];

    if(!this.mouseState["clicked"]){
      return;
    }

    const oldCellCoordinates = this.getCellCoordinates(oldPosition);
    const newCellCoordinates = this.getCellCoordinates(newPosition);
    if(oldCellCoordinates[0] == newCellCoordinates[0] && oldCellCoordinates[1] == newCellCoordinates[1]){
      return;
    }
    if(oldCellCoordinates.length != 0 && this.boardState[oldCellCoordinates[0]][oldCellCoordinates[1]] == "."){
      this.boardState[oldCellCoordinates[0]][oldCellCoordinates[1]] = "?";
      this.updateSrc(oldCellCoordinates[0], oldCellCoordinates[1]);
    }
    if(newCellCoordinates.length != 0 && this.boardState[newCellCoordinates[0]][newCellCoordinates[1]] == "?"){
      this.boardState[newCellCoordinates[0]][newCellCoordinates[1]] = ".";
      this.updateSrc(newCellCoordinates[0], newCellCoordinates[1]);
    }
  }

  updateSrc(i, j){
    this.boardImages[i][j].src = letter_to_image_map[this.boardState[i][j]];
  }

  setBoardState(newStateString) {
    newStateString = newStateString.replace(/(\r\n|\n|\r)/gm, "");
    const newState = JSON.parse(newStateString);
    const newBoard = newState["board"];
    this.state = newState["state"];
    if(newState["state"] == "Running"){
      this.bombs_left = newState["bombs"];
      console.log(this.bombs_left);
    }

    for(var i = 0; i < this.y; i++){
      for(var j = 0; j < this.x; j++){
        if(this.boardState[i][j] != newBoard[i * this.x + j]){
          this.boardState[i][j] = newBoard[i * this.x + j];
          this.updateSrc(i, j);
        }
      }
    }
  }
}
