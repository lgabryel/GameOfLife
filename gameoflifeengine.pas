unit GameOfLifeEngine;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, crt, Windows;

type

  { TCell }

  TCell = Class

  private
    neightbours : TList;
    internalState: boolean;
    x : integer;
    y : integer;
    function CountLiveNeightbours : integer;

  public
    constructor Create(startState: boolean; Ax :integer; Ay : integer);
    procedure AddNeightbour(neightbour : TCell);
    function CalculateState : boolean;
    Property State :  boolean read internalState write internalState;
  end;

   TCell2DArray = array of array of TCell;

   { TCellsCollection }

   TCellsCollection = Class

     private
       {fields}
       _cells : TCell2DArray;
       {procedures}
       procedure SetNeightbours;

     public
       {constructors}
       constructor Create(dimensionX : integer; dimensionY : integer);
       constructor Create(existingCollection : TCellsCollection);
       {procedures}
       procedure ResizeTo(dimensionX : integer; dimensionY : integer);
       {functions}
       function Clone : TCellsCollection;
       {properties}
       property Items : TCell2DArray read _cells;

   end;

  { TGameOfLiveBoard }

  TGameOfLiveBoard = Class

    private
      _board : TCellsCollection;
      procedure DumpBoardToConsole;

    public
      constructor Create;
      procedure Load(pathToMapFile : String);
      function GetNextGeneration :  TCell2DArray;

  end;

  { TGameOfLiveBoardLoader }

  TGameOfLiveBoardLoader = Class

    public
      function Load(pathToMapFile : String) : TCell2DArray;
      constructor Create;
  end;

implementation

{ TCellsCollection }

procedure TCellsCollection.SetNeightbours;
var
  i, j, size, nj, ni : integer;
begin
  size := Length(_cells);

  for i := 0 to size-1 do
  begin
    for j := 0 to size-1 do
    begin
       nj := j;
       ni := i-1;

       if ( (nj > 0) and (nj < size)) and  ( (ni > 0) and (ni < size)) then
       begin
         _cells[i][j].AddNeightbour(_cells[ni][nj]);
       end;

       nj := j;
       ni := i+1;

       if ( (nj > 0) and (nj < size)) and  ( (ni > 0) and (ni < size)) then
       begin
         _cells[i][j].AddNeightbour(_cells[ni][nj]);
       end;

       nj := j+1;
       ni := i;

       if ( (nj > 0) and (nj < size)) and  ( (ni > 0) and (ni < size)) then
       begin
         _cells[i][j].AddNeightbour(_cells[ni][nj]);
       end;

       nj := j+1;
       ni := i-1;

       if ( (nj > 0) and (nj < size)) and  ( (ni > 0) and (ni < size)) then
       begin
         _cells[i][j].AddNeightbour(_cells[ni][nj]);
       end;

       nj := j+1;
       ni := i+1;

       if ( (nj > 0) and (nj < size)) and  ( (ni > 0) and (ni < size)) then
       begin
         _cells[i][j].AddNeightbour(_cells[ni][nj]);
       end;

       nj := j-1;
       ni := i;

       if ( (nj > 0) and (nj < size)) and  ( (ni > 0) and (ni < size)) then
       begin
         _cells[i][j].AddNeightbour(_cells[ni][nj]);
       end;

       nj := j-1;
       ni := i-1;

       if ( (nj > 0) and (nj < size)) and  ( (ni > 0) and (ni < size)) then
       begin
         _cells[i][j].AddNeightbour(_cells[ni][nj]);
       end;

       nj := j-1;
       ni := i+1;

       if ( (nj > 0) and (nj < size)) and  ( (ni > 0) and (ni < size)) then
       begin
         _cells[i][j].AddNeightbour(_cells[ni][nj]);
       end;
    end;
  end;
end;

constructor TCellsCollection.Create(dimensionX: integer; dimensionY: integer);
begin
  ResizeTo(dimensionX, dimensionY);
end;

constructor TCellsCollection.Create(existingCollection: TCellsCollection);
begin
  _cells :=  copy(existingCollection.Items, 0, Length(existingCollection.Items));
end;

procedure TCellsCollection.ResizeTo(dimensionX: integer; dimensionY: integer);
begin
  SetLength(_cells, dimensionX, dimensionY);
end;

function TCellsCollection.Clone: TCellsCollection;
var
  newCells : TCellsCollection;
  newCell: TCell;
  i : integer;
  j : integer;
  size : integer;
begin
  size := Length(self.Items);
  newCells := TCellsCollection.Create(size,size);

  for i := 0 to size-1 do
  begin
     for j := 0 to size-1 do
     begin
        newCells.Items[i][j] := TCell.Create(self.Items[i][j].InternalState, i,j);
     end;
  end;

  newCells.SetNeightbours;
  Clone := newCells;
end;

{ TGameOfLiveBoard }

procedure TGameOfLiveBoard.DumpBoardToConsole;
var
  i : integer;
  j : integer;
  size : integer;
  line : String;
  LNumberOfCharsToWritten: longword;
begin
  size := Length(_board.Items);
  i := 0;
  j := 0;
  Setlength(line,size+2);
  line[size+1] :=  #10;
  line[size+2]  :=  #13;

  for i := 0 to size-1 do
  begin
     for j := 0 to size-1 do
     begin
        if _board.Items[i][j].internalState then
        begin
          line[j+1] := '@';
        end else begin
          line[j+1] := ' ';
        end;
     end;
     WriteConsole(GetStdHandle(STD_OUTPUT_HANDLE), PChar(line), size+2, LNumberOfCharsToWritten, nil);
   //  Writeln;
  end;
end;

constructor TGameOfLiveBoard.Create;
begin

end;

procedure TGameOfLiveBoard.Load(pathToMapFile: String);
var
  txtf : TextFile;
  i : integer;
  j : integer;
  ni: integer;
  nj: integer;
  s : string;
  size : integer;
  c : char;
begin
  //Otwieramy mapę
  AssignFile(txtf, pathToMapFile);
  Reset(txtf);

  //Liczymy rozmiar mapy
  i := 0;
  ReadLn(txtf, s);
  while not EOF(txtf) do
  begin
    i := i + 1;
    j := Length(s);
    ReadLn(txtf, s);
  end;

  //Alokujemy mapę
  //SetLength(_board, i+1, j);
  _board := TCellsCollection.Create(i+1, j);

  size := i + 1;

  //Wracamy na początek pliku i ladujemy do mapy stany komórek
  Reset(txtf);
  i := 0;
  repeat
  begin
    ReadLn(txtf, s);
    j := 0;
    for c in s do
    begin
    _board.Items[i][j] := TCell.Create(c = '1', i, j);
      j := j + 1;
    end;
    i := i + 1;
  end until  EOF(txtf);

  DumpBoardToConsole;

  _board.SetNeightbours;

  {//Musimy przypisać jeszcze referencje na sąsiadów
  i := 0;
  j := 0;

  for i := 0 to size-1 do
  begin
    for j := 0 to size-1 do
    begin
       nj := j;
       ni := i-1;

       if ( (nj > 0) and (nj < size)) and  ( (ni > 0) and (ni < size)) then
       begin
         _board[i][j].AddNeightbour(_board[ni][nj]);
       end;

       nj := j;
       ni := i+1;

       if ( (nj > 0) and (nj < size)) and  ( (ni > 0) and (ni < size)) then
       begin
         _board[i][j].AddNeightbour(_board[ni][nj]);
       end;

       nj := j+1;
       ni := i;

       if ( (nj > 0) and (nj < size)) and  ( (ni > 0) and (ni < size)) then
       begin
         _board[i][j].AddNeightbour(_board[ni][nj]);
       end;

       nj := j+1;
       ni := i-1;

       if ( (nj > 0) and (nj < size)) and  ( (ni > 0) and (ni < size)) then
       begin
         _board[i][j].AddNeightbour(_board[ni][nj]);
       end;

       nj := j+1;
       ni := i+1;

       if ( (nj > 0) and (nj < size)) and  ( (ni > 0) and (ni < size)) then
       begin
         _board[i][j].AddNeightbour(_board[ni][nj]);
       end;

       nj := j-1;
       ni := i;

       if ( (nj > 0) and (nj < size)) and  ( (ni > 0) and (ni < size)) then
       begin
         _board[i][j].AddNeightbour(_board[ni][nj]);
       end;

       nj := j-1;
       ni := i-1;

       if ( (nj > 0) and (nj < size)) and  ( (ni > 0) and (ni < size)) then
       begin
         _board[i][j].AddNeightbour(_board[ni][nj]);
       end;

       nj := j-1;
       ni := i+1;

       if ( (nj > 0) and (nj < size)) and  ( (ni > 0) and (ni < size)) then
       begin
         _board[i][j].AddNeightbour(_board[ni][nj]);
       end;
    end;
  end;
  }

end;

function TGameOfLiveBoard.GetNextGeneration: TCell2DArray;
var
  i : integer;
  j : integer;
  size : integer;
  newBoard : TCellsCollection;
begin
  size := Length(_board.Items);
  //newBoard := copy(_board, 0, length(_board)) ;
  newBoard := _board.Clone;

  for i := 0 to size-1 do
  begin
    for j := 0 to size-1 do
    begin
      newBoard.Items[i][j].State := _board.Items[i][j].CalculateState;
    end;
  end;

  _board := nil;
  _board := newBoard;

  ClrScr;

  DumpBoardToConsole;

end;

{ TGameOfLiveBoardLoader }

function TGameOfLiveBoardLoader.Load(pathToMapFile: String): TCell2DArray;
begin

end;

constructor TGameOfLiveBoardLoader.Create;
begin

end;

{ TCell }

constructor TCell.Create(startState: boolean; Ax: integer; Ay: integer);
begin
  neightbours := TList.Create;
  internalState := startState;
  x := Ax;
  y := Ay;
end;

procedure TCell.AddNeightbour(neightbour: TCell);
begin
  neightbours.Add(neightbour);
end;

function TCell.CalculateState : boolean;
var
  liveNeightbours : integer;
begin
  liveNeightbours := CountLiveNeightbours;
  if internalState then //jesli żyje
  begin
     if (liveNeightbours < 2) or (liveNeightbours > 3) then
     begin
        CalculateState := False;
     end else begin
        CalculateState := True;
     end;
  end else begin  // jesli martwy
     if liveNeightbours = 3 then
     begin
       CalculateState := True;
     end else begin
       CalculateState := False;
     end;
  end;
end;

function TCell.CountLiveNeightbours: integer;
var
  liveNeightboursCoun : integer;
  neightbour : ^TCell;
begin
  liveNeightboursCoun := 0;
  for neightbour in  neightbours do
  begin
    if TCell(neightbour).State then
    begin
      Inc(liveNeightboursCoun);
    end;
  end;
  CountLiveNeightbours := liveNeightboursCoun;
end;

end.

