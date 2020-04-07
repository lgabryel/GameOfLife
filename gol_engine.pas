unit GOL_Engine;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, crt, GOL_Interfaces, GOL_plainconsoleview;

type

  { TCell }

  TCell = class

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

   TCellsCollection = class

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

   TGOL_EnginWorker = class;

  { TGameOfLiveBoard }

  TGameOfLiveBoard = class(TInterfacedObject, IGOL_Engine)

    private
      _board : TCellsCollection;
      _presenter : IGOL_Presenter;
      _finished : boolean;
      _worker : TGOL_EnginWorker;
      procedure DrawBoard;
      procedure GetNextGeneration;
      procedure Play;

    public
      constructor Create(APresenter : IGOL_Presenter);
      procedure Load(pathToMapFile : String);
      procedure Start;
      procedure Stop;
      procedure UpdateConfig;
      procedure Init;
  end;

    { TGOL_EnginWorker }

    TGOL_EnginWorker = class(TThread)
    protected
      procedure Execute; override;

    private
      _board : TGameOfLiveBoard;

    public
      constructor Create(ABoard : TGameOfLiveBoard);
  end;

  { TGameOfLiveBoardLoader }

  TGameOfLiveBoardLoader = Class

    public
      function Load(pathToMapFile : String) : TCell2DArray;
      constructor Create;
  end;

implementation

{ TGOL_EnginWorker }

procedure TGOL_EnginWorker.Execute;
begin
  _board.Play;
end;

constructor TGOL_EnginWorker.Create(ABoard: TGameOfLiveBoard);
begin
  inherited Create(False);
  FreeOnTerminate := True;
  _board := ABoard;
end;

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

procedure TGameOfLiveBoard.DrawBoard;
var
  i : integer;
  j : integer;
  size : integer;
  bufor : TViewBoard;
begin
  i:=0;
  j:=0;
  size:=Length(_board.Items);
  SetLength(bufor, size, size);

  //Konwersja stanów na bufor
  for i := 0 to size-1 do
  begin
     for j := 0 to size-1 do
     begin
        if _board.Items[i][j].internalState then
        begin
          bufor[i][j] := '@';
        end else begin
          bufor[i][j] := ' ';
        end;
     end;
  end;

  _presenter.UpdateView(bufor);
end;


constructor TGameOfLiveBoard.Create( APresenter : IGOL_Presenter);
begin
  _presenter := APresenter;
end;

procedure TGameOfLiveBoard.Start;
begin
  //TODO czyszczenie tablicy
  _worker := TGOL_EnginWorker.Create(self);
  _finished := false;
end;

procedure TGameOfLiveBoard.Stop;
begin
 _finished := true;
end;

procedure TGameOfLiveBoard.UpdateConfig;
begin

end;

procedure TGameOfLiveBoard.Init;
begin
  Load('test.txt');
end;

procedure TGameOfLiveBoard.Play;
begin
  repeat
    GetNextGeneration;
    delay(100);
  until _finished;
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

  //DumpBoardToConsole;
  DrawBoard();

  _board.SetNeightbours;

end;

procedure TGameOfLiveBoard.GetNextGeneration;
var
  i : integer;
  j : integer;
  size : integer;
  newBoard : TCellsCollection;
begin
  size := Length(_board.Items);
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

  DrawBoard();
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

