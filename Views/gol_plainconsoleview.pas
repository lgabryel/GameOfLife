unit GOL_plainconsoleview;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, crt, Windows, GOL_Interfaces;

type

  { TPlainConsoleView }

  TPlainConsoleView = class(TInterfacedObject, IGOL_View)
  public
    procedure UpdateView( ABoard : TViewBoard);
    constructor Create;
  end;

implementation



 { TPlainConsoleView }

procedure TPlainConsoleView.UpdateView(ABoard: TViewBoard);
var
  i : integer;
  x : integer;
  size : integer;
  endOffset : integer;
  line : array of char;
  LNumberOfCharsToWritten: longword;
begin
  i := 0;
  size := Length(ABoard);
  endOffset := Length(LineEnding);

  ClrScr;

  //kopiowanie z bufora planszy do bufora wydruku
  for i := 0 to size-1 do
  begin
    //Move(ABoard[i][0], line, size);
    line := Copy(ABoard[i], 0, MaxInt);

    //Alokowanie bufora na linię tekstu
    Setlength(line,size+endOffset);

    //Wpisanie do buffora na stale konca linii
    for x := 0 to endOffset do
    begin
      line[size+x] := LineEnding[x+1];
    end;

    //Szybkie wypisanie linii
    WriteConsole(GetStdHandle(STD_OUTPUT_HANDLE), PChar(line), size+endOffset,
      LNumberOfCharsToWritten, nil);
  end;
end;

constructor TPlainConsoleView.Create;
begin
  cursoroff;
end;


end.

