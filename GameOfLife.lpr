program GameOfLife;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, GameOfLifeEngine, crt
  { you can add units after this };

var
  game : TGameOfLiveBoard;

begin
  game := TGameOfLiveBoard.Create;
  game.Load('test.txt');
  cursoroff;

  repeat
    game.GetNextGeneration;
    delay(100);
  until false;
end.

