program GameOfLife;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Classes,
  crt,
  SysUtils,
  Forms, Interfaces,
  GOL_Engine,
  GOL_Presenter,
  GOL_Interfaces;

var
  _presenter: IGOL_Presenter;


begin
  Application.Initialize;

  _presenter := TGOL_Presenter.Create;
  _presenter.SynchronizeModel;

  Application.Run;

  {
  //Pętla programu, w osobnym wątku model dziala i wola presenter.
  //TODO obsluzyc jakos watek wejscia
  repeat
    sleep(100);
  until False;  }
end.
