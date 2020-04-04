program GameOfLife;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, GOL_Engine, GOL_Presenter, GOL_Interfaces
  { you can add units after this };

var
 _presenter : IGOL_Presenter;

begin
  _presenter := TGOL_Presenter.Create;
  _presenter.SynchronizeModel;
end.

