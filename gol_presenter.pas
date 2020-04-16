unit GOL_Presenter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GOL_Interfaces, GOL_plainconsoleview, GOL_LclView,
  GOL_Engine;

type

  { TGOL_Presenter }

  TGOL_Presenter = class(TInterfacedObject, IGOL_Presenter)
  private
    _view : IGOL_View;
    _model : IGOL_Engine;
  public
    procedure UpdateView( ABoard : TViewBoard);
    procedure SynchronizeModel;
    constructor Create;
  end;

implementation

{ TGOL_Presenter }

procedure TGOL_Presenter.UpdateView(ABoard: TViewBoard);
begin
  _view.UpdateView(ABoard);
end;

procedure TGOL_Presenter.SynchronizeModel;
begin
  _model.Init;
  _model.Start;
end;

constructor TGOL_Presenter.Create;
begin
  //_view := TPlainConsoleView.Create;
  _view := TLclView.Create(50,50);
  _model := TGameOfLiveBoard.Create(self);
end;

end.

