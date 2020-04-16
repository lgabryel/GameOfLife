unit GOL_LclView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, LCL, Dialogs, GOL_Interfaces;

type

  { TBoardForm }

  TBoardForm = class(TForm)
  private

  public
    procedure DrawBoard(ABoard : TViewBoard);
  end;

  {TLclView }

  TLclView = class(TInterfacedObject, IGOL_View)
  private
    _board : TBoardForm;
  public
    procedure UpdateView(ABoard : TViewBoard);
    constructor Create(Ax : integer; Ay :integer);
  end;


implementation


{ TBoardForm }

procedure TBoardForm.DrawBoard(ABoard: TViewBoard);
var
  i : integer;
  j : integer;
  size : integer;
begin
  size := Length(ABoard);

  //kopiowanie z bufora planszy do bufora wydruku
  for i := 0 to size-1 do
  begin
    for j := 0 to size-1 do
    begin
      if ABoard[i,j] = '@' then
      begin
        self.Canvas.Pixels[i,j] := clBlack;
      end else begin
        self.Canvas.Pixels[i,j] := clNone;
      end;
    end;
  end;

end;

{ TLclView }


procedure TLclView.UpdateView(ABoard: TViewBoard);
begin
  _board.DrawBoard(ABoard);
end;

constructor TLclView.Create(Ax: integer; Ay: integer);
begin
  _board := TBoardForm.CreateNew(Application);
  _board.Width:=Ax;
  _board.Height:=Ay;
  _board.Show();
end;

end.

