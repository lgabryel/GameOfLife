unit GOL_Interfaces;

{$mode objfpc}{$H+}

interface


type

  TViewBoard = array of array of char;

  IGOL_View = interface
    procedure UpdateView( ABoard : TViewBoard);
  end;

  IGOL_Presenter = interface
    procedure UpdateView( ABoard : TViewBoard);
    procedure SynchronizeModel;
  end;

  IGOL_Engine = interface
    procedure Start;
    procedure Stop;
    procedure UpdateConfig;
    procedure Init;
  end;

implementation

end.

