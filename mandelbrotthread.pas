unit mandelbrotthread;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, mandelbrot;

type

  TOnFinish = procedure of object;

  { TMBThread }

  TMBThread = class(TThread)
  private
    FOnFinish: TOnFinish;
    FMandelbrot: TMandelbrot;
    FTargetBitMap: TBitmap;
    procedure CallOnFinish;
  protected
    procedure Execute; override;
  public
    property OnFinish: TOnFinish read FOnFinish write FOnFinish;
    constructor Create(CreateSuspended: boolean; AMandelbrot: TMandelbrot; const ATargetBitmap: TBitmap);
    destructor Destroy; override;
  end;

implementation

{ TMBThread }

procedure TMBThread.CallOnFinish;
begin
  if Assigned(FOnFinish) then
  begin
    FTargetBitMap.Canvas.Draw(FMandelbrot.OffsetX, 0, FMandelbrot.GetBitmap);
    FOnFinish;
  end;
end;

procedure TMBThread.Execute;
begin
  try
    FMandelbrot.Calulate;
  finally
    Synchronize(@CallOnFinish);
  end;
end;

constructor TMBThread.Create(CreateSuspended: boolean; AMandelbrot: TMandelbrot; const ATargetBitmap: TBitmap);
begin
  inherited Create(CreateSuspended);
  FreeOnTerminate := True;
  FMandelbrot := AMandelbrot;
  FTargetBitmap := ATargetBitmap;
end;

destructor TMBThread.Destroy;
begin
  inherited Destroy;
end;

end.
