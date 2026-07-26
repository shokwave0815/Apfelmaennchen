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
    FBitmap: TBitmap;
    procedure CallOnFinish;
  protected
    procedure Execute; override;
  public
    property OnFinish: TOnFinish read FOnFinish write FOnFinish;
    constructor Create(CreateSuspended: boolean; const AMandelbrot: TMandelbrot; const ABitmap: TBitmap);
    destructor Destroy; override;
  end;

implementation

{ TMBThread }

procedure TMBThread.CallOnFinish;
begin
  FBitmap.Canvas.Draw(FMandelbrot.OffsetX, 0, FMandelbrot.GetBitmap);
  if Assigned(FOnFinish) then
  begin
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

constructor TMBThread.Create(CreateSuspended: boolean; const AMandelbrot: TMandelbrot; const ABitmap: TBitmap);
begin
  inherited Create(CreateSuspended);
  FreeOnTerminate := True;
  FMandelbrot := AMandelbrot;
  FBitmap := ABitmap;
end;

destructor TMBThread.Destroy;
begin
  FreeAndNil(FMandelbrot);
  inherited Destroy;
end;

end.
