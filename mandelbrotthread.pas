unit mandelbrotthread;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, mandelbrot;

type

  TOnFinish = procedure(AMandelbrot: TMandelbrot) of object;

  { TMBThread }

  TMBThread = class(TThread)
  private
    FOnFinish: TOnFinish;
    FMandelbrot: TMandelbrot;
    FBitmap: TBitmap;
  protected
    procedure Execute; override;
  public
    property OnFinish: TOnFinish read FOnFinish write FOnFinish;
    constructor Create(CreateSuspended: boolean; const AMandelbrot: TMandelbrot);
    destructor Destroy; override;
  end;

implementation

{ TMBThread }

procedure TMBThread.Execute;
begin
  try
    FMandelbrot.Calulate;
  finally
    if Assigned(FOnFinish) then
    begin
      FOnFinish(FMandelbrot);
    end;
  end;
end;

constructor TMBThread.Create(CreateSuspended: boolean; const AMandelbrot: TMandelbrot);
begin
  inherited Create(CreateSuspended);
  FreeOnTerminate := True;
  FMandelbrot := AMandelbrot;
end;

destructor TMBThread.Destroy;
begin
  FreeAndNil(FMandelbrot);
  inherited Destroy;
end;

end.
