unit mandelbrotthread;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, mandelbrot;

type

  TOnFinish = procedure(AMandelbrot: TMandelbrot) of object;

  { TMBThread }

  TMBThread = class(TThread)
  private
    FOnFinish: TOnFinish;
    FMandelbrot: TMandelbrot;
    procedure CallOnFinish;
  protected
    procedure Execute; override;
  public
    property OnFinish: TOnFinish read FOnFinish write FOnFinish;
    constructor Create(CreateSuspended: boolean; AMandelbrot: TMandelbrot);
    destructor Destroy; override;
  end;

implementation

{ TMBThread }

procedure TMBThread.CallOnFinish;
begin
  if Assigned(FOnFinish) then
  begin
    FOnFinish(FMandelbrot);
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

constructor TMBThread.Create(CreateSuspended: boolean; AMandelbrot: TMandelbrot);
begin
  inherited Create(CreateSuspended);
  FreeOnTerminate := True;
  FMandelbrot := AMandelbrot;
end;

destructor TMBThread.Destroy;
begin
  inherited Destroy;
end;

end.
