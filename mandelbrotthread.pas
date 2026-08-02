unit mandelbrotthread;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, mandelbrot;

type

  { TMBThread }

  TMBThread = class(TThread)
  private
    FMandelbrot: TMandelbrot;
  protected
    procedure Execute; override;
  public
    property Mandelbrot: TMandelbrot read FMandelbrot;
    constructor Create(CreateSuspended: boolean; const AMandelbrot: TMandelbrot);
    destructor Destroy; override;
  end;

implementation

{ TMBThread }

procedure TMBThread.Execute;
begin
  FMandelbrot.Calulate;
end;

constructor TMBThread.Create(CreateSuspended: boolean; const AMandelbrot: TMandelbrot);
begin
  inherited Create(CreateSuspended);

  FreeOnTerminate := False;
  FMandelbrot := AMandelbrot;
end;

destructor TMBThread.Destroy;
begin
  FreeAndNil(FMandelbrot);
  inherited Destroy;
end;

end.
