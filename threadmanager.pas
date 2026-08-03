unit threadmanager;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, mandelbrotthread, mandelbrot;

type
  { TMBThreadManager }

  TMBThreadManager = class(TThread)
  private
    FWidth: integer;
    Fheight: integer;
    FZoom: QWord;
    FMaxIterations: QWord;
    FStartReal: extended;
    FStartImagenary: extended;
    FBitmap: TBitmap;
    FMaxThreads: Integer;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: boolean; const AWidth, AHeight: integer;
      const AZoom, AMaxIterations: QWord; const AStartReal, AStartImagenary: extended; AMaxThreads: Integer);
    destructor Destroy; override;
    function GetBitmap: TBitmap;
  end;

implementation

{ TMBThreadManager }

procedure TMBThreadManager.Execute;
var
  PartMB: TMandelbrot;
  i, PartWidth: integer;
  Threads: array of TMBThread;
begin
  FBitmap.SetSize(FWidth, FHeight);
  SetLength(Threads, FMaxThreads);
  PartWidth := FWidth div FMaxThreads;
  for i := 0 to FMaxThreads - 1 do
  begin
    PartMB := TMandelbrot.Create(i * PartWidth, PartWidth, FHeight, FZoom, FMaxIterations);
    //add remainig pixel to be calculated ba the last thread
    if i = FMaxThreads - 1 then
    begin
      PartMB.SetSize(PartMB.Width + FWidth mod FMaxThreads, PartMB.Height);
    end;
    PartMB.SetStartPoint(FStartReal + i * ((FWidth div FMaxThreads) / FZoom), FStartImagenary);

    Threads[i] := TMBThread.Create(True, PartMB);

    //Check if creation failed.
    if Assigned(Threads[i].FatalException) then
      raise Threads[i].FatalException;

    Threads[i].Start;
  end;

  for i := 0 to FMaxThreads - 1 do
  begin
    Threads[i].WaitFor;
    //Check if execution failed.
    if Assigned(Threads[i].FatalException) then
      raise Threads[i].FatalException;
  end;

  for i := 0 to FMaxThreads - 1 do
  begin
    FBitmap.Canvas.Draw(Threads[i].Mandelbrot.OffsetX, 0, Threads[i].Mandelbrot.GetBitmap);
    Threads[i].Free;
  end;

  SetLength(Threads, 0);
end;

constructor TMBThreadManager.Create(CreateSuspended: boolean; const AWidth,
  AHeight: integer; const AZoom, AMaxIterations: QWord; const AStartReal,
  AStartImagenary: extended; AMaxThreads: Integer);
begin
  inherited Create(CreateSuspended);
  FreeOnTerminate := False;
  FWidth := AWidth;
  FHeight := AHeight;
  FZoom := AZoom;
  FMaxIterations := AMaxIterations;
  FStartReal := AStartReal;
  FStartImagenary := AStartImagenary;
  FMaxThreads := AMaxThreads;

  FBitmap := TBitmap.Create;
  FBitmap.SetSize(FWidth, FHeight);
  FBitmap.Clear;
end;

destructor TMBThreadManager.Destroy;
begin
  FreeAndNil(FBitmap);
  inherited Destroy;
end;

function TMBThreadManager.GetBitmap: TBitmap;
begin
  Result := FBitmap;
end;

end.
