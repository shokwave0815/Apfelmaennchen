unit mandelbrotmt;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Dialogs, mandelbrot, mandelbrotthread, ULogicalCPUCount;

var
  CritSec: TRTLCriticalSection;

type

  TOnFinishCalculation = procedure of object;

  { TMandelbrotMT }

  TMandelbrotMT = class(TObject)
  private
    FBitmap: TBitmap;
    FTargetBitmap: TBitmap;
    FStartReal: extended;
    FStartImagenary: extended;
    FWidth: integer;
    FHeight: integer;
    FMaxIterations: QWord;
    FZoom: QWord;
    FOnFinishCalculation: TOnFinishCalculation;
    FNumRunningThreads: integer;
    FNumMaxThreads: integer;
    procedure FOnExitThread(AMandelbrot: TMandelbrot);
  public
    property NumThreads: integer read FNumMaxThreads;
    property Width: integer read FWidth;
    property Height: integer read FHeight;
    property StartReal: extended read FStartReal;
    property StartImagenary: extended read FStartImagenary;
    property Zoom: QWord read FZoom write FZoom;
    property MaxIterations: QWord read FMaxIterations write FMaxIterations;
    property OnFinishCalculation: TOnFinishCalculation read FOnFinishCalculation write FOnFinishCalculation;
    constructor Create(const AWidth: integer; const AHeight: integer;
      const AZoom: QWord; const AMaxIterations: QWord;
  const ATargetBitmap: TBitmap);
    destructor Destroy; override;
    procedure SetSize(const AWidth: integer; const AHeight: integer);
    procedure SetStartPoint(const AReal: extended; const AImagenary: extended);
    procedure ZoomInOrOut(const AFactor: double);
    procedure Calulate; virtual;
  end;

implementation

procedure TMandelbrotMT.FOnExitThread(AMandelbrot: TMandelbrot);
begin
  EnterCriticalSection(CritSec);
  FBitmap.Canvas.Draw(AMandelbrot.OffsetX, 0, AMandelbrot.GetBitmap);
  Dec(FNumRunningThreads);
  LeaveCriticalSection(CritSec);
  if FNumRunningThreads = 0 then
  begin
    //if all threads finished call event to repaint PaintBox.
    if Assigned(FOnFinishCalculation) then
    begin
      FTargetBitmap.SetSize(FWidth, FHeight);
      FTargetBitmap.Canvas.Draw(0, 0, FBitmap);
      FOnFinishCalculation;
    end;
  end;
end;

constructor TMandelbrotMT.Create(const AWidth: integer; const AHeight: integer; const AZoom: QWord;
  const AMaxIterations: QWord; const ATargetBitmap: TBitmap);
begin
  inherited Create;

  InitCriticalSection(CritSec);

  FWidth := AWidth;
  FHeight := AHeight;
  FZoom := AZoom;
  FMaxIterations := AMaxIterations;

  FBitmap := TBitmap.Create;
  FBitmap.SetSize(FWidth, FHeight);

  FTargetBitmap := ATargetBitmap;

  FNumMaxThreads := GetLogicalCPUCount;
  FNumRunningThreads := 0;
end;

destructor TMandelbrotMT.Destroy;
begin
  DoneCriticalSection(CritSec);
  FreeAndNil(FBitmap);
  inherited Destroy;
end;

procedure TMandelbrotMT.SetSize(const AWidth: integer; const AHeight: integer);
begin
  FWidth := AWidth;
  FHeight := AHeight;
end;

procedure TMandelbrotMT.SetStartPoint(const AReal: extended; const AImagenary: extended);
begin
  FStartReal := AReal;
  FStartImagenary := AImagenary;
end;

procedure TMandelbrotMT.ZoomInOrOut(const AFactor: double);
var
  newYValue: extended;
  oldYValue: extended;
  newXValue: extended;
  oldXValue: extended;
begin
  oldXValue := FWidth / FZoom;
  oldYValue := FHeight / FZoom;

  if AFactor > 0 then
    FZoom := Trunc(FZoom * AFactor)
  else
    FZoom := Trunc(FZoom / Abs(AFactor));

  newXValue := FWidth / FZoom;
  newYValue := FHeight / FZoom;

  FStartReal := FStartReal + (oldXValue - newXValue) / 2;
  FStartImagenary := FStartImagenary + (oldYValue - newYValue) / 2;
end;

procedure TMandelbrotMT.Calulate;
var
  mbThread: TMBThread;
  PartMB: TMandelbrot;
  i, PartWidth: integer;
begin
  FBitmap.SetSize(FWidth, FHeight);
  PartWidth := FWidth div FNumMaxThreads;
  for i := 0 to FNumMaxThreads - 1 do
  begin
    PartMB := TMandelbrot.Create(i * PartWidth, PartWidth, FHeight, FZoom, FMaxIterations);
    //add remainig pixel to be calculated ba the last thread
    if i = FNumMaxThreads - 1 then
    begin
      PartMB.SetSize(PartMB.Width + FWidth mod FNumMaxThreads, PartMB.Height);
    end;
    PartMB.SetStartPoint(FStartReal + i * ((FWidth div FNumMaxThreads) / FZoom), FStartImagenary);
    mbThread := TMBThread.Create(True, PartMB, FBitmap);

    //Check if creation failed.
    if Assigned(mbThread.FatalException) then
      raise mbThread.FatalException;

    mbThread.OnFinish := @FOnExitThread;
    Inc(FNumRunningThreads);
    mbThread.Start;
  end;
end;

end.
