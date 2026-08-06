unit mandelbrotmt;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Dialogs, ULogicalCPUCount, threadmanager;

type
  TOnCalculationDone = procedure of Object;
  { TMandelbrotMT }

  TMandelbrotMT = class(TObject)
  private
    FOnCalculationDone: TOnCalculationDone;
    FBitmap: TBitmap;
    FStartReal: extended;
    FStartImagenary: extended;
    FWidth: integer;
    FHeight: integer;
    FMaxIterations: QWord;
    FZoom: QWord;
    FNumMaxThreads: integer;
  public
    property OnCalculationDone: TOnCalculationDone read FOnCalculationDone write FOnCalculationDone;
    property Bitmap: TBitmap read FBitmap;
    property NumMaxThreads: integer read FNumMaxThreads;
    property Width: integer read FWidth;
    property Height: integer read FHeight;
    property StartReal: extended read FStartReal;
    property StartImagenary: extended read FStartImagenary;
    property Zoom: QWord read FZoom write FZoom;
    property MaxIterations: QWord read FMaxIterations write FMaxIterations;
    constructor Create(const AWidth: integer; const AHeight: integer;
      const AZoom: QWord; const AMaxIterations: QWord);
    destructor Destroy; override;
    procedure SetSize(const AWidth: integer; const AHeight: integer);
    procedure SetStartPoint(const AReal: extended; const AImagenary: extended);
    procedure ZoomInOrOut(const AFactor: double);
    procedure Calulate; virtual;
    procedure AllThreadsFinished(ABitmap: TBitmap);
  end;

implementation

constructor TMandelbrotMT.Create(const AWidth: integer; const AHeight: integer; const AZoom: QWord;
  const AMaxIterations: QWord);
begin
  inherited Create;

  FWidth := AWidth;
  FHeight := AHeight;
  FZoom := AZoom;
  FMaxIterations := AMaxIterations;

  FBitmap := TBitmap.Create;
  FBitmap.SetSize(FWidth, FHeight);

  FNumMaxThreads := GetLogicalCPUCount;
end;

destructor TMandelbrotMT.Destroy;
begin
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
  ThreadManager: TMBThreadManager;
begin
  ThreadManager := TMBThreadManager.Create(True, FWidth, FHeight, FZoom, FMaxIterations, FStartReal, FStartImagenary, FNumMaxThreads);
  ThreadManager.OnFinished := @AllThreadsFinished;
  //Check creation failed.
  if Assigned(ThreadManager.FatalException) then
    raise ThreadManager.FatalException;

  ThreadManager.Start;
end;

procedure TMandelbrotMT.AllThreadsFinished(ABitmap: TBitmap);
begin
  FBitmap.SetSize(FWidth, FHeight);
  FBitmap.Canvas.Draw(0, 0, ABitmap);
  if Assigned(FOnCalculationDone) then
    FOnCalculationDone;
end;

end.
