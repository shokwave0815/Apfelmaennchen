unit mandelbrotmt;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, mandelbrot, mandelbrotthread, ULogicalCPUCount;

type

  TOnFinishCalculation = procedure(ABitmap: TBitmap) of object;

  { TMandelbrotMT }

  TMandelbrotMT = class(TObject)
  private
    FBitmap: TBitmap;
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
    procedure CallOnFinishCalculation;
  public
    property NumThreads: integer read FNumMaxThreads;
    property Width: integer read FWidth;
    property Height: integer read FHeight;
    property StartReal: extended read FStartReal;
    property StartImagenary: extended read FStartImagenary;
    property Zoom: QWord read FZoom write FZoom;
    property MaxIterations: QWord read FMaxIterations write FMaxIterations;
    property OnFinishCalculation: TOnFinishCalculation read FOnFinishCalculation write FOnFinishCalculation;
    constructor Create(const AWidth: integer; const AHeight: integer; const AZoom: QWord;
      const AMaxIterations: QWord);
    destructor Destroy; override;
    procedure SetSize(const AWidth: integer; const AHeight: integer);
    procedure SetStartPoint(const AReal: extended; const AImagenary: extended);
    procedure ZoomInOrOut(const AFactor: double);
    procedure Calulate; virtual;
    function GetBitmap: TBitmap;
  end;

implementation

procedure TMandelbrotMT.FOnExitThread(AMandelbrot: TMandelbrot);
var
  offset: integer;
begin
  offset := AMandelbrot.OffsetX * (FWidth div FNumMaxThreads);
  FBitmap.Canvas.Draw(offset, 0, AMandelbrot.GetBitmap);// Just paint the part calculated by the tread.
  FreeAndNil(AMandelbrot);
  //Decrement number of running threads and if all finished call repaint of the Paintbox.
  FNumRunningThreads -= 1;
  if FNumRunningThreads = 0 then
  begin
    CallOnFinishCalculation;
  end;
end;

procedure TMandelbrotMT.CallOnFinishCalculation;
begin
  if Assigned(FOnFinishCalculation) then
  begin
    FOnFinishCalculation(FBitmap);
  end;
end;

constructor TMandelbrotMT.Create(const AWidth: integer; const AHeight: integer; const AZoom: QWord;
  const AMaxIterations: QWord);
begin
  inherited Create();

  FWidth := AWidth;
  FHeight := AHeight;
  FZoom := AZoom;
  FMaxIterations := AMaxIterations;

  FBitmap := TBitmap.Create;
  FBitmap.SetSize(FWidth, FHeight);
  FBitmap.Clear;

//  FNumMaxThreads := TThread.ProcessorCount;
  FNumMaxThreads := TLogicalCPUCount.GetLogicalCPUCount();
  {$ifdef DARWIN}
  FNumMaxThreads := 10;
  {$endif DARWIN}
  FNumRunningThreads := 0;
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
  FBitmap.SetSize(AWidth, AHeight);
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
  i: integer;
begin
  for i := 0 to FNumMaxThreads - 1 do
  begin
    PartMB := TMandelbrot.Create(i, FWidth div FNumMaxThreads, FHeight, FZoom, FMaxIterations);
    //add remainig pixel to be calculated ba the last thread
    if i = FNumMaxThreads - 1 then
    begin
      PartMB.SetSize(PartMB.Width + FWidth mod FNumMaxThreads, PartMB.Height);
    end;
    PartMB.SetStartPoint(FStartReal + i * ((FWidth div FNumMaxThreads) / FZoom), FStartImagenary);
    mbThread := TMBThread.Create(True, PartMB);

    //Check if creation failed.
    if Assigned(mbThread.FatalException) then
      raise mbThread.FatalException;

    mbThread.OnFinish := @FOnExitThread;
    FNumRunningThreads += 1;
    mbThread.Start;

  end;
end;

function TMandelbrotMT.GetBitmap: TBitmap;
begin
  Result := FBitmap;
end;

end.
