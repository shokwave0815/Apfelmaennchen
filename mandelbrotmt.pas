unit mandelbrotmt;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, mandelbrot, mandelbrotthread;

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
    procedure FOnExitThread(AMandelbrot: TMandelbrot);
        procedure CallOnFinishCalculation;

  public
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
begin
  FBitmap.Canvas.Draw(0, 0, AMandelbrot.GetBitmap);// ToDo: Just paint the part calculated by the tread.
  FreeAndNil(AMandelbrot);
  CallOnFinishCalculation;
  //ToDo: Decrement number of running threads and if all finished call repaint of the Paintbox.
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
begin
  //ToDo: Implement the threads creation.
  PartMB := TMandelbrot.Create(0, FWidth, FHeight, FZoom, FMaxIterations);
  PartMB.SetStartPoint(FStartReal, FStartImagenary);
  mbThread := TMBThread.Create(True, PartMB);
  mbThread.OnFinish := @FOnExitThread;
  mbThread.Start;
end;

function TMandelbrotMT.GetBitmap: TBitmap;
begin
  Result := FBitmap;
end;

end.
