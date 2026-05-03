unit mandelbrot;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, GraphUtil;

const
  COLOR_OFFSET = 0;
  HUE_MAX = 360;
  SATURATION_MAX = 255;
  BRIGHTNESS_MAX = 255;
  DIVISOR = 60;

type

  { TMandelbrot }

  TMandelbrot = class(TObject)
  private
    FBitmap: TBitmap;
    FStartReal: extended;
    FStartImagenary: extended;
    FLeft: integer;
    FWidth: integer;
    FHeight: integer;
    FMaxIterations: QWord;
    FZoom: QWord;
    function Iterate(const AX: integer; const AY: integer): QWord;
    function CalculateColor(const AIterations: QWord): TColor;
  public
    property Left: integer read FLeft;
    property Width: integer read FWidth;
    property Height: integer read FHeight;
    property StartReal: extended read FStartReal;
    property StartImagenary: extended read FStartImagenary;
    property Zoom: QWord read FZoom write FZoom;
    property MaxIterations: QWord read FMaxIterations write FMaxIterations;
    constructor Create(const ALeft: integer; const AWidth: integer; const AHeight: integer; const AZoom: QWord;
      const AMaxIterations: QWord);
    destructor Destroy; override;
    procedure SetSize(const AWidth: integer; const AHeight: integer);
    procedure SetStartPoint(const AReal: extended; const AImagenary: extended);
    procedure ZoomInOrOut(const AFactor: double);
    procedure Calulate; virtual;
    function GetBitmap: TBitmap;
  end;

implementation

function TMandelbrot.Iterate(const AX: integer; const AY: integer): QWord;
var
  real: extended;
  Imagenary: extended;
  Temp: extended;
  ConstantReal: extended;
  ConstantImagenary: extended;
begin
  real := 0;
  Imagenary := 0;
  Result := 0;

  ConstantReal := FStartReal + AX / FZoom;
  ConstantImagenary := FStartImagenary + AY / FZoom;

  while ((Result < FMaxIterations) and ((Sqr(real) + Sqr(Imagenary)) < 4)) do
  begin
    Temp := real * Imagenary;
    real := Sqr(real) - Sqr(Imagenary) + ConstantReal;
    Imagenary := 2 * Temp + ConstantImagenary;
    Result += 1;
  end;
end;

function TMandelbrot.CalculateColor(const AIterations: QWord): TColor;
var
  Hue, Saturation, Brightness: integer;
  NumIterations: QWord;
begin
  NumIterations := AIterations;
  while NumIterations > HUE_MAX do
    NumIterations -= HUE_MAX;

  Hue := NumIterations + COLOR_OFFSET;
  Saturation := SATURATION_MAX - Trunc((NumIterations mod DIVISOR) * (SATURATION_MAX / DIVISOR));

  if AIterations = FMaxIterations then
    Brightness := 0
  else
    Brightness := BRIGHTNESS_MAX;

  Result := HSVRangeToColor(Hue, Saturation, Brightness);
end;

constructor TMandelbrot.Create(const ALeft: integer; const AWidth: integer;
  const AHeight: integer; const AZoom: QWord; const AMaxIterations: QWord);
begin
  inherited Create();

  FLeft := ALeft;
  FWidth := AWidth;
  FHeight := AHeight;
  FZoom := AZoom;
  FMaxIterations := AMaxIterations;

  FBitmap := TBitmap.Create;
  FBitmap.SetSize(FWidth, FHeight);
  FBitmap.Clear;
end;

destructor TMandelbrot.Destroy;
begin
  FreeAndNil(FBitmap);
  inherited Destroy;
end;

procedure TMandelbrot.SetSize(const AWidth: integer; const AHeight: integer);
begin
  FWidth := AWidth;
  FHeight := AHeight;
  FBitmap.SetSize(AWidth, AHeight);
end;

procedure TMandelbrot.SetStartPoint(const AReal: extended; const AImagenary: extended);
begin
  FStartReal := AReal;
  FStartImagenary := AImagenary;
end;

procedure TMandelbrot.ZoomInOrOut(const AFactor: double);
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

procedure TMandelbrot.Calulate;
var
  x, y: integer;
  NumIterations: QWord;
begin
  for y := 0 to FHeight - 1 do
  begin
    for x := 0 to FWidth - 1 do
    begin
      NumIterations := Iterate(x, y);
      FBitmap.Canvas.Pixels[x, y] := CalculateColor(NumIterations);
    end;
  end;
end;

function TMandelbrot.GetBitmap: TBitmap;
begin
  Result := FBitmap;
end;

end.
