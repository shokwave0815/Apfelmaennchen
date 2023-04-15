unit mandelbrot;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, GraphUtil;

type

  { TMandelbrot }

  TMandelbrot = class(TObject)
    private
      FBitmap: TBitmap;
      FStartReal: Extended;
      FStartImagenary: Extended;
      FWidth: Integer;
      FHeight: Integer;
      FMaxIterations: QWord;
      FZoom: QWord;
      function Iterate(const AX: Integer; const AY: Integer): QWord;
      function CalculateColor(const AIterations: QWord): TColor;
    public
      property Width: Integer read FWidth;
      property Height: Integer read FHeight;
      property StartReal: Extended read FStartReal;
      property StartImagenary: Extended read FStartImagenary;
      property Zoom: QWord read FZoom write FZoom;
      property MaxIterations: QWord read FMaxIterations write FMaxIterations;
      constructor Create(const AWidth: Integer; const AHeight: Integer; const AZoom: QWord; const AMaxIterations: QWord);
      destructor Destroy; override;
      procedure SetSize(const AWidth: Integer; const AHeight: Integer);
      procedure SetStartPoint(const AReal: Extended; const AImagenary: Extended);
      procedure ZoomInOrOut(const AFactor: Double);
      procedure Calulate; virtual;
      function GetBitmap: TBitmap;
  end;

implementation

function TMandelbrot.Iterate(const AX: Integer; const AY: Integer): QWord;
var
  Real: Extended;
  Imagenary: Extended;
  Temp: Extended;
  ConstantReal: Extended;
  ConstantImagenary: Extended;
begin
  Real:= 0;
  Imagenary:= 0;
  Result:= 0;

  ConstantReal:= FStartReal + AX / FZoom;
  ConstantImagenary:= FStartImagenary + AY / FZoom;

  while ((Result < FMaxIterations) and ((Sqr(Real) + Sqr(Imagenary)) < 4)) do
  begin
    Temp:= Real * Imagenary;
    Real:= Sqr(Real) - Sqr(Imagenary) + ConstantReal;
    Imagenary:= 2 * Temp + ConstantImagenary;
    inc(Result);
  end;
end;

function TMandelbrot.CalculateColor(const AIterations: QWord): TColor;
var Hue, Saturation, Brightness, Divisor: Integer;
  NumIterations: QWord;
begin
  NumIterations:= AIterations;
  Divisor:= 40;
  while NumIterations > 360 do
    Dec(NumIterations, 360);

  Hue:= NumIterations + 180;
  Saturation:= 255;
  Saturation:= 255 - Trunc((NumIterations mod Divisor) * (255 / Divisor));

  if AIterations = FMaxIterations then
    Brightness := 0
  else
    Brightness:= 255;

  Result := HSVRangeToColor(Hue, Saturation, Brightness);
end;

constructor TMandelbrot.Create(const AWidth: Integer; const AHeight: Integer;
  const AZoom: QWord; const AMaxIterations: QWord);
begin
  inherited Create();

  FWidth:= AWidth;
  FHeight:= AHeight;
  FZoom:= AZoom;
  FMaxIterations:= AMaxIterations;

  FBitmap:= TBitmap.Create;
  FBitmap.SetSize(FWidth, FHeight);
  FBitmap.Clear;
end;

destructor TMandelbrot.Destroy;
begin
  FreeAndNil(FBitmap);
  inherited Destroy;
end;

procedure TMandelbrot.SetSize(const AWidth: Integer; const AHeight: Integer);
begin
  FWidth:= AWidth;
  FHeight:= AHeight;
  FBitmap.SetSize(AWidth, AHeight);
end;

procedure TMandelbrot.SetStartPoint(const AReal: Extended; const AImagenary: Extended);
begin
  FStartReal:= AReal;
  FStartImagenary:= AImagenary;
end;

procedure TMandelbrot.ZoomInOrOut(const AFactor: Double);
var
  newYValue: Extended;
  oldYValue: Extended;
  newXValue: Extended;
  oldXValue: Extended;
begin
  oldXValue:= FWidth / FZoom;
  oldYValue:= FHeight / FZoom;

  if AFactor > 0 then
    FZoom := Trunc(FZoom * AFactor)
  else
    FZoom := Trunc(FZoom / Abs(AFactor));

  newXValue := FWidth / FZoom;
  newYValue := FHeight / FZoom;

  FStartReal:= FStartReal + (oldXValue - newXValue) / 2;
  FStartImagenary:= FStartImagenary + (oldYValue - newYValue) / 2;
end;

procedure TMandelbrot.Calulate;
var x, y: Integer;
  NumIterations: QWord;
begin
  for y := 0 to FHeight - 1 do
  begin
    for x := 0 to FWidth - 1 do
    begin
      NumIterations:= Iterate(x, y);
      FBitmap.Canvas.Pixels[x, y] := CalculateColor(NumIterations);
    end;
  end;
end;

function TMandelbrot.GetBitmap: TBitmap;
begin
  Result:= FBitmap;
end;

end.

