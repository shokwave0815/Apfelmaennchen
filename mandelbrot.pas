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
      FMaxIterations: LongWord;
      FZoom: LongWord;
      function Iterate(const x: Integer; const y: Integer): LongWord;
      function CalculateColor(const Iterations: LongWord): TColor;
    public
      constructor Create(const Width: Integer; const Height: Integer; const Zoom: LongWord; const MaxIterations: LongWord);
      destructor Destroy(); override;
      function GetZoom(): LongWord;
      function GetMaxIterations(): LongWord;
      function GetStartReal(): Extended;
      function GetStartImagenary(): Extended;
      procedure SetPosToCenter(const x: Integer; const y: Integer);
      procedure SetSize(const Width: Integer; const Height: Integer);
      procedure SetStartPoint(const x: Extended; const y: Extended);
      procedure SetZoom(const Zoom: LongWord);
      procedure ZoomInOrOut(const Factor: Double);
      procedure SetMaxIterations(const MaxIterations: LongWord);
      procedure Calulate();
      function GetBitmap(): TBitmap;
  end;

implementation

{ TMandelbrot }

function TMandelbrot.Iterate(const x: Integer; const y: Integer): LongWord;
var
  xt: Extended;
  zy: Extended;
  zx: Extended;
  cy: Extended;
  cx: Extended;
begin
  zx := 0;
  zy := 0;
  Result := 0;

  cx:= FStartReal + x / FZoom;
  cy:= FStartImagenary + y / FZoom;

  while (Result < FMaxIterations) and ((zx * zx + zy * zy) < 4) do
  begin
    xt := zx * zy;
    zx := zx * zx - zy * zy + cx;
    zy := 2 * xt + cy;
    inc(Result);
  end;
end;

function TMandelbrot.CalculateColor(const Iterations: LongWord): TColor;
var Hue, Saturation, Brightness: Integer;
  ColorVal: Integer;
  NumIterations: LongWord;
begin
  NumIterations:= Iterations;

  ColorVal:= NumIterations div FMaxIterations;
  Hue:= (360 * NumIterations) div FMaxIterations - 180;
  Saturation:= 255;
  if NumIterations = FMaxIterations then
    Brightness := 0
  else
  Brightness:= 255 - colorval;

  Result := HSVRangeToColor(Hue, Saturation, Brightness);
end;

constructor TMandelbrot.Create(const Width: Integer; const Height: Integer;
  const Zoom: LongWord; const MaxIterations: LongWord);
begin
  inherited Create();

  FWidth:= Width;
  FHeight:= Height;
  FZoom:= Zoom;
  FMaxIterations:= MaxIterations;

  FBitmap:= TBitmap.Create;
  FBitmap.SetSize(FWidth, FHeight);
  FBitmap.Clear;
end;

destructor TMandelbrot.Destroy;
begin
  FreeAndNil(FBitmap);

  inherited Destroy;
end;

function TMandelbrot.GetZoom: LongWord;
begin
  Result:= FZoom;
end;

function TMandelbrot.GetMaxIterations: LongWord;
begin
  Result:= FMaxIterations;
end;

function TMandelbrot.GetStartReal(): Extended;
begin
  Result:= FStartReal;
end;

function TMandelbrot.GetStartImagenary(): Extended;
begin
  Result:= FStartImagenary;
end;

procedure TMandelbrot.SetPosToCenter(const x: Integer; const y: Integer);
begin
  FStartReal := FStartReal + (x - FWidth / 2) / FZoom;
  FStartImagenary := FStartImagenary + (y - FHeight / 2) / FZoom;
end;

procedure TMandelbrot.SetSize(const Width: Integer; const Height: Integer);
var x, y: Integer;
begin
  x:= FWidth div 2;
  y:= Fheight div 2;
  FWidth:= Width;
  FHeight:= Height;
  FBitmap.SetSize(Width, Height);
  SetPosToCenter(x, y);
end;

procedure TMandelbrot.SetStartPoint(const x: Extended; const y: Extended);
begin
  FStartReal:= x;
  FStartImagenary:= y;
end;

procedure TMandelbrot.SetZoom(const Zoom: LongWord);
begin
  FZoom:= Zoom;
end;

procedure TMandelbrot.ZoomInOrOut(const Factor: Double);
var
  newYValue: Extended;
  oldYValue: Extended;
  newXValue: Extended;
  oldXValue: Extended;
begin
  oldXValue:= FWidth / FZoom;
  oldYValue:= FHeight / FZoom;

  if Factor > 0 then
    FZoom := Trunc(FZoom * Factor)
  else
    FZoom := Trunc(FZoom / Abs(Factor));

  newXValue := FWidth / FZoom;
  newYValue := FHeight / FZoom;

  FStartReal:= FStartReal + (oldXValue - newXValue) / 2;
  FStartImagenary:= FStartImagenary + (oldYValue - newYValue) / 2;

  //FMaxIterations:= Trunc(FMaxIterations * (1 + Factor / 30));
end;

procedure TMandelbrot.SetMaxIterations(const MaxIterations: LongWord);
begin
  if (FMaxIterations > 4) then
   FMaxIterations:= MaxIterations
  else
    Inc(FMaxIterations);
end;

procedure TMandelbrot.Calulate;
var x, y: Integer;
  NumIterations: LongWord;
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

