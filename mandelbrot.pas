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
      FRangeReal: Extended;
      FRangeImagenary: Extended;
      FWidth: Integer;
      FHeight: Integer;
      FMaxIterations: LongWord;
      FZoom: LongWord;
      function Iterate(): LongWord;
      function CalculateColor(const Iterations: LongWord): TColor;
    public
      constructor Create(const Width: Integer; const Height: Integer; const Zoom: LongWord);
      destructor Destroy(); override;
      procedure SetSize(const Width: Integer; const Height: Integer);
      procedure SetStartPoint(const x: Extended; const y: Extended);
      Procedure SetZoom(const Zoom: LongWord);
      procedure Calulate();
      function GetBitmap(): TBitmap;
  end;

implementation

{ TMandelbrot }

function TMandelbrot.Iterate: LongWord;
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

  cx:= FStartReal + FWidth / Zoom;
  cy:= FStartImagenary + FHeight / Zoom;

  while (Result < MaxIterations) and ((zx * zx + zy * zy) < 4) do
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

  ColorVal:= NumIterations div MaxIterations;
  Hue:= (360 * NumIterations) div MaxIterations - 180;
  Saturation:= 255;
  Brightness:= 255 - colorval;
  if NumIterations = MaxIterations then
    Brightness := 0;

  Result := HSVRangeToColor(Hue, Saturation, Brightness);
end;

constructor TMandelbrot.Create(const Width: Integer; const Height: Integer;
  const Zoom: LongWord);
begin
  inherited Create();

  FWidth:= Width;
  FHeight:= Height;
  FZoom:= Zoom;

  FBitmap:= TBitmap.Create;
  FBitmap.SetSize(FWidth, FHeight);
  FBitmap.Clear;
end;

destructor TMandelbrot.Destroy;
begin
  FreeAndNil(FBitmap);

  inherited Destroy;
end;

procedure TMandelbrot.SetSize(const Width: Integer; const Height: Integer);
begin
  FWidth:= Width;
  FHeight:= Height;
  FBitmap.SetSize(Width, Height);
  FBitmap.Clear;
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

procedure TMandelbrot.Calulate;
var x, y: Integer;
  NumIterations: LongWord;
begin
  for y := 0 to FHeight - 1 do
  begin
    for x := 0 to FWidth - 1 do
    begin
      NumIterations:= Iterate(y, x);
      Result.Canvas.Pixels[x, y] := CalculateColor(NumIterations);
    end;
  end;
end;

function TMandelbrot.GetBitmap: TBitmap;
begin
  Result:= FBitmap;
end;

end.

