unit mandelbrot;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, GraphUtil, GraphType;

const
  HUE_MAX = 360;
  SATURATION_MAX = 255;
  BRIGHTNESS_MAX = 255;
  DIVISOR = 60;
  SATURATION_FACTOR = SATURATION_MAX / DIVISOR;

type

  { TMandelbrot }

  TMandelbrot = class(TObject)
  private
    FBitmap: TBitmap;
    FRawImage: TRawImage;
    FStartReal: extended;
    FStartImagenary: extended;
    FOffsetX: integer;
    FWidth: integer;
    FHeight: integer;
    FMaxIterations: QWord;
    FZoom: QWord;
    function Iterate(const AX: integer; const AY: integer): QWord;
    function CalculateColor(const AIterations: QWord): TColor;
  public
    property Width: integer read FWidth;
    property Height: integer read FHeight;
    property OffsetX: integer read FOffsetX;
    constructor Create(const AOffsetX: integer; const AWidth: integer; const AHeight: integer;
      const AZoom: QWord; const AMaxIterations: QWord);
    destructor Destroy; override;
    procedure SetStartPoint(const AReal: extended; const AImagenary: extended);
    procedure SetSize(const AWidth: integer; const AHeight: integer);
    procedure Calulate; virtual;
    function GetBitmap: TBitmap;
    function GetRawImage: TRawImage;
  end;

implementation

function TMandelbrot.Iterate(const AX: integer; const AY: integer): QWord;
var
  VariableReal: extended;
  VariableImagenary: extended;
  Temp: extended;
  ConstantReal: extended;
  ConstantImagenary: extended;
begin
  VariableReal := 0;
  VariableImagenary := 0;
  Result := 0;

  ConstantReal := FStartReal + AX / FZoom;
  ConstantImagenary := FStartImagenary + AY / FZoom;

  while (Result < FMaxIterations) and (Sqr(VariableReal) + Sqr(VariableImagenary) < 4) do
  begin
    Temp := VariableReal * VariableImagenary;
    VariableReal := Sqr(VariableReal) - Sqr(VariableImagenary) + ConstantReal;
    VariableImagenary := 2 * Temp + ConstantImagenary;
    Result += 1;
  end;
end;

function TMandelbrot.CalculateColor(const AIterations: QWord): TColor;
var
  Hue, Saturation, Brightness: integer;
begin
  Result := HSVRangeToColor(0, 0, 0);

  if AIterations < FMaxIterations then
  begin
    Brightness := BRIGHTNESS_MAX;
    Hue := AIterations mod HUE_MAX;
    Saturation := SATURATION_MAX - Trunc(Hue mod DIVISOR * SATURATION_FACTOR);

    Result := HSVRangeToColor(Hue, Saturation, Brightness);
  end;
end;

constructor TMandelbrot.Create(const AOffsetX: integer; const AWidth: integer; const AHeight: integer;
  const AZoom: QWord; const AMaxIterations: QWord);
begin
  inherited Create;

  FOffsetX := AOffsetX;
  FWidth := AWidth;
  FHeight := AHeight;
  FZoom := AZoom;
  FMaxIterations := AMaxIterations;

  FBitmap := TBitmap.Create;
  FBitmap.SetSize(FWidth, FHeight);
  FBitmap.Clear;

  FRawImage.Init;
  FRawImage.Description.Init_BPP32_A8R8G8B8_BIO_TTB(FWidth, FHeight);
  FRawImage.CreateData(False);
end;

destructor TMandelbrot.Destroy;
begin
  if FRawImage.Data <> nil then
    FRawImage.FreeData;
  FreeAndNil(FBitmap);
  inherited Destroy;
end;

procedure TMandelbrot.SetStartPoint(const AReal: extended; const AImagenary: extended);
begin
  FStartReal := AReal;
  FStartImagenary := AImagenary;
end;

procedure TMandelbrot.SetSize(const AWidth: integer; const AHeight: integer);
begin
  FWidth := AWidth;
  FHeight := AHeight;
  FBitmap.SetSize(AWidth, AHeight);

  if FRawImage.Data <> nil then
    FRawImage.FreeData;
  FRawImage.Init;
  FRawImage.Description.Init_BPP32_A8R8G8B8_BIO_TTB(FWidth, FHeight);
  FRawImage.CreateData(False);
end;

procedure TMandelbrot.Calulate;
var
  x, y: integer;
  NumIterations: QWord;
  PixelData: pbyte;
  BytesPerPixel: integer;
  PixelOffset: pbyte;
begin

  if FRawImage.Data <> nil then
  begin
    BytesPerPixel := FRawImage.Description.BitsPerPixel div 8;
    PixelData := FRawImage.Data;
    for y := 0 to FHeight - 1 do
    begin
      for x := 0 to FWidth - 1 do
      begin
        NumIterations := Iterate(x, y);

        PixelOffset := PixelData + (Y * FRawImage.Description.BytesPerLine) + (X * BytesPerPixel);
        PInteger(PixelOffset)^ := ColorToRGB(CalculateColor(NumIterations));
        //FBitmap.Canvas.Pixels[x, y] := CalculateColor(NumIterations);
      end;
    end;
    FBitmap.LoadFromRawImage(FRawImage, False);
  end;
end;

function TMandelbrot.GetBitmap: TBitmap;
begin
  Result := FBitmap;
end;

function TMandelbrot.GetRawImage: TRawImage;
begin
  Result := FRawImage;
end;

end.
