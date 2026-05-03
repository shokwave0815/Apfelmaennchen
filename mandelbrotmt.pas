unit mandelbrotmt;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

type

  { TMandelbrotMT }

  TMandelbrotMT = class(TObject)
  private
    FBitmap: TBitmap;
    FStartReal: extended;
    FStartImagenary: extended;
    FOffsetX: integer;
    FWidth: integer;
    FHeight: integer;
    FMaxIterations: QWord;
    FZoom: QWord;
  public
    property OffsetX: integer read FOffsetX;
    property Width: integer read FWidth;
    property Height: integer read FHeight;
    property StartReal: extended read FStartReal;
    property StartImagenary: extended read FStartImagenary;
    property Zoom: QWord read FZoom write FZoom;
    property MaxIterations: QWord read FMaxIterations write FMaxIterations;
    constructor Create(const AOffsetX: integer; const AWidth: integer; const AHeight: integer;
      const AZoom: QWord; const AMaxIterations: QWord);
    destructor Destroy; override;
    procedure SetSize(const AWidth: integer; const AHeight: integer);
    procedure SetStartPoint(const AReal: extended; const AImagenary: extended);
    procedure ZoomInOrOut(const AFactor: double);
    procedure Calulate; virtual;
    function GetBitmap: TBitmap;
  end;

implementation

constructor TMandelbrotMT.Create(const AOffsetX: integer; const AWidth: integer; const AHeight: integer;
  const AZoom: QWord; const AMaxIterations: QWord);
begin
  inherited Create();

  FOffsetX := AOffsetX;
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
begin
  //ToDo: Implement the threads creation.
end;

function TMandelbrotMT.GetBitmap: TBitmap;
begin
  Result := FBitmap;
end;

end.
