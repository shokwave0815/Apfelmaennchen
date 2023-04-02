unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, GraphUtil,
  StdCtrls, LCLType, ComCtrls, Spin, mandelbrot;

const MyVersion = 'Apfelmännchen V1.0 ©2023 by shoKwave';

type

  { TForm1 }

  TForm1 = class(TForm)
    Button_SavePicture: TButton;
    Button_Repaint: TButton;
    Button_Zoom: TButton;
    Button_Out: TButton;
    FloatSpinEdit_Zoom: TFloatSpinEdit;
    Label_Calc: TLabel;
    PaintBox1: TPaintBox;
    Panel1: TPanel;
    SaveDialog1: TSaveDialog;
    StatusBar1: TStatusBar;
    procedure Button_OutClick(Sender: TObject);
    procedure Button_RepaintClick(Sender: TObject);
    procedure Button_SavePictureClick(Sender: TObject);
    procedure Button_ZoomClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox1Paint(Sender: TObject);
  private
    MandelBrot: TMandelbrot;
    Zoom: LongWord;
    CurrentPicture: TBitmap;
    StartX, StartY: Extended;
    MaxIterations: Integer;
    procedure PaintMandelbrot();
    function Iterate(const y: Integer; const x: Integer): Integer;
    procedure SetMousePosToCenter(const Y: Integer; const X: Integer);
    function CalculateMandelbrot(): TBitmap;
    function CalculateColor(const NumIterations: Integer): TColor;
    procedure SetZoom(Factor: Double);
    procedure UpdateStatus();
    procedure RefreshPicture();
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  Caption:= MyVersion;
//  StartX := -2;
//  StartY := -1.2;
//  Zoom := 200;
//  MaxIterations:= 256;
  Mandelbrot:= TMandelbrot.Create(PaintBox1.Width, PaintBox1.Height, 200, 256);
  Mandelbrot.SetStartPoint(-2, -1.2);
  MandelBrot.Calulate();
end;

procedure TForm1.Button_RepaintClick(Sender: TObject);
begin
  RefreshPicture();
end;

procedure TForm1.Button_SavePictureClick(Sender: TObject);
begin
  if (SaveDialog1.Execute) then
  begin
    CurrentPicture.SaveToFile(SaveDialog1.FileName);
  end;
end;

procedure TForm1.Button_ZoomClick(Sender: TObject);
begin
  SetZoom(FloatSpinEdit_Zoom.Value);
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FreeAndNil(CurrentPicture);
  FreeAndNil(Mandelbrot);
end;

procedure TForm1.Button_OutClick(Sender: TObject);
begin
  SetZoom(-1 * FloatSpinEdit_Zoom.Value);
end;

procedure TForm1.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_MULTIPLY: begin
      MaxIterations:= Trunc(MaxIterations * 1.2);
    end;
    VK_DIVIDE: begin
      if (MaxIterations > 1) then
        MaxIterations := Trunc(MaxIterations / 1.2);
    end;
    VK_ADD: Button_ZoomClick(nil);
    VK_SUBTRACT: Button_OutClick(nil);
  end;
  UpdateStatus();
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  RefreshPicture();
end;

procedure TForm1.PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) then
  begin
    SetMousePosToCenter(Y, X);
    SetZoom(FloatSpinEdit_Zoom.Value);
    RefreshPicture();
  end;
end;

procedure TForm1.PaintBox1Paint(Sender: TObject);
begin
  //PaintMandelbrot();
  PaintBox1.Canvas.Draw(0,0, Mandelbrot.GetBitmap());
end;

procedure TForm1.PaintMandelbrot();
begin
  if (CurrentPicture <> Nil) then
    FreeAndNil(CurrentPicture);
  CurrentPicture:= CalculateMandelbrot();
  PaintBox1.Canvas.Draw(0, 0, CurrentPicture);
end;

function TForm1.Iterate(const y: integer; const x: integer): Integer;
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

  cx:= StartX + x / Zoom;
  cy:= StartY + y / Zoom;

  while (Result < MaxIterations) and ((zx * zx + zy * zy) < 4) do
  begin
    xt := zx * zy;
    zx := zx * zx - zy * zy + cx;
    zy := 2 * xt + cy;
    inc(Result);
  end;
end;

procedure TForm1.SetMousePosToCenter(const Y: Integer; const X: Integer);
begin
  StartX := StartX + (X - PaintBox1.Width / 2) / Zoom;
  StartY := StartY + (Y - PaintBox1.Height / 2) / Zoom;
end;

function TForm1.CalculateMandelbrot: TBitmap;
var x, y, NumIterations: Integer;
begin
  Result:= TBitmap.Create;
  Result.SetSize(PaintBox1.Width, PaintBox1.Height);
  Result.Clear;

  for y := 0 to Result.Height - 1 do
  begin
    for x := 0 to Result.Width - 1 do
    begin
      NumIterations:= Iterate(y, x);
      Result.Canvas.Pixels[x, y] := CalculateColor(NumIterations);
    end;
  end;
end;

function TForm1.CalculateColor(const NumIterations: Integer): TColor;
var Hue, Saturation, Brightness: Integer;
  ColorVal: Integer;
begin
  ColorVal:= NumIterations div MaxIterations;
  Hue:= (360 * NumIterations) div MaxIterations - 180;
  Saturation:= 255;
  Brightness:= 255 - colorval;
  if NumIterations = MaxIterations then
    Brightness := 0;

  Result := HSVRangeToColor(Hue, Saturation, Brightness);
end;

procedure TForm1.SetZoom(Factor: Double);
var
  newYValue: Extended;
  oldYValue: Extended;
  newXValue: Extended;
  oldXValue: Extended;
begin
  oldXValue:= PaintBox1.Width / Zoom;
  oldYValue:= PaintBox1.Height / Zoom;

  if Factor > 0 then
    Zoom := Trunc(Zoom * Factor)
  else
    Zoom := Trunc(Zoom / Abs(Factor));

  newXValue := PaintBox1.Width / Zoom;
  newYValue := PaintBox1.Height / Zoom;

  StartX:= StartX + (oldXValue - newXValue) / 2;
  StartY:= StartY + (oldYValue - newYValue) / 2;
  MaxIterations:= Trunc(MaxIterations * (1 + Factor / 30));
end;

procedure TForm1.UpdateStatus;
begin
  Statusbar1.SimpleText:= 'StartX: ' + FormatFloat('0.0##########', StartX) +
                          '/ StartY: ' + FormatFloat('0.0##########', StartY) +
                          '/ Zoom: ' + FormatFloat('0.0', Zoom / 200) + 'x' +
                          '/ Iterations: ' + IntToStr(MaxIterations);
end;

procedure TForm1.RefreshPicture;
begin
  Label_Calc.Visible:=true;
  Application.ProcessMessages;

  Mandelbrot.Calulate();
  PaintBox1.Invalidate;


  Label_Calc.Visible:=false;
  UpdateStatus();
end;

end.

