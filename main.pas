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
    procedure PaintMandelbrot();
    procedure SetMousePosToCenter(const Y: Integer; const X: Integer);
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
    Mandelbrot.GetBitmap().SaveToFile(SaveDialog1.FileName);
  end;
end;

procedure TForm1.Button_ZoomClick(Sender: TObject);
begin
  SetZoom(FloatSpinEdit_Zoom.Value);
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
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
      Mandelbrot.SetMaxIterations(Trunc(Mandelbrot.GetMaxIterations() * 1.2));
    end;
    VK_DIVIDE: begin
      if (Mandelbrot.GetMaxIterations() > 1) then
        Mandelbrot.SetMaxIterations(Trunc(Mandelbrot.GetMaxIterations() / 1.2));
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
  PaintBox1.Canvas.Draw(0,0, Mandelbrot.GetBitmap());
end;

procedure TForm1.PaintMandelbrot();
begin
  PaintBox1.Canvas.Draw(0, 0, MandelBrot.GetBitmap());
end;

procedure TForm1.SetMousePosToCenter(const Y: Integer; const X: Integer);
begin
{  StartX := StartX + (X - PaintBox1.Width / 2) / Zoom;
  StartY := StartY + (Y - PaintBox1.Height / 2) / Zoom;
  }
end;

procedure TForm1.SetZoom(Factor: Double);
var
  newYValue: Extended;
  oldYValue: Extended;
  newXValue: Extended;
  oldXValue: Extended;
begin
{  oldXValue:= PaintBox1.Width / Zoom;
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
}
  MandelBrot.ZoomInOrOut(Factor);
end;

procedure TForm1.UpdateStatus;
begin
  Statusbar1.SimpleText:= 'StartX: ' + FormatFloat('0.0##########', Mandelbrot.GetStartReal()) +
                          '/ StartY: ' + FormatFloat('0.0##########', Mandelbrot.GetStartImagenary()) +
                          '/ Zoom: ' + FormatFloat('0.0', Mandelbrot.GetZoom() / 200) + 'x' +
                          '/ Iterations: ' + IntToStr(MandelBrot.GetMaxIterations);
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

