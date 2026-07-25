unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, LCLType, ComCtrls, Spin, mandelbrotmt;

const
  MyVersion = 'Apfelmännchen V1.1 ©2026 by shoKwave';

type

  { TForm_Main }

  TForm_Main = class(TForm)
    Button_DecIterations: TButton;
    Button_IncIterations: TButton;
    Button_SavePicture: TButton;
    Button_Repaint: TButton;
    Button_ZoomIn: TButton;
    Button_ZoomOut: TButton;
    FloatSpinEdit_Zoom: TFloatSpinEdit;
    Label_Info: TLabel;
    Label_Calc: TLabel;
    PaintBox: TPaintBox;
    Panel_Head: TPanel;
    SaveDialog: TSaveDialog;
    procedure Button_IncIterationsClick(Sender: TObject);
    procedure Button_DecIterationsClick(Sender: TObject);
    procedure Button_ZoomOutClick(Sender: TObject);
    procedure Button_RepaintClick(Sender: TObject);
    procedure Button_SavePictureClick(Sender: TObject);
    procedure Button_ZoomInClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure PaintBoxMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure PaintBoxPaint(Sender: TObject);
    procedure PaintBoxResize(Sender: TObject);
  private
    FOldSize: TPoint;
    FCalculating: boolean;
    FRenderTime: double;
    FStartTime: double;
    FBufferImage: TBitmap;
    FMandelBrot: TMandelbrotMT;
    procedure Center(const AX: integer; const AY: integer);
    procedure UpdateStatus;
    procedure StartCalculation;
    procedure FinishCalculation(const ABitmap: TBitmap);
  public

  end;

var
  Form_Main: TForm_Main;

implementation

{$R *.lfm}

procedure TForm_Main.FormCreate(Sender: TObject);
begin
  FOldSize := TPoint.Create(0, 0);
  FCalculating := False;
  Caption := MyVersion;
  PaintBox.Canvas.AntialiasingMode := amOff;

  FBufferImage := TBitmap.Create;
  FMandelBrot := TMandelbrotMT.Create(PaintBox.Width, PaintBox.Height, 200, 360);
  FMandelbrot.OnFinishCalculation := @FinishCalculation;
  FMandelBrot.SetStartPoint(-2.0, -1.2);
end;

procedure TForm_Main.Button_RepaintClick(Sender: TObject);
begin
  StartCalculation;
end;

procedure TForm_Main.Button_SavePictureClick(Sender: TObject);
begin
  if SaveDialog.Execute then
  begin
    FMandelBrot.GetBitmap.SaveToFile(SaveDialog.FileName);
  end;
end;

procedure TForm_Main.Button_ZoomInClick(Sender: TObject);
begin
  FMandelBrot.ZoomInOrOut(FloatSpinEdit_Zoom.Value);
  UpdateStatus;
end;

procedure TForm_Main.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FreeAndNil(FBufferImage);
  FreeAndNil(FMandelBrot);
end;

procedure TForm_Main.Button_ZoomOutClick(Sender: TObject);
begin
  FMandelBrot.ZoomInOrOut(-1 * FloatSpinEdit_Zoom.Value);
  UpdateStatus;
end;

procedure TForm_Main.Button_IncIterationsClick(Sender: TObject);
begin
  FMandelBrot.MaxIterations := Round(FMandelBrot.MaxIterations * 1.2);
  UpdateStatus;
end;

procedure TForm_Main.Button_DecIterationsClick(Sender: TObject);
begin
  FMandelBrot.MaxIterations := Round(FMandelBrot.MaxIterations / 1.2);
  UpdateStatus;
end;

procedure TForm_Main.FormKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  case Key of
    VK_MULTIPLY: FMandelBrot.MaxIterations := Round(FMandelBrot.MaxIterations * 1.2);
    VK_DIVIDE: FMandelBrot.MaxIterations := Round(FMandelBrot.MaxIterations / 1.2);
    VK_ADD: Button_ZoomInClick(nil);
    VK_SUBTRACT: Button_ZoomOutClick(nil);
    VK_F5: StartCalculation;
  end;
  UpdateStatus;
end;

procedure TForm_Main.PaintBoxMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  if Button = mbLeft then
  begin
    Center(X, Y);
    FMandelbrot.MaxIterations := round(FMandelbrot.MaxIterations * 1.05);
    FMandelBrot.ZoomInOrOut(FloatSpinEdit_Zoom.Value);
  end;

  if Button = mbRight then
  begin
    FMandelbrot.MaxIterations := round(FMandelbrot.MaxIterations / 1.05);
    FMandelBrot.ZoomInOrOut(-1 * FloatSpinEdit_Zoom.Value);
  end;

  StartCalculation;
end;

procedure TForm_Main.PaintBoxPaint(Sender: TObject);
begin
  PaintBox.Canvas.Draw(
    PaintBox.Width div 2 - FBufferImage.Width div 2,
    PaintBox.Height div 2 - FBufferImage.Height div 2,
    FBufferImage);
end;

procedure TForm_Main.PaintBoxResize(Sender: TObject);
var
  OldX, OldY: integer;
begin
  if not FCalculating and (FOldSize.x <> PaintBox.Width) and (FOldSize.y <> PaintBox.Height)then
  begin
    OldX := FMandelBrot.Width div 2;
    OldY := FMandelbrot.Height div 2;

    FMandelBrot.SetSize(PaintBox.Width, PaintBox.Height);
    FOldSize.X := FMandelBrot.Width;
    FOldSize.Y := FMandelBrot.Height;

    Center(OldX, OldY);
    StartCalculation;
  end;
end;

procedure TForm_Main.Center(const AX: integer; const AY: integer);
begin
  FMandelBrot.SetStartPoint(
    FMandelBrot.StartReal + (AX - PaintBox.Width / 2) / FMandelBrot.Zoom,
    FMandelBrot.StartImagenary + (AY - PaintBox.Height / 2) / FMandelBrot.Zoom);
end;

procedure TForm_Main.UpdateStatus;
begin
  Label_Info.Caption := 'StartX: ' + FormatFloat('#,##0.0##########', FMandelBrot.StartReal) + LineEnding +
    'StartY: ' + FormatFloat('#,##0.0##########', FMandelBrot.StartImagenary) + LineEnding +
    'Zoom: ' + FormatFloat('#,##0.0', FMandelBrot.Zoom / 200) + 'x' + LineEnding +
    'Iterations: ' + FormatFloat('#,##0', FMandelBrot.MaxIterations * 1.0);

  if FCalculating then
  begin
    Label_Calc.Caption := 'Calculating...';
  end else
  begin
    Label_Calc.Caption := 'Rendertime: ' + FormatFloat('#,##0.0###', FRenderTime) + 's using ' +
      IntToStr(FMandelBrot.NumThreads) + ' CPU-Threads for a picture size of ' +
      IntToStr(FBufferImage.Width) + ' x ' + IntToStr(FBufferImage.Height) + ' pixels';
  end;
end;

procedure TForm_Main.StartCalculation;
begin
  if not FCalculating then
  begin
    Panel_Head.Enabled := False;
    FCalculating := True;
    PaintBox.Cursor:= crHourGlass;
    UpdateStatus;
    FStartTime := GetTickCount64;
    FMandelBrot.Calulate;
  end;
end;

//Called by event FMandelbrot.OnFinishCalculation
procedure TForm_Main.FinishCalculation(const ABitmap: TBitmap);
begin
  FRenderTime := (GetTickCount64 - FStartTime) / 1000;
  FBufferImage.SetSize(ABitmap.Width, ABitmap.Height);
  FBufferImage.Canvas.Draw(0, 0, ABitmap);
  PaintBox.Invalidate;

  PaintBox.Cursor:= crDefault;
  FCalculating := False;
  UpdateStatus;
  Panel_Head.Enabled := True;
end;

end.
