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
    Label_Calc: TLabel;
    PaintBox: TPaintBox;
    Panel_Head: TPanel;
    SaveDialog: TSaveDialog;
    StatusBar: TStatusBar;
    procedure Button_IncIterationsClick(Sender: TObject);
    procedure Button_DecIterationsClick(Sender: TObject);
    procedure Button_ZoomOutClick(Sender: TObject);
    procedure Button_RepaintClick(Sender: TObject);
    procedure Button_SavePictureClick(Sender: TObject);
    procedure Button_ZoomInClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PaintBoxMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure PaintBoxPaint(Sender: TObject);
  private
    FStartTime: double;

    FBufferImage: TBitmap;
    FMandelBrot: TMandelbrotMT;
    procedure Center(const AX: integer; const AY: integer);
    procedure PaintMandelbrot(ABitmap: TBitmap);
    procedure UpdateStatus;
    procedure RefreshPicture;
  public

  end;

var
  Form_Main: TForm_Main;

implementation

{$R *.lfm}

procedure TForm_Main.FormCreate(Sender: TObject);
begin
  Caption := MyVersion;
  PaintBox.Canvas.AntialiasingMode := amOff;

  FBufferImage := TBitmap.Create;
  FMandelBrot := TMandelbrotMT.Create(PaintBox.Width, PaintBox.Height, 200, 360);
  FMandelbrot.OnFinishCalculation := @PaintMandelbrot;
  FMandelBrot.SetStartPoint(-2, -1.2);
end;

procedure TForm_Main.Button_RepaintClick(Sender: TObject);
begin
  RefreshPicture();
end;

procedure TForm_Main.Button_SavePictureClick(Sender: TObject);
begin
  if (SaveDialog.Execute) then
  begin
    FMandelBrot.GetBitmap().SaveToFile(SaveDialog.FileName);
  end;
end;

procedure TForm_Main.Button_ZoomInClick(Sender: TObject);
begin
  FMandelBrot.ZoomInOrOut(FloatSpinEdit_Zoom.Value);
  UpdateStatus();
end;

procedure TForm_Main.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FreeAndNil(FBufferImage);
  FreeAndNil(FMandelBrot);
end;

procedure TForm_Main.Button_ZoomOutClick(Sender: TObject);
begin
  FMandelBrot.ZoomInOrOut(-1 * FloatSpinEdit_Zoom.Value);
  UpdateStatus();
end;

procedure TForm_Main.Button_IncIterationsClick(Sender: TObject);
begin
  FMandelBrot.MaxIterations := Round(FMandelBrot.MaxIterations * 1.2);
  UpdateStatus();
end;

procedure TForm_Main.Button_DecIterationsClick(Sender: TObject);
begin
  FMandelBrot.MaxIterations := Round(FMandelBrot.MaxIterations / 1.2);
  UpdateStatus();
end;

procedure TForm_Main.FormKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  case Key of
    VK_MULTIPLY: FMandelBrot.MaxIterations := Round(FMandelBrot.MaxIterations * 1.2);
    VK_DIVIDE: FMandelBrot.MaxIterations := Round(FMandelBrot.MaxIterations / 1.2);
    VK_ADD: Button_ZoomInClick(nil);
    VK_SUBTRACT: Button_ZoomOutClick(nil);
    VK_F5: RefreshPicture();
  end;
  UpdateStatus();
end;

procedure TForm_Main.FormResize(Sender: TObject);
var
  OldX, OldY: integer;
begin
  OldX := FMandelBrot.Width div 2;
  OldY := FMandelbrot.Height div 2;

  FMandelBrot.SetSize(PaintBox.Width, PaintBox.Height);
  Center(OldX, OldY);
  RefreshPicture();
end;

procedure TForm_Main.FormShow(Sender: TObject);
begin
  RefreshPicture();
end;

procedure TForm_Main.PaintBoxMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  if (Button = mbLeft) then
  begin
    Center(X, Y);
    FMandelbrot.MaxIterations := round(FMandelbrot.MaxIterations * 1.05);
    FMandelBrot.ZoomInOrOut(FloatSpinEdit_Zoom.Value);
  end else if (Button = mbRight) then
  begin
    FMandelbrot.MaxIterations := round(FMandelbrot.MaxIterations / 1.05);
    FMandelBrot.ZoomInOrOut(-1 * FloatSpinEdit_Zoom.Value);
  end;
  RefreshPicture();
end;

procedure TForm_Main.PaintBoxPaint(Sender: TObject);
begin
  PaintBox.Canvas.Draw(0, 0, FBufferImage);
end;

procedure TForm_Main.PaintMandelbrot(ABitmap: TBitmap);
begin
  PaintBox.Canvas.Draw(0, 0, ABitmap);

  FBufferImage.SetSize(PaintBox.Width, PaintBox.Height);
  FBufferImage.Canvas.Draw(0, 0, ABitmap);
  Label_Calc.Caption := 'Rendertime: ' + FormatFloat('#,##0.0', (GetTickCount64 - FStartTime) / 1000) + 's with ' + IntToStr(FMandelBrot.NumThreads) + ' CPU-Threads used';

  PaintBox.Invalidate;

  Label_Calc.Visible := True;
  UpdateStatus();

end;

procedure TForm_Main.Center(const AX: integer; const AY: integer);
begin
  FMandelBrot.SetStartPoint(FMandelBrot.StartReal + (AX - PaintBox.Width / 2) / FMandelBrot.Zoom,
    FMandelBrot.StartImagenary + (AY - PaintBox.Height / 2) / FMandelBrot.Zoom);
end;

procedure TForm_Main.UpdateStatus;
begin
  StatusBar.SimpleText := 'StartX: ' + FormatFloat('#,##0.0##########', FMandelBrot.StartReal) +
    '/ StartY: ' + FormatFloat('#,##0.0##########', FMandelBrot.StartImagenary) + '/ Zoom: ' +
    FormatFloat('#,##0.0', FMandelBrot.Zoom / 200) + 'x' + '/ Iterations: ' +
    FormatFloat('#,##0', FMandelBrot.MaxIterations * 1.0);
end;

procedure TForm_Main.RefreshPicture;
begin
  Label_Calc.Caption := 'calculating...';
  Application.ProcessMessages;

  FStartTime := GetTickCount64;
  FMandelBrot.Calulate();

end;

end.
