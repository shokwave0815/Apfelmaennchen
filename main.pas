unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, LCLType, ComCtrls, Spin, mandelbrot;

const MyVersion = 'Apfelmännchen V1.0 ©2023 by shoKwave';

type

  { TFormMain }

  TFormMain = class(TForm)
    Button_SavePicture: TButton;
    Button_Repaint: TButton;
    Button_Zoom: TButton;
    Button_Out: TButton;
    FloatSpinEdit_Zoom: TFloatSpinEdit;
    Label_Calc: TLabel;
    PaintBox: TPaintBox;
    PanelHead: TPanel;
    SaveDialog: TSaveDialog;
    StatusBar: TStatusBar;
    procedure Button_OutClick(Sender: TObject);
    procedure Button_RepaintClick(Sender: TObject);
    procedure Button_SavePictureClick(Sender: TObject);
    procedure Button_ZoomClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxPaint(Sender: TObject);
  private
    FBufferImage: TBitmap;
    FMandelBrot: TMandelbrot;
    procedure Center(const AX: Integer; const AY: Integer);
    procedure PaintMandelbrot;
    procedure UpdateStatus;
    procedure RefreshPicture;
  public

  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

procedure TFormMain.FormCreate(Sender: TObject);
begin
  Caption:= MyVersion;
  PaintBox.Canvas.AntialiasingMode:= amOff;

  FBufferImage:= TBitmap.Create;
  FMandelBrot:= TMandelbrot.Create(PaintBox.Width, PaintBox.Height, 200, 360);
  FMandelBrot.SetStartPoint(-2, -1.2);
  FMandelBrot.Calulate();
end;

procedure TFormMain.Button_RepaintClick(Sender: TObject);
begin
  RefreshPicture();
end;

procedure TFormMain.Button_SavePictureClick(Sender: TObject);
begin
  if (SaveDialog.Execute) then
  begin
    FMandelBrot.GetBitmap().SaveToFile(SaveDialog.FileName);
  end;
end;

procedure TFormMain.Button_ZoomClick(Sender: TObject);
begin
  FMandelBrot.ZoomInOrOut(FloatSpinEdit_Zoom.Value);
end;

procedure TFormMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FreeAndNil(FBufferImage);
  FreeAndNil(FMandelBrot);
end;

procedure TFormMain.Button_OutClick(Sender: TObject);
begin
  FMandelBrot.ZoomInOrOut(-1 * FloatSpinEdit_Zoom.Value);
end;

procedure TFormMain.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_MULTIPLY: FMandelBrot.MaxIterations:= Round(FMandelBrot.MaxIterations * 1.2);
    VK_DIVIDE: FMandelBrot.MaxIterations:= Round(FMandelBrot.MaxIterations / 1.2);
    VK_ADD: Button_ZoomClick(nil);
    VK_SUBTRACT: Button_OutClick(nil);
    VK_F5: RefreshPicture();
  end;
  UpdateStatus();
end;

procedure TFormMain.FormResize(Sender: TObject);
var OldX, OldY: Integer;
begin
  OldX:= FMandelBrot.Width div 2;
  OldY:= FMandelbrot.Height div 2;

  FMandelBrot.SetSize(PaintBox.Width, PaintBox.Height);
  Center(OldX, OldY);
  RefreshPicture();
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
  RefreshPicture();
end;

procedure TFormMain.PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) then
  begin
    Center(X, Y);
    RefreshPicture();
  end;
end;

procedure TFormMain.PaintBoxPaint(Sender: TObject);
begin
  PaintBox.Canvas.Draw(0,0, FBufferImage);
end;

procedure TFormMain.PaintMandelbrot;
begin
  PaintBox.Canvas.Draw(0, 0, FBufferImage);
end;

procedure TFormMain.Center(const AX: Integer; const AY: Integer);
begin
  FMandelBrot.SetStartPoint(FMandelBrot.StartReal + (AX - PaintBox.Width / 2) / FMandelBrot.Zoom,
                            FMandelBrot.StartImagenary + (AY - PaintBox.Height / 2) / FMandelBrot.Zoom);
end;

procedure TFormMain.UpdateStatus;
begin
  StatusBar.SimpleText:= 'StartX: ' + FormatFloat('#,##0.0##########', FMandelBrot.StartReal) +
                          '/ StartY: ' + FormatFloat('#,##0.0##########', FMandelBrot.StartImagenary) +
                          '/ Zoom: ' + FormatFloat('#,##0.0', FMandelBrot.Zoom / 200) + 'x' +
                          '/ Iterations: ' + FormatFloat('#,##0', FMandelBrot.MaxIterations * 1.0);
end;

procedure TFormMain.RefreshPicture;
var   StartTime: Double;
begin
  Label_Calc.caption:='calculating...';
  Application.ProcessMessages;

  StartTime:= GetTickCount64;
  FMandelBrot.Calulate();

  FBufferImage.SetSize(PaintBox.Width, PaintBox.Height);
  FBufferImage.Canvas.Draw(0, 0, FMandelBrot.GetBitmap());
  Label_Calc.Caption:='Rendertime: ' + FormatFloat('#,##0.0', (GetTickCount64 - StartTime) / 1000) + 's';

  PaintBox.Invalidate;


  Label_Calc.Visible:=true;
  UpdateStatus();
end;

end.

