unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
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
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox1Paint(Sender: TObject);
  private
    FPicture: TBitmap;
    FMandelBrot: TMandelbrot;
    procedure PaintMandelbrot();
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

  FPicture:= TBitmap.Create;
  FMandelBrot:= TMandelbrot.Create(PaintBox1.Width, PaintBox1.Height, 200, 360);
  FMandelBrot.SetStartPoint(-2, -1.2);
  FMandelBrot.Calulate();
end;

procedure TForm1.Button_RepaintClick(Sender: TObject);
begin
  RefreshPicture();
end;

procedure TForm1.Button_SavePictureClick(Sender: TObject);
begin
  if (SaveDialog1.Execute) then
  begin
    FMandelBrot.GetBitmap().SaveToFile(SaveDialog1.FileName);
  end;
end;

procedure TForm1.Button_ZoomClick(Sender: TObject);
begin
  FMandelBrot.ZoomInOrOut(FloatSpinEdit_Zoom.Value);
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FreeAndNil(FPicture);
  FreeAndNil(FMandelBrot);
end;

procedure TForm1.Button_OutClick(Sender: TObject);
begin
  FMandelBrot.ZoomInOrOut(-1 * FloatSpinEdit_Zoom.Value);
end;

procedure TForm1.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_MULTIPLY: FMandelBrot.SetMaxIterations(Round(FMandelBrot.GetMaxIterations() * 1.2));
    VK_DIVIDE: FMandelBrot.SetMaxIterations(Round(FMandelBrot.GetMaxIterations() / 1.2));
    VK_ADD: Button_ZoomClick(nil);
    VK_SUBTRACT: Button_OutClick(nil);
    VK_F5: RefreshPicture();
  end;
  UpdateStatus();
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  FMandelBrot.SetSize(PaintBox1.Width, PaintBox1.Height);
  RefreshPicture();
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
    FMandelBrot.SetPosToCenter(X, Y);
    //FMandelBrot.ZoomInOrOut(FloatSpinEdit_Zoom.Value);
    RefreshPicture();
  end;
end;

procedure TForm1.PaintBox1Paint(Sender: TObject);
begin
  PaintBox1.Canvas.Draw(0,0, FPicture);
end;

procedure TForm1.PaintMandelbrot();
begin
  PaintBox1.Canvas.Draw(0, 0, FPicture);
end;

procedure TForm1.UpdateStatus;
begin
  Statusbar1.SimpleText:= 'StartX: ' + FormatFloat('0.0##########', FMandelBrot.GetStartReal()) +
                          '/ StartY: ' + FormatFloat('0.0##########', FMandelBrot.GetStartImagenary()) +
                          '/ Zoom: ' + FormatFloat('0.0', FMandelBrot.GetZoom() / 200) + 'x' +
                          '/ Iterations: ' + IntToStr(FMandelBrot.GetMaxIterations);
end;

procedure TForm1.RefreshPicture;
begin
  Label_Calc.Visible:=true;
  Application.ProcessMessages;

  FMandelBrot.Calulate();
  FPicture.SetSize(PaintBox1.Width, PaintBox1.Height);
  FPicture.Canvas.Draw(0, 0, FMandelBrot.GetBitmap());
  PaintBox1.Invalidate;


  Label_Calc.Visible:=false;
  UpdateStatus();
end;

end.

