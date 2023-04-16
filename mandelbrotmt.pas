unit mandelbrotmt;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, mandelbrot;

type

  { TMandelbrotMT }

  TMandelbrotMT = class(TMandelbrot)
  public
    procedure Calculate; reintroduce;
  private
  end;

implementation

{ TMandelbrotMT }

procedure TMandelbrotMT.Calculate;
begin

end;

end.

