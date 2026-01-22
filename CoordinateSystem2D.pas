unit CoordinateSystem2D;

interface

uses
  ArrayOfTypes;

type
  TCoordinateSystem2D=class
	  private
		  FP0:TValue2d; // Базовая точка
		  FPx:TValue2d; // Вектор - нормаль, выходит из начала мировой координатной системы и указывает направление оси X
		  FPy:TValue2d; // Вектор - нормаль, выходит из начала мировой координатной системы и указывает направление оси Y
		public
      constructor Create;

      function WorldToSystem(Xw, Yw:Double):TValue2d;
      function SystemToWorld(Xs, Ys:Double):TValue2d;
		  procedure RotateSystem(Ugol:Double);

		  procedure Move(Len:Double; Normal:TValue2d);

		  property P0:TValue2d read FP0 write FP0;
		  property Px:TValue2d read FPx write FPx;
		  property Py:TValue2d read FPy write FPy;
	end;

const
	Zero2DVector:TValue2d=(X:0;Y:0);
  One2DXVector:TValue2d=(X:1;Y:0);
  One2DYVector:TValue2d=(X:0;Y:1);

implementation

{ TCoordinateSystem2D }

procedure TCoordinateSystem2D.Move(Len: Double; Normal: TValue2d);
begin
	FP0.X:=FP0.X+Normal.X*Len;
	FP0.Y:=FP0.Y+Normal.Y*Len;
end;

procedure TCoordinateSystem2D.RotateSystem(Ugol: Double);
var
	c,s:Double;
  p:TValue2d;
begin
	c:=Cos(Ugol);
  s:=Sin(Ugol);

	p.x:=c*FPx.X-s*FPx.Y;
  p.y:=s*FPx.X-c*FPx.Y;
  FPx:=p;

	p.x:=c*FPy.X-s*FPy.Y;
  p.y:=s*FPy.X+c*FPy.Y;
  FPy:=p;
end;

function TCoordinateSystem2D.SystemToWorld(Xs, Ys: Double): TValue2d;
begin
	Result.X:=FP0.X+FPx.X*Xs+FPy.X*Ys;
	Result.Y:=FP0.Y+FPx.Y*Xs+FPy.Y*Ys;
end;

constructor TCoordinateSystem2D.Create;
begin
  FP0:=Zero2DVector;
  FPx:=One2DXVector;
  FPy:=One2DYVector;
end;

function TCoordinateSystem2D.WorldToSystem(Xw, Yw: Double): TValue2d;
var
	pw:TValue2d;
begin
	pw.X:=Xw-FP0.X;
	pw.Y:=Yw-FP0.Y;
  Result.X:=(pw.X*FPx.X+pw.Y*FPx.Y)/(FPx.X*FPx.X+FPx.Y*FPx.Y);
  Result.Y:=(pw.X*FPy.X+pw.Y*FPy.Y)/(FPy.X*FPy.X+FPy.Y*FPy.Y);
end;

end.
