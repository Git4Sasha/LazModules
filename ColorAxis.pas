unit ColorAxis;// ÷вета равномерно распредел€ютс€ от минимального зн-€ к максимальному


interface
uses Graphics, SysUtils;

type
  TArrayOfColor=array of TColor;

  { TColorAxis }

  TColorAxis=class
	 private
		  FDlt:Double;
		  procedure SetMinValue(const Value: Double);
		  procedure SetMaxValue(const Value: Double);
		  procedure GetDlt;
	  protected
		  FMinValue:Double;
		  FMaxValue:Double;
		  FColors:TArrayOfColor;
		  FNumColor:Integer;
	  public
		  constructor Create;
		  destructor Destroy; override;
		  procedure AddColor(Color:TColor);
		  function ValueToColor(Value:Double):TColor;
		  procedure ValueToRGB(Value:Double; var R,G,B:Byte);
		  property MinValue:Double Read FMinValue write SetMinValue;
		  property MaxValue:Double Read FMinValue write SetMaxValue;
  end;


implementation

function RGB(r, g, b: Byte): TColor;
begin
  Result := (r or (g shl 8) or (b shl 16));
end;

function GetRValue(rgb: TColor): Byte;
begin
  Result := Byte(rgb);
end;

function GetGValue(rgb: TColor): Byte;
begin
  Result := Byte(rgb shr 8);
end;

function GetBValue(rgb: TColor): Byte;
begin
  Result := Byte(rgb shr 16);
end;


{ TColorAxis }

procedure TColorAxis.AddColor(Color: TColor);
begin
  SetLength(FColors, FNumColor+1);
	FColors[FNumColor]:=Color;
	Inc(FNumColor);
	GetDlt;
end;

constructor TColorAxis.Create;
begin
	FMinValue:=0;
	FMaxValue:=1;
	FNumColor:=0;
	FColors:=nil;
end;

destructor TColorAxis.Destroy;
begin
	FNumColor:=0;
	FColors:=nil;

  inherited Destroy;
end;

procedure TColorAxis.GetDlt;
begin
	if FNumColor<=1 then
		Exit;
	FDlt:=(FMaxValue-FMinValue)/(FNumColor-1);
end;

procedure TColorAxis.SetMaxValue(const Value: Double);
begin
	FMaxValue := Value;
	GetDlt;
end;

procedure TColorAxis.SetMinValue(const Value: Double);
begin
	FMinValue := Value;
	GetDlt;
end;

function TColorAxis.ValueToColor(Value: Double): TColor;
var
	R,G,B:Byte;
begin
	ValueToRGB(Value,R,G,B);
	Result:=RGB(R,G,B);
end;

procedure TColorAxis.ValueToRGB(Value: Double; var R, G, B: Byte);
var
	NColor:Integer;
	dR,dG,dB:Integer;
begin
	if Value>=FMaxValue then begin
		R:=GetRValue(FColors[FNumColor-1]);
		G:=GetGValue(FColors[FNumColor-1]);
		B:=GetBValue(FColors[FNumColor-1]);
		Exit;
	end;
	if Value<=FMinValue then begin
		R:=GetRValue(FColors[0]);
		G:=GetGValue(FColors[0]);
		B:=GetBValue(FColors[0]);
		Exit;
	end;
	Value:=Value-FMinValue;
	NColor:=Trunc(Value/FDlt);
	Value:=(Value-NColor*FDlt)/FDlt;

	R:=GetRValue(FColors[NColor]);
	G:=GetGValue(FColors[NColor]);
	B:=GetBValue(FColors[NColor]);

	dR:=GetRValue(FColors[NColor+1])-R;
	dG:=GetGValue(FColors[NColor+1])-G;
	dB:=GetBValue(FColors[NColor+1])-B;

	R:=Trunc(R+dR*Value);
	G:=Trunc(G+dG*Value);
	B:=Trunc(B+dB*Value);
end;

end.
