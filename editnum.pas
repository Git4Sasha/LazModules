unit EditNum;

{$mode objfpc}{$H+}

interface

uses
  Classes
  , SysUtils
  , Forms
  , Controls
  , Graphics
  , StdCtrls;

type

  { TEditNum }

  TEditNum = class(TCustomEdit)
  private
    FErrorLastConvert:Boolean;  // Признак ошибки при последнем преобразовании
    FBadColor:TColor;           // Цвет, который назначается компоненту, если преобразование строки в число выполнилось с ошибкой
    FOldColor:TColor;           // Сюда будет сохранятся цвет компонента до замены на FBadColor, чтобы потом возстановить цвет компонента по умолчанию

    function GetTxtToDouble: Double;
    function GetTxtToFloat: Single;
    function GetTxtToInt: Integer;
    function GetTxtToInt64: Int64;
    procedure SetTxtToDouble(AValue: Double);
    procedure SetTxtToFloat(AValue: Single);
    procedure SetTxtToInt(AValue: Integer);
    procedure SetTxtToInt64(AValue: Int64);

  public
    constructor Create(AOwner: TComponent); override;

    function ToFloat(sep:array of Char):Single;

    property TxtToInt:Integer read GetTxtToInt write SetTxtToInt;
    property TxtToFloat:Single read GetTxtToFloat write SetTxtToFloat;
    property TxtToInt64:Int64 read GetTxtToInt64 write SetTxtToInt64;
    property TxtToDouble:Double read GetTxtToDouble write SetTxtToDouble;
    property ErrorLastConvert:Boolean read FErrorLastConvert;
    property AutoSelected;
  published
    property Action;
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property AutoSelect;
    property BadColor:TColor read FBadColor write FBadColor;
    property BidiMode;
    property BorderSpacing;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property EchoMode;
    property Enabled;
    property Font;
    property HideSelection;
    property MaxLength;
    property NumbersOnly;
    property ParentBidiMode;
    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditingDone;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDrag;
    property OnUTF8KeyPress;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabStop;
    property TabOrder;
    property Text;
    property TextHint;
    property Visible;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('OtherComponents',[TEditNum]);
end;

{ TEditNum }

function TEditNum.GetTxtToInt: Integer;
var
  ec:Integer;
begin
  Val(Text, Result, ec); // функция val умеет разпозновать число записанное в шестнадцатеричной форме, если есть префикс 0x
  if ec=0  then begin
    Color:=FOldColor; // Если всё хорошо, то устанавливаем то цвет, который был раньше
    FErrorLastConvert:=False; // Сброс признака, ошибки преобразования
  end else begin
    Result:=0;
    if Color<>FBadColor then
      FOldColor:=Color; // Запоминаем текущий цвет до его изменения
    Color:=FBadColor; // Назначаем цвет неверного преобразования
    FErrorLastConvert:=True; // Установка признака, ошибки преобразования
  end
end;

function TEditNum.GetTxtToInt64: Int64;
var
  ec:Integer;
begin
  Val(Text, Result, ec); // функция val умеет разпозновать число записанное в шестнадцатеричной форме, если есть префикс 0x
  if ec=0  then begin
    Color:=FOldColor; // Если всё хорошо, то устанавливаем то цвет, который был раньше
    FErrorLastConvert:=False; // Сброс признака, ошибки преобразования
  end else begin
    Result:=0;
    if Color<>FBadColor then
      FOldColor:=Color; // Запоминаем текущий цвет до его изменения
    Color:=FBadColor; // Назначаем цвет неверного преобразования
    FErrorLastConvert:=True; // Установка признака, ошибки преобразования
  end
end;

procedure TEditNum.SetTxtToDouble(AValue: Double);
begin
  Text:=FormatFloat('0.#########', AValue);
end;

function TEditNum.GetTxtToFloat: Single;
begin
  if TryStrToFloat(Text, Result) then begin
    Color:=FOldColor; // Если всё хорошо, то устанавливаем то цвет, который был раньше
    FErrorLastConvert:=False; // Сброс признака, ошибки преобразования
  end else begin
    Result:=0;
    if Color<>FBadColor then
      FOldColor:=Color; // Запоминаем текущий цвет до его изменения
    Color:=FBadColor; // Назначаем цвет неверного преобразования
    FErrorLastConvert:=True; // Установка признака, ошибки преобразования
  end
end;

function TEditNum.GetTxtToDouble: Double;
begin
  if TryStrToFloat(Text, Result) then begin
    Color:=FOldColor; // Если всё хорошо, то устанавливаем то цвет, который был раньше
    FErrorLastConvert:=False; // Сброс признака, ошибки преобразования
  end else begin
    Result:=0;
    if Color<>FBadColor then
      FOldColor:=Color;       // Запоминаем текущий цвет до его изменения
    Color:=FBadColor;         // Назначаем цвет неверного преобразования
    FErrorLastConvert:=True;  // Установка признака, ошибки преобразования
  end
end;

procedure TEditNum.SetTxtToFloat(AValue: Single);
begin
  Text:=FormatFloat('0.######', AValue);
end;

procedure TEditNum.SetTxtToInt(AValue: Integer);
begin
  Text:=Format('%d',[AValue]);
end;

procedure TEditNum.SetTxtToInt64(AValue: Int64);
begin
  Text:=Format('%d',[AValue]);
end;

constructor TEditNum.Create(AOwner: TComponent);
begin
  inherited;
  FBadColor:=255; // Цвет ошибки - красный
  FOldColor:=Color;
end;

function TEditNum.ToFloat(sep: array of Char): Single;
var
  ch,chi:Char;
begin
  ch:=DefaultFormatSettings.DecimalSeparator;
  if Length(sep)=0 then begin
    DefaultFormatSettings.DecimalSeparator:='.';
    Result:=TxtToFloat;
    DefaultFormatSettings.DecimalSeparator:=ch;
    Exit;
  end;

  for chi in sep do begin
    DefaultFormatSettings.DecimalSeparator:=chi;
    Result:=TxtToFloat;
    if not FErrorLastConvert then Break;
  end;

  DefaultFormatSettings.DecimalSeparator:=ch;
end;

end.
