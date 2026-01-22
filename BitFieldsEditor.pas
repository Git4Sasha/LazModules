unit BitFieldsEditor;

{$mode objfpc}{$H+}

// Порядок описания полей:
// Имя битового поля
// Нужно ли отображать имя битового поля вертикально ( vud или vdu или пуста ) (vud - надпись сверху-вниз, vdu - надпись снизу-вверх)
// Позиция битового поля в битах (начиная с нуля)
// Количество бит, которое занимает данное битовое поле
// Начальное значение битового поля
// Является ли данное поле, полем только для чтения (ro или не ro)

// Примеры:
// Имя, vud, 0, 3, 5, ,
// Имя,  , 4, 4, 0, ro,


interface

uses
  Classes
  , SysUtils
  , Controls
  , StdCtrls
  , Graphics
  , StringWork
  , ArrayOfTypes
  ;

type
  TCellsInfo=record
    name:string;
    nvert:Integer;        // вертикальное отображение 0-нет вертикального отображения; 1 - вертикальное отображение (надписи сверху-вниз); 2 - вертикальное отображение (надписи снизу--вверх)
    readonly:Boolean;
    offset:Integer;       // Смещение (влево) в битах битового поля
    cnt:Integer;          // Количество бит, которое занимает данное битовое поле
    mask:Cardinal;
  end;
  TArrayOfCellsInfo=array of TCellsInfo;
  PCellsInfo=^TCellsInfo;

  TBitSize=(bs8, bs16, bs32);

  { TBitFieldsEditor }

  TBitFieldsEditor = class(TCustomControl)
  private
    FBitSize: TBitSize;
    FBitCount:Integer;              // Количество бит с которыми выполняется работа (8, 16, 32)
    FCellsHeight:TArrayOfInteger;   // Массив для высот ячеек
    FCellBitWidth:Single;           // Ширина ячейки одного бита
    FCellsInfo:TArrayOfCellsInfo;
    FBitLink:TArrayOfByte;
    FCommonValue:Cardinal;
    FCellDescription:TStrings;
    FEditVal:TEdit;
    FEditCellNum:Integer;
    FShowHeader:Boolean;
    FOnValueChange:TNotifyEvent;

    procedure CreateCellsInfo;
    function GetCellNameHeight: Integer;
    function GetFont: TFont;
    procedure SetBitSize(AValue: TBitSize);
    procedure SetCellDescription(AValue: TStrings);
    procedure SetCellNameHeight(AValue: Integer);
    procedure SetCommonValue(AValue: Cardinal);
    procedure SetFont(AValue: TFont);
    function GetEditorRect(ci:TCellsInfo):TRect;
    procedure SetFShowHeader(AValue: Boolean);
    procedure CalcCellHW;                           // Процедура выполняет разсчёт ширин и высот ячеек

  protected
    procedure DoOnResize; override;
    procedure Paint; override;
    procedure Click; override;
    procedure Loaded; override;
    procedure DrawCell(ci:TCellsInfo);              // Отрисовка битового поля
    procedure CellEditKeyPress(Sender:TObject; var Key:Char);
    procedure CellEditExit(Sender:TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property CommonValue:Cardinal read FCommonValue write SetCommonValue;

  published
    property CellDescription:TStrings read FCellDescription write SetCellDescription;
    property CellNameHeight:Integer read GetCellNameHeight write SetCellNameHeight;
    property Font:TFont read GetFont write SetFont;
    property ShowHeader:Boolean read FShowHeader write SetFShowHeader;
    property BitSize:TBitSize read FBitSize write SetBitSize default bs32;
    property Align;
    property Anchors;
    property BorderSpacing;
    property Color;
    property OnValueChange:TNotifyEvent read FOnValueChange write FOnValueChange;
    property OnClick;
    property Tag;
    property ShowHint;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('OtherComponents',[TBitFieldsEditor]);
end;

{ TBitFieldsEditor }

procedure TBitFieldsEditor.CreateCellsInfo;
var
  i,nb,allbits,n:Integer;
  val:Integer;
  cellpar:TArrayOfString;
  namesep:TArrayOfString;
  lm:Int64;
  pci:PCellsInfo;
  str:string;
begin
  if (csLoading in ComponentState) then Exit;

  SetLength(FCellsInfo, FCellDescription.Count);
  SetLength(FBitLink, FBitCount);

  allbits:=0;
  FCommonValue:=0;
  n:=0;
  for i:=0 to High(FCellsInfo) do begin
    // Формат описателя битового поля - Имя, "v"-имя отображать вертикально или не "v" - горизонтально, смещение поля в битах, кол-во бит для поля, начальное значение, "ro" / не "ro" - только для чтения или нет

    cellpar:=SeparateString(',', FCellDescription[i]+', ,');
    if Length(cellpar)<6 then Continue;

    // Значения в в строке с описание поля: Имя, Вертикальное отображение имени, смещение битового поля, кол-во бит в поле, значение поля

    pci:=@FCellsInfo[n];                      // Адрес на описатель текущего битового поля
    pci^.name:=cellpar[0];                    // Имя битового поля

    if pci^.Name='phase#13adjust' then
      Beep;


    namesep:=SeparateString('#13', pci^.name);
    if Length(namesep)>1 then begin
      pci^.name:='';
      for str in namesep do
        pci^.name:=pci^.name+str+#13;
      Delete(pci^.name, Length(pci^.name), 1);
    end;
    namesep:=nil;

    pci^.nvert:=0;                            // Изначально предполагается, что будет горизонтальное отображение
    if Pos('vud', cellpar[1])<>0 then pci^.nvert:=1;  // Вертикальное отображение написи сверху-вниз
    if Pos('vdu', cellpar[1])<>0 then pci^.nvert:=2;  // Вертикальное отображения надписи снизу-вверх

    if not TryStrToInt(cellpar[2], pci^.offset) then begin     // Смещение (влево) в битах битового поля
      pci^.offset:=0;
      raise Exception.Create('Ошибка при чтении смещения битового поля: ' + FCellDescription[i]);
    end;

    if not TryStrToInt(cellpar[3], pci^.cnt) then begin     // Количество бит, которое занимает данное битовое поле
      pci^.cnt:=1;
      raise Exception.Create('Ошибка при чтении кол-ва бит в битовом поле: ' + FCellDescription[i]);
    end;

    if not TryStrToInt(cellpar[4], val) then begin     // Значение (начальное) битового поля
      val:=0;
      raise Exception.Create('Ошибка при чтении значения по умолчанию: ' + FCellDescription[i]);
    end;

    pci^.readonly:=Pos('ro', cellpar[5])>0;   // Является ли битовое поле полем только для чтения

    lm:=1;
    lm:=(lm shl Int64(pci^.cnt))-1;
    pci^.mask:=lm;
    val:=val and pci^.mask;
    pci^.mask:=pci^.mask shl pci^.offset;     // Маска, которая позволяет выделить значение поля

    FCommonValue:=FCommonValue or (val shl pci^.offset);

    for nb:=pci^.offset to pci^.offset+pci^.cnt - 1 do
      FBitLink[nb]:=n;

    allbits:=allbits+pci^.cnt;
    Inc(n);

    cellpar:=nil;
  end;

  SetLength(FCellsInfo, n); // Коррекция длины массива с учётом реального кол-ва строк в списке, который описывает битовые поля

  if allbits<FBitCount then begin
    SetLength(FCellsInfo, n+1);

    pci:=@FCellsInfo[n];
    pci^.name:='Reserv';
    pci^.offset:=allbits;
    pci^.cnt:=FBitCount-allbits;

    pci^.mask:=(1 shl pci^.cnt)-1;
    pci^.mask:=pci^.mask shl pci^.offset;

    for nb:=pci^.offset to pci^.offset+pci^.cnt - 1 do
      FBitLink[nb]:=n;
  end;

  // Если есть функция для обработки события изменения FCommonValue, то вызывается этот обработчик
  if Assigned(FOnValueChange) then FOnValueChange(self);

  Invalidate;
end;

function TBitFieldsEditor.GetCellNameHeight: Integer;
begin
  Result:=FCellsHeight[0];
end;

function TBitFieldsEditor.GetFont: TFont;
begin
  Result:=Canvas.Font;
end;

procedure TBitFieldsEditor.SetBitSize(AValue: TBitSize);
var
  chw:Boolean;
begin
  if FBitSize=AValue then Exit;
  FBitSize:=AValue;
  case FBitSize of
    bs8:  FBitCount:=8;
    bs16: FBitCount:=16;
    bs32: FBitCount:=32;
  end;
  chw:=not ((csLoading in ComponentState) or (csDesigning in ComponentState));
  if chw then  // Нельзя менять ширину компонента во время его загрузки или во время проектирования формы
    Width:=Round(FBitCount*FCellBitWidth);
  CalcCellHW;
  CreateCellsInfo;
  Repaint;
end;

procedure TBitFieldsEditor.SetCellDescription(AValue: TStrings);
begin
  FCellDescription.Assign(AValue);
  CreateCellsInfo;
end;

procedure TBitFieldsEditor.SetCellNameHeight(AValue: Integer);
begin
  if CellNameHeight=AValue then Exit;
  FCellsHeight[0]:=AValue;
  DoOnResize;
end;

procedure TBitFieldsEditor.SetCommonValue(AValue: Cardinal);
begin
  if FCommonValue=AValue then Exit;
  FCommonValue:=AValue;

  // Если есть функция для обработки события изменения FCommonValue, то вызывается этот обработчик
  if Assigned(FOnValueChange) then FOnValueChange(self);

  Invalidate;
end;

procedure TBitFieldsEditor.SetFont(AValue: TFont);
begin
  Canvas.Font.Assign(AValue);
end;

function TBitFieldsEditor.GetEditorRect(ci: TCellsInfo): TRect;
var
  xlnv,xrnv:Integer;
  y,th:Integer;
begin
  xrnv:=Round(ci.offset*FCellBitWidth);
  xlnv:=Round(xrnv+ci.cnt*FCellBitWidth);
  xrnv:=Width - xrnv;
  xlnv:=Width - xlnv;

  th:=Canvas.TextHeight('0') div 2;
  y:=FCellsHeight[0] + FCellsHeight[1] + FCellsHeight[2] + FCellsHeight[3] div 2; // Координата середины нижней строки (та в которой отображаются значения битовых полей)

  Result:=Rect(xlnv+2, y-th, xrnv-2, y+th);
end;

procedure TBitFieldsEditor.SetFShowHeader(AValue: Boolean);
const
  shh:Integer=0;
begin
  if FShowHeader=AValue then Exit;
  FShowHeader:=AValue;
  if not FShowHeader then begin
    shh:=FCellsHeight[0];   // Сохранение высоты ячейки для имён битовых полей, чтобы при обратном включении видимости 0-й строки возстановить высоту 0-й строки
    SetCellNameHeight(0);   // Установка высоты 0-й строки в ноль, чтобы она не оказывала влияние на разсчёты
  end else
    SetCellNameHeight(shh); // Установка высоты 0-й строки в то значение, которое было сохранено ранее
end;

procedure TBitFieldsEditor.CalcCellHW; // Процедура выполняет разсчёт ширин и высот ячеек
begin
  // При изменении размера необходимо разсчитать высоту 1-й и 3-й строк
  // в них отображаются:
  //  - номер бита
  //  - значение битового поля (в графическом виде)

  FCellBitWidth:=Width/FBitCount;  // Разсчёт дробного значения ширины одной битовой ячейки
  // FCellsHeight[0] - в этой ячейке находится высота строки, с именами полей, от изменения высоты компонента она меняться не должна

  FCellsHeight[2]:=(Height - FCellsHeight[0]) div 3;  // Высота ячейки в которой отображаются (в графическом виде (кружки)) состояния битов должна быть равна ширине, которая задаётся (не пользователем) для этих ячеек

  // Высота ячеек, в которых отображается номера битов и значения битовых полей определяются изходя из того, что осталось после формирования высот ячеек 0 и 2
  FCellsHeight[1]:=(Height - FCellsHeight[0] - FCellsHeight[2]) div 2;
  FCellsHeight[3]:=FCellsHeight[1];
end;

procedure TBitFieldsEditor.DoOnResize;
begin
  // При изменении размера необходимо разсчитать высоту 1-й и 3-й строк
  // в них отображаются:
  //  - номер бита
  //  - значение битового поля
  CalcCellHW;
  Invalidate;
  inherited DoOnResize;
end;

procedure TBitFieldsEditor.Paint;
var
  ci:TCellsInfo;
  y:Integer;
begin
  Canvas.Brush.Style:=bsSolid;
  Canvas.Brush.Color:=Color;
  Canvas.Rectangle(0,0,Width,Height); // Закрашивание всей области рисования

  //Canvas.Draw(); - Можно воспользоваться этим, чтобы не было моргания при перерисовки компонента

  for ci in FCellsInfo do DrawCell(ci);  // Отрисовка всех ячеек

  // Отрисовка горизонтальных линий, которые разделяют строки
  y:=FCellsHeight[0];
  Canvas.Line(0, y, Width, y);  // Линия, которая отделяет 0-ю строку и 1-ю
  y:=y+FCellsHeight[1];
  Canvas.Line(0, y, Width, y);  // Линия, которая отделяет 1-ю строку и 2-ю
  y:=y+FCellsHeight[2];
  Canvas.Line(0, y, Width, y);  // Линия, которая отделяет 3-ю строку и 4-ю
end;

procedure TBitFieldsEditor.Click;
var
  mpos:TPoint;
  celx,cely,h:Integer;
  rct:TRect;
  ci:TCellsInfo;
  val:Cardinal;
begin
  inherited Click;
  mpos:=Self.ScreenToClient(Mouse.CursorPos);

  // Поиск ячеечных координат в которые был выполнен клик мышкой
  celx:=(FBitCount-1) - Trunc(FBitCount * mpos.x / Width);
  h:=0;
  for cely:=0 to High(FCellsHeight) do begin // Вертикальную ячеечную координату необходимо именно искать, т.к. ячейки разные по высоте
    h:=h+FCellsHeight[cely];
    if mpos.y<h then Break;
  end;

  FEditCellNum:=FBitLink[celx];
  ci:=FCellsInfo[FEditCellNum];
  if not ci.readonly then begin  // Если битовое поле предназначено только для чтения, то дальнейшие действия безсмысленны

    FEditVal.Hide; // Прячем редактор значения битового поля

    if cely=2 then begin  // Если кликнули на строку, которая отображает состояние битов, то
      FCommonValue:=FCommonValue xor (1 shl celx);  // инвертируем состояние бита на который кликнули
      DrawCell(ci); // Перерисовка ячейки
      Invalidate;
      // Если есть функция для обработки события изменения FCommonValue, то вызывается этот обработчик
      if Assigned(FOnValueChange) then FOnValueChange(self);
      Exit;
    end;

    if cely=3 then begin // Если произошёл клик над ячейкой в которой хранится значение битового поля, то нужно отобразить в этом поле редактор для изменения значения
      val:=(FCommonValue and ci.mask) shr ci.offset;

      rct:=GetEditorRect(ci);
      FEditVal.SetBounds(rct.Left, rct.Top, rct.Width, rct.Height);
      FEditVal.Text:=IntToStr(val);
      FEditVal.Color:=Color;
      FEditVal.Show;
      FEditVal.SetFocus;
      FEditVal.SelectAll;

      Exit
    end;
  end;
end;

procedure TBitFieldsEditor.Loaded;
begin
  inherited Loaded;
  CreateCellsInfo;
end;

procedure TBitFieldsEditor.DrawCell(ci: TCellsInfo); // Битовое поле може состоять из нескольких бит, потоэтому в этой процедуре отрисовываются все биты, которые относятся к указанному полю
var
  xr,xl,y,i,j,val,yh0,yc:Integer;
  xlnv,xrnv:Integer;
  xgof,ygof:Integer;
  tst:TTextStyle;
  lrect:TRect;
begin
  tst.Alignment:=taCenter;
  tst.Layout:=tlCenter;
  tst.SingleLine:=False;
  tst.Clipping:=True;
  tst.ExpandTabs:=False;
  tst.ShowPrefix:=False;
  tst.Wordbreak:=False;
  tst.Opaque:=True;
  tst.SystemFont:=False;
  tst.RightToLeft:=False;
  tst.EndEllipsis:=False;

  xrnv:=Round(ci.offset*FCellBitWidth);
  xlnv:=Round(xrnv+ci.cnt*FCellBitWidth);
  xrnv:=Width - xrnv;
  xlnv:=Width - xlnv;

  // Отрисовка заголовка (имена битовых полей)
  lrect:=Rect(xlnv, 0, xrnv, FCellsHeight[0]);
  if FShowHeader then begin
    case ci.nvert of
      0:  begin                                 // Горизонтальное отображение
            Canvas.Font.Orientation:=0;
            Canvas.TextRect(lrect, 0, 0, ci.name, tst);
          end;
      1:  begin                                 // Вертикальное отображение сверху-вниз
            Canvas.Font.Orientation:=-900;
            Canvas.TextOut(lrect.Right - (lrect.Width - Canvas.TextHeight(ci.name)) div 2, lrect.Top+(FCellsHeight[0]-Canvas.TextWidth(ci.name)) div 2, ci.name);
            //Canvas.TextRect(lrect, 0, 20, ci.name, tst);

          end;
      2:  begin                                 // Вертикальное отображение снизу-вверх
            Canvas.Font.Orientation:=900;
            Canvas.TextOut(lrect.Left + (lrect.Width - Canvas.TextHeight(ci.name)) div 2, lrect.Height-(FCellsHeight[0]-Canvas.TextWidth(ci.name)) div 2, ci.name);

            //Canvas.TextRect(lrect, 0, 0, ci.name, tst);
          end;
    end;
    Canvas.Font.Orientation:=0;
  end;

  // Отрисовка кружков, которые отображают состояния битовых полей
  xgof:=Trunc(FCellBitWidth*0.77);   // Диаметр кружочка по оси X
  ygof:=77*FCellsHeight[2] div 100;     // Диаметр кружочка по оси Y
  if xgof<ygof then begin            // Как окончательный диаметр выбирается тот, который меньше (и превращается в радиус, так удобнее при отрисовке)
    xgof:=xgof div 2;
    ygof:=xgof;
  end else begin
    ygof:=ygof div 2;
    xgof:=ygof;
  end;

  yh0:=FCellsHeight[0]+FCellsHeight[1]; // Начальная вертикальная координата ячейки в которой отображаются кружки
  y:=yh0 + FCellsHeight[2];             // Конечная вертикальная координата ячейки в которой отображаются кружки
  j:=ci.offset+ci.cnt-1;                // Цикл по всем битам поля
  for i:=ci.offset to j do begin
    xr:=Round(i*FCellBitWidth);
    xl:=Round(xr+FCellBitWidth);
    xr:=Width - xr;
    xl:=Width - xl;

    // Отображение номера бита в ячейке
    Canvas.Brush.Color:=Color;
    if i=j then begin                    // Если отображается последняя ячейка битового поля, то вертикальная линия рисуется на высоту всего элемента
      Canvas.Line(xlnv,0, xlnv, Height);
    end else begin
      Canvas.Line(xl, FCellsHeight[0], xl, y);
    end;
    lrect:=Rect(xl, FCellsHeight[0], xr, yh0);
    Canvas.TextRect(lrect, 0, 0, IntToStr(i), tst);

    // Рисование кружка, который показывает состояние битовой ячейки
    if FCommonValue and (1 shl i)<>0 then
      Canvas.Brush.Color:=clGreen
    else
      Canvas.Brush.Color:=Color;

    xr:=(xr+xl) div 2;
    yc:=(yh0+y) div 2;

    Canvas.Ellipse(xr-xgof, yc-ygof, xr+xgof, yc+ygof);
  end;

  val:=(FCommonValue and ci.mask) shr ci.offset;

  // Отрисовка значений битовых полей
  y:=FCellsHeight[0]+FCellsHeight[1]+FCellsHeight[2];
  Canvas.Brush.Color:=Color;
  lrect:=Rect(xlnv+1, y, xrnv-1, y+FCellsHeight[3]-1);
  Canvas.FillRect(lrect);
  Canvas.TextRect(lrect, 0, 0, Format('%u',[val]), tst);
end;

procedure TBitFieldsEditor.CellEditKeyPress(Sender: TObject; var Key: Char); // Реакция на нажатие какой-либо клавиши в ячейке битового поля
var
  val:Cardinal;
begin
  if Key=#27 then begin
    FEditVal.Hide;
    Exit;
  end;

  if Key=#13 then begin
    val:=StrToInt64(FEditVal.Text);
    FEditVal.Hide;

    val:=(val shl FCellsInfo[FEditCellNum].offset) and FCellsInfo[FEditCellNum].mask;
    FCommonValue:=FCommonValue and (not FCellsInfo[FEditCellNum].mask);
    FCommonValue:=FCommonValue or val;
    DrawCell(FCellsInfo[FEditCellNum]);

    // Если есть функция для обработки события изменения FCommonValue, то вызывается этот обработчик
    if Assigned(FOnValueChange) then FOnValueChange(self);
  end;
end;

procedure TBitFieldsEditor.CellEditExit(Sender: TObject);
begin
  FEditVal.Hide;
end;

constructor TBitFieldsEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  SetLength(FCellsHeight, 4); // Массив для хранения высот ячеек

  FCellBitWidth:=20;
  FBitCount:=32;
  FBitSize:=bs32;
  FShowHeader:=True;
  FCellsHeight[0]:=30;

  Width:=Trunc(FBitCount * FCellBitWidth);
  Height:=3* Trunc(FCellBitWidth) + FCellsHeight[0];

  FCellDescription:=TStringList.Create;
  FCellDescription.Add('Reserv, 0, 0, 5, 0,,');
  FCellDescription.Add('vert, vud, 6, 1, 0, ro,');
  FCellDescription.Add('vert, vdu, 7, 1, 0, ,');
  CreateCellsInfo;

  if not (csDesigning in ComponentState) then begin
    FEditVal:=TEdit.Create(self);
    FEditVal.Align:=alNone;
    FEditVal.AutoSize:=False;
    FEditVal.Visible:=False;
    FEditVal.BorderStyle:=bsNone;
    FEditVal.Alignment:=taCenter;
//    FEditVal.NumbersOnly:=True;
    FEditVal.OnKeyPress:=@CellEditKeyPress;
    FEditVal.OnExit:=@CellEditExit;
    FEditVal.Parent:=self;
    FEditVal.Color:=clCream;
    FEditVal.Constraints.MaxHeight:=30;
  end;
end;

destructor TBitFieldsEditor.Destroy;
begin
  FBitLink:=nil;
  FCellDescription.Free;
  FCellsHeight:=nil;
  FCellsInfo:=nil;
  inherited Destroy;
end;




end.
