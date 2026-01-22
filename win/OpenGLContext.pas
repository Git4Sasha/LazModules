unit OpenGLContext;

{$mode objfpc}{$H+}

interface

uses
   Classes
  ,SysUtils
  ,Controls
  ,TypInfo
  ,dglOpenGL
  ,Windows
  ,LMessages
  ,Graphics
  ,LCLType
  ,ArrayOfTypes
  ;

type
  { TSimpleGLContext }

  TSimpleGLContext=class(TWinControl)
  private
    FCanvas: TCanvas; // Только в режиме редактирования формы
    Fhrc:HGLRC;
    Fdc:HDC;
    FGLContextInit:Boolean;           // Признак того, что контекст OpenGL создан
    FUseDefaultPaint:Boolean;         // Признак необходимости запуска отрисовки по умолчанию
    FLastErrorStr:string;             // Строка с последней возникшей ошибкой
    FOnAfterGLInit:TNotifyEvent;      // Объект - процедура для обслуживания свойства OnAfterGLInit
    FOnBeforeGLDeinit:TNotifyEvent;   // Объект - процедура для обслуживания свойства OnBeforeGLDeinit
    FOnPaint:TNotifyEvent;            // Переменная - процедура для обслуживания свойства OnPaint
    fOpenGLMajorVersion: Cardinal;
    fOpenGLMinorVersion: Cardinal;
    FRGBA: Boolean;
    FRedBits        : Cardinal;
    FGreenBits      : Cardinal;
    FBlueBits       : Cardinal;

    FMultiSampling  : Cardinal;
    FAlphaBits      : Cardinal;
    FDepthBits      : Cardinal;
    FStencilBits    : Cardinal;
    FAUXBuffers     : Cardinal;

    procedure SetAlphaBits(AValue: Cardinal);
    procedure SetAUXBuffers(AValue: Cardinal);
    procedure SetDepthBits(AValue: Cardinal);
    procedure SetMultiSampling(AValue: Cardinal);
    procedure SetOpenGLMajorVersion(AValue: Cardinal);
    procedure SetOpenGLMinorVersion(AValue: Cardinal);

    procedure SetRGBA(AValue: boolean);

    procedure SetBlueBits(AValue: Cardinal);
    procedure SetGreenBits(AValue: Cardinal);
    procedure SetRedBits(AValue: Cardinal);
    procedure SetStencilBits(AValue: Cardinal);

  protected
    FClearColor:    TColor;                                             // Цвет фона
    FClearColorGL:  TValue4f;                                           // Цвет фона в удобном для некоторых случаев виде

    procedure Loaded; override;                                         // Процедура запускается, когда все свойства установились в те значения, которые указанные в инспекторе объектов
    procedure SetClearColor(AValue: TColor); virtual;                   // Процедура обслуживает свойство ClearColor

    procedure WMPaint(var Msg: TLMPaint); message LM_PAINT;
    procedure WMEraseBkgnd(var Message: TLMEraseBkgnd); message LM_ERASEBKGND;

    procedure OpenGLAttributesChanged;

    procedure CreateWnd; override;    // Создание окна
    procedure DestroyWnd; override;   // Функция уничтожения онак (уничтожение идентификатора окна)
    procedure DoOnResize; override;   // Для обновления области отрисовки OpenGL
    procedure DoOnPaint;  virtual;    // Вызов процедуры OnPaint
    procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure DeinitGLContext;  virtual; // Завершение работы с контекстом OpenGL
    procedure DefaultPaint;     virtual;  // Эта процедура запускается, когда не назначен обработчик для события OnPaint
    procedure InitGLContext;    virtual; // Инициализация контекста OpenGL
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ClearScreen;                              // Очистка цветового буфера OpenGL (самое простое стирание экрана)
    procedure SwapBuffers;                              // Смена буферов
    function MakeCurrent:Boolean;                       // Эта процедура устанавливает текущий контекст вывода на тот контекст, который был создан в этом объекте
    procedure EraseBackground(DC: HDC); override;
    function RGBAToColor(r,g,b,a:Byte):TColor;          // Формирование значения типа TColor через компоненты цвета

    procedure RectToGL(P:TPoint; var Gx,Gy:Double);     // Пересчёт точки из координат элемента управления ( поверхности на которой рисуется ось ) в координаты системы координат OpenGL
    function RectToGL(P:TPoint):TValue2f;               // -------- // ----------
    function RectToGL(x,y:Integer):TValue2f;            // -------- // ----------

    function GLToRect(Gx,Gy:Double):TPoint;             // Пересчёт точки из координат координатной системы OpenGL в координаты элемента управления
    function GLToRect(glpos:TValue2f):TPoint;           // -------- // -----------

    property LastErrorStr:string read FLastErrorStr;
    property GLContextInit:Boolean read FGLContextInit;

    property RGBA: boolean read FRGBA write SetRGBA default True;

  published  // Свойства, которые будут отображаться в инспекторе объектов
    property Align;
    property Anchors;
    property Enabled;
    property ClearColor:TColor read FClearColor write SetClearColor;                // Свойство цвета фона
    property Hint;

    property OpenGLMajorVersion: Cardinal read fOpenGLMajorVersion write SetOpenGLMajorVersion default 0;
    property OpenGLMinorVersion: Cardinal read fOpenGLMinorVersion write SetOpenGLMinorVersion default 0;

    { Number of samples per pixel, for OpenGL multi-sampling (anti-aliasing).

      Value <= 1 means that we use 1 sample per pixel, which means no anti-aliasing.
      Higher values mean anti-aliasing. Exactly which values are supported
      depends on GPU, common modern GPUs support values like 2 and 4.

      If this is > 1, and we will not be able to create OpenGL
      with multi-sampling, we will fallback to normal non-multi-sampled context.
      You can query OpenGL values GL_SAMPLE_BUFFERS_ARB and GL_SAMPLES_ARB
      (see ARB_multisample extension) to see how many samples have been
      actually allocated for your context. }
    property MultiSampling: Cardinal read FMultiSampling write SetMultiSampling default 1;

    property RedBits: Cardinal read FRedBits write SetRedBits default 8;
    property GreenBits: Cardinal read FGreenBits write SetGreenBits default 8;
    property BlueBits: Cardinal read FBlueBits write SetBlueBits default 8;
    property AlphaBits: Cardinal read FAlphaBits write SetAlphaBits default 0;
    property DepthBits: Cardinal read FDepthBits write SetDepthBits default 24;
    property StencilBits: Cardinal read FStencilBits write SetStencilBits default 0;
    property AUXBuffers: Cardinal read FAUXBuffers write SetAUXBuffers default 0;


    property ParentShowHint;
    property PopupMenu;

    property ShowHint;

    property Visible;

    property UseDefaultPaint:Boolean read FUseDefaultPaint write FUseDefaultPaint;


    // Свойства - события
    property OnAfterGLInit:TNotifyEvent read FOnAfterGLInit write FOnAfterGLInit; // Событие наступает, после инициализации OpenGL
    property OnBeforeGLDeinit:TNotifyEvent read FOnBeforeGLDeinit write FOnBeforeGLDeinit; // Событие наступает до уничтожения контекста OpenGL, в нём выполняются действия для удаления пользовательских объектов OpenGL
    property OnClick;
    property OnDblClick;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyUp;
    property OnKeyPress;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property OnResize;
    property OnShowHint;
    property OnContextPopup;
  end;

procedure Register;


implementation

procedure Register;
begin
  RegisterComponents('OtherComponents',[TSimpleGLContext]);
end;


{ TSimpleGLContext }

procedure TSimpleGLContext.DeinitGLContext;
begin
  if FGLContextInit then begin // только если контекст был создан
    if Assigned(FOnBeforeGLDeinit) then // Если есть обработчик на событие, то
      FOnBeforeGLDeinit(Self); // запускаем этот обработчик
    wglMakeCurrent(0, 0); // Делаем контекст None текущим (переключение на контекст по умолчанию)
    wglDeleteContext(Fhrc); // Удаление контекста OpenGL
    ReleaseDC(Handle, Fdc);
  end;
end;

procedure TSimpleGLContext.DefaultPaint;
begin
  MakeCurrent;

  glUseProgram(0);

  glMatrixMode(GL_PROJECTION);
	glLoadIdentity;
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
  glDisable(GL_DEPTH_TEST);

  glClear(GL_COLOR_BUFFER_BIT);
  glLineWidth(3);
  glColor3ub(255,0,0);
  glBegin(GL_LINES);
    glVertex2f(-1,-1);
    glVertex2f(1,1);
    glVertex2f(-1,1);
    glVertex2f(1,-1);
  glEnd;

  glBegin(GL_LINE_LOOP);
    glVertex2f(-1,-1);
    glVertex2f(-1,1);
    glVertex2f(1,1);
    glVertex2f(1,-1);
  glEnd;

  SwapBuffers;
end;

constructor TSimpleGLContext.Create(AOwner: TComponent);
begin
  FLastErrorStr:='';
  inherited Create(AOwner);
  Left:=2;
  Height:=2;
  Width:=100;
  Height:=100;
  Color:=0;

  fOpenGLMajorVersion:=0;
  fOpenGLMinorVersion:=0;
  FMultiSampling:=1;
  FRGBA:=True;
  FRedBits:=8;
  FGreenBits:=8;
  FBlueBits:=8;

  FGLContextInit:=False;
  FUseDefaultPaint:=True;  // Признак необходимости запуска отрисовки по умолчанию

  ControlStyle:=ControlStyle-[csSetCaption];
  if (csDesigning in ComponentState) then begin
    FCanvas := TControlCanvas.Create;
    TControlCanvas(FCanvas).Control := Self;
  end else
    FCompStyle:=csNonLCL;

  DoubleBuffered:=False;
end;

destructor TSimpleGLContext.Destroy;
begin
  FCanvas.Free;
  FCanvas:=nil;
  inherited Destroy;
end;

procedure TSimpleGLContext.ClearScreen;
begin
  glClear(GL_COLOR_BUFFER_BIT);
end;

procedure TSimpleGLContext.WMPaint(var Msg: TLMPaint);
begin
  Include(FControlState, csCustomPaint);
  inherited WMPaint(Msg);

  if (csDesigning in ComponentState) and (FCanvas<>nil) then
    with FCanvas do begin
      if Msg.DC <> 0 then Handle := Msg.DC;
      Brush.Color:=FClearColor;
      Pen.Color:=clRed;
      Rectangle(0,0,Self.Width,Self.Height);
      MoveTo(0,0);
      LineTo(Self.Width,Self.Height);
      MoveTo(0,Self.Height);
      LineTo(Self.Width,0);
      if Msg.DC <> 0 then Handle := 0;
    end
  else
    if IsVisible and HandleAllocated then
      DoOnPaint;
  Exclude(FControlState, csCustomPaint);
end;

procedure TSimpleGLContext.WMEraseBkgnd(var Message: TLMEraseBkgnd);
begin
  // Если нужно стереть задник, то ничего не делаем т.к. перед отрисовкой и так всё стирается
  // Если экран моргает при перестройке изображения можно попробовать отключить изпользование файла манифеста виндовс
end;

procedure TSimpleGLContext.OpenGLAttributesChanged;
begin
  {$Warning Нужно реализовать то, что закоментированно}
//  if HandleAllocated and ( ([csLoading, csDestroying]*ComponentState=[]) and IsOpenGLRenderAllowed ) then RecreateWnd(Self);
end;

procedure TSimpleGLContext.SetOpenGLMajorVersion(AValue: Cardinal);
begin
  if fOpenGLMajorVersion=AValue then Exit;
  fOpenGLMajorVersion:=AValue;
end;

procedure TSimpleGLContext.SetMultiSampling(AValue: Cardinal);
begin
  if FMultiSampling=AValue then Exit;
  FMultiSampling:=AValue;
  OpenGLAttributesChanged;
end;

procedure TSimpleGLContext.SetAlphaBits(AValue: Cardinal);
begin
  if FAlphaBits=AValue then Exit;
  FAlphaBits:=AValue;
  OpenGLAttributesChanged;
end;

procedure TSimpleGLContext.SetAUXBuffers(AValue: Cardinal);
begin
  if FAUXBuffers=AValue then Exit;
  FAUXBuffers:=AValue;
  OpenGLAttributesChanged;
end;

procedure TSimpleGLContext.SetDepthBits(AValue: Cardinal);
begin
  if FDepthBits=AValue then Exit;
  FDepthBits:=AValue;
  OpenGLAttributesChanged;
end;

procedure TSimpleGLContext.SetOpenGLMinorVersion(AValue: Cardinal);
begin
  if fOpenGLMinorVersion=AValue then Exit;
  fOpenGLMinorVersion:=AValue;
end;

procedure TSimpleGLContext.SetRGBA(AValue: boolean);
begin
  if FRGBA=AValue then Exit;
  FRGBA:=AValue;
  OpenGLAttributesChanged;
end;

procedure TSimpleGLContext.SetRedBits(AValue: Cardinal);
begin
  if FRedBits=AValue then Exit;
  FRedBits:=AValue;
  OpenGLAttributesChanged;
end;

procedure TSimpleGLContext.SetStencilBits(AValue: Cardinal);
begin
  if FStencilBits=AValue then Exit;
  FStencilBits:=AValue;
  OpenGLAttributesChanged;
end;

procedure TSimpleGLContext.SetGreenBits(AValue: Cardinal);
begin
  if FGreenBits=AValue then Exit;
  FGreenBits:=AValue;
  OpenGLAttributesChanged;
end;

procedure TSimpleGLContext.SetBlueBits(AValue: Cardinal);
begin
  if FBlueBits=AValue then Exit;
  FBlueBits:=AValue;
  OpenGLAttributesChanged;
end;

procedure TSimpleGLContext.Loaded;
begin
  inherited Loaded;

  // Альфа компонент цвета задаётся равным 255, т.к. в интерфейс выбора цвета в инспекторе объектов не позволяет выбрать альфа канал, а здась цвет может быть только после него
  SetClearColor(FClearColor or $FF000000);                // Установка цвета, которым будет стираться экран
end;

procedure TSimpleGLContext.SetClearColor(AValue: TColor);
var
  pv4b:PValue4b;
begin
  if AValue=FClearColor then Exit;                          // Если устанавливается тот же самый цвет, то делать нечего

  pv4b:=@AValue;
  FClearColor:=AValue;

  FClearColorGL.v1:=pv4b^.a/255.0;
  FClearColorGL.v2:=pv4b^.b/255.0;
  FClearColorGL.v3:=pv4b^.c/255.0;
  FClearColorGL.v4:=pv4b^.d/255.0;

  if csLoading in ComponentState then Exit;               // Если выполняется загрузка компонента, то делать тут нечего (на этом этапе формируются свойства из секции published)
  if csDesigning in ComponentState then Exit;             // Если компонент в состоянии дизайна формы, то ничего не делаем
  if not MakeCurrent then Exit;
  glClearColor(FClearColorGL.v1, FClearColorGL.v2, FClearColorGL.v3, FClearColorGL.v4);   // Установка цвета закраски экрана
end;

procedure TSimpleGLContext.CreateWnd;
begin
  inherited CreateWnd;

  if (csDesigning in ComponentState) then Exit;

  InitOpenGL();

  Fdc:=GetDC(Handle);
  if (fOpenGLMajorVersion<>0) and (OpenGLMinorVersion<>0) then
    Fhrc:=CreateRenderingContextVersion(Fdc, [opDoubleBuffered], fOpenGLMajorVersion, fOpenGLMinorVersion, True, 32,  FDepthBits, FStencilBits, 0, FAUXBuffers, 0)
  else
    Fhrc:=CreateRenderingContext(Fdc, [opDoubleBuffered], 32,  FDepthBits, FStencilBits, 0, FAUXBuffers, 0);
  FGLContextInit:=wglMakeCurrent(Fdc, Fhrc); // Делаем текущим созданый контекст
  if not FGLContextInit then Exception.Create('Конетекст OpenGL не инициализирован');
  ReadExtensions;
  InitGLContext; // Инициализация для потомков

  if Assigned(FOnAfterGLInit) then
    FOnAfterGLInit(self);
end;

procedure TSimpleGLContext.DestroyWnd;
begin
  if not (csDesigning in ComponentState) then
    DeinitGLContext; // Перед уничтожением окна удаляем контекст OpenGL
  inherited DestroyWnd;
end;

procedure TSimpleGLContext.DoOnResize; // Обновление области отображения для OpenGL
begin
  inherited DoOnResize;
  if csDesigning in ComponentState then Exit;
  if not wglMakeCurrent(Fdc, Fhrc) then Exit; // Делаем текущим созданый контекст
  glViewport(0,0, Width, Height);
end;

procedure TSimpleGLContext.DoOnPaint;
begin
  if Assigned(OnPaint) then begin
    if not wglMakeCurrent(Fdc, Fhrc) then Exit;
    FOnPaint(Self);
  end else begin
    if not wglMakeCurrent(Fdc, Fhrc) then Exit;
    if FUseDefaultPaint then DefaultPaint;
  end;
end;

procedure TSimpleGLContext.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  SetFocus; // Устанавливаем фокус ввода при нажатии кнопки мыши
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TSimpleGLContext.InitGLContext;
begin
  ///////
end;

procedure TSimpleGLContext.SwapBuffers;
begin
  Windows.SwapBuffers(Fdc);
end;

function TSimpleGLContext.MakeCurrent: Boolean;
begin
  Result:=wglMakeCurrent(Fdc, Fhrc); // Делаем текущим созданый контекст
end;

procedure TSimpleGLContext.EraseBackground(DC: HDC);
begin
  if DC=0 then ;
  // Высегда выполняется отрисовка, поэтому в стирании нет необходимости
end;

function TSimpleGLContext.RGBAToColor(r, g, b, a: Byte): TColor;
begin
  Result:=Value4b(r,g,b,a).UInt;
end;

procedure TSimpleGLContext.RectToGL(P: TPoint; var Gx, Gy: Double);
begin
	Gx:=2*(P.X-Width div 2)/Width;
	Gy:=2*(Height div 2-P.Y)/Height;
end;

function TSimpleGLContext.RectToGL(P: TPoint): TValue2f;
begin
	Result.X:=2*(P.X-Width div 2)/Width;
	Result.Y:=2*(Height div 2-P.Y)/Height;
end;

function TSimpleGLContext.RectToGL(x, y: Integer): TValue2f;
begin
	Result.X:=2*(x-Width div 2)/Width;
	Result.Y:=2*(Height div 2-y)/Height;
end;

function TSimpleGLContext.GLToRect(Gx, Gy: Double): TPoint;
begin
	Result.X:=Trunc(Width div 2*(Gx+1));
	Result.Y:=Trunc(Height div 2*(1-Gy));
end;

function TSimpleGLContext.GLToRect(glpos: TValue2f): TPoint;
begin
	Result.X:=Trunc(Width div 2*(glpos.X+1));
	Result.Y:=Trunc(Height div 2*(1-glpos.Y));
end;


end.




