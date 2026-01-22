{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Mattias Gaertner

  Abstract:
    TOpenGLControl is a LCL control with an opengl context.
    It works under the following platforms:
      - gtk with glx    : full
      - gtk2 with glx   : full
      - carbon with agl : full
      - cocoa           : no
      - windows with wgl: full
      - wince           : no
      - qt with glx     : no (started)
      - fpgui with glx  : no
      - nogui           : no
}
unit OpenGLContext;

{$mode objfpc}{$H+}

// choose the right backend depending on used LCL widgetset
{$IFDEF LCLGTK}
  {$IFDEF Linux}
    {$DEFINE UseGtkGLX}
    {$DEFINE HasRGBA}
    {$DEFINE HasRGBBits}
    {$DEFINE OpenGLTargetDefined}
  {$ENDIF}
{$ENDIF}
{$IFDEF LCLGTK2}
  {$IF defined(Linux) or defined(FreeBSD)}
    {$DEFINE UseGtk2GLX}
    {$DEFINE UsesModernGL}
    {$DEFINE HasRGBA}
    {$DEFINE HasRGBBits}
    {$DEFINE HasDebugContext}
    {$DEFINE OpenGLTargetDefined}
  {$ENDIF}
{$ENDIF}
{$IFDEF LCLGTK3}
  {$IF defined(Linux) or defined(FreeBSD)}
    {$DEFINE UseGtk3GLX}
    {$DEFINE UsesModernGL}
    {$DEFINE HasRGBA}
    {$DEFINE HasRGBBits}
    {$DEFINE HasDebugContext}
    {$DEFINE OpenGLTargetDefined}
  {$ENDIF}
{$ENDIF}
{$IFDEF LCLCarbon}
  {$DEFINE UseCarbonAGL}
  {$DEFINE HasRGBA}
  {$DEFINE HasRGBBits}
  {$DEFINE OpenGLTargetDefined}
{$ENDIF}
{$IFDEF LCLCocoa}
  {$DEFINE UseCocoaNS}
  {$DEFINE UsesModernGL}
  {$DEFINE OpenGLTargetDefined}
  {$DEFINE HasMacRetinaMode}
{$ENDIF}
{$IFDEF LCLWin32}
  {$DEFINE UseWin32WGL}
  {$DEFINE HasRGBA}
  {$DEFINE HasRGBBits}
  {$DEFINE HasDebugContext}
  {$DEFINE OpenGLTargetDefined}
{$ENDIF}
{$IFDEF LCLQT}
  {$DEFINE UseQTGLX}
  {$DEFINE UsesModernGL}
  {$DEFINE HasRGBA}
  {$DEFINE HasRGBBits}
  {$DEFINE OpenGLTargetDefined}
{$ENDIF}
{$IFDEF LCLQT5}
  {$DEFINE UseQTGLX}
  {$DEFINE UsesModernGL}
  {$DEFINE HasRGBA}
  {$DEFINE HasRGBBits}
  {$DEFINE OpenGLTargetDefined}
{$ENDIF}
{$IFNDEF OpenGLTargetDefined}
  {$ERROR this LCL widgetset/OS is not yet supported}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  // LCL
  LCLType, LCLIntf, LResources, Forms, Controls, Graphics, LMessages,
  WSLCLClasses, WSControls, dglOpenGL, ArrayOfTypes,
{$IFDEF UseGtkGLX}
  GLGtkGlxContext;
{$ENDIF}
{$IFDEF UseGtk2GLX}
  glgtkglxcontext;
{$ENDIF}
{$IFDEF UseGtk3GLX}
  GLGtk3GlxContext;
{$ENDIF}
{$IFDEF UseCarbonAGL}
  GLCarbonAGLContext;
{$ENDIF}
{$IFDEF UseCocoaNS}
  GLCocoaNSContext;
{$ENDIF}
{$IFDEF UseWin32WGL}
  GLWin32WGLContext;
{$ENDIF}
{$IFDEF UseQTGLX}
  GLQTContext;
{$ENDIF}

const
  DefaultDepthBits = 24;

type
  TOpenGlCtrlMakeCurrentEvent = procedure(Sender: TObject; var Allow: boolean) of object;
  TOpenGLControlOption = (ocoMacRetinaMode, ocoRenderAtDesignTime);
  TOpenGLControlOptions = set of TOpenGLControlOption;

  { TSimpleGLContextCustom }
  { Sharing:
    You can share opengl contexts. For example:
    Assume OpenGLControl2 and OpenGLControl3 should share the same as
    OpenGLControl1. Then set

        OpenGLControl2.SharedControl:=OpenGLControl1;
        OpenGLControl3.SharedControl:=OpenGLControl1;

     After this OpenGLControl1.SharingControlCount will be two and
     OpenGLControl1.SharingControls will contain OpenGLControl2 and
     OpenGLControl3.
    }

  TSimpleGLContextCustom = class(TWinControl)
  private
    FCanvas: TCanvas;                         // Изпользуется только на этапе создания формы
    FOnAfterGLInit:TNotifyEvent;              // Объект - процедура для обслуживания свойства OnAfterGLInit
    FOnBeforeGLDeinit:TNotifyEvent;           // Объект - процедура для обслуживания свойства OnBeforeGLDeinit
    FDebugContext: boolean;
    FOnMakeCurrent: TOpenGlCtrlMakeCurrentEvent;
    FOnPaint: TNotifyEvent;
    fOpenGLMajorVersion: Cardinal;
    fOpenGLMinorVersion: Cardinal;
    FRGBA: boolean;
    {$IFDEF HasRGBBits}
    FRedBits, FGreenBits, FBlueBits,
    {$ENDIF}
    FMultiSampling, FAlphaBits, FDepthBits, FStencilBits, FAUXBuffers: Cardinal;
    FSharedOpenGLControl: TSimpleGLContextCustom;
    FSharingOpenGlControls: TList;
    FOptions: TOpenGLControlOptions;
    FGLContextInit:Boolean;                             // Признак того, что контекст OpenGL создан
    FUseDefaultPaint:Boolean;                           // Признак необходимости запуска отрисовки по умолчанию

    function GetSharingControls(Index: integer): TSimpleGLContextCustom;
    procedure SetDebugContext(AValue: boolean);
    procedure SetOpenGLMajorVersion(AValue: Cardinal);
    procedure SetOpenGLMinorVersion(AValue: Cardinal);
    procedure SetOptions(AValue: TOpenGLControlOptions);
    procedure SetRGBA(const AValue: boolean);
    {$IFDEF HasRGBBits}
    procedure SetRedBits(const AValue: Cardinal);
    procedure SetGreenBits(const AValue: Cardinal);
    procedure SetBlueBits(const AValue: Cardinal);
    {$ENDIF}
    procedure SetMultiSampling(const AMultiSampling: Cardinal);
    procedure SetAlphaBits(const AValue: Cardinal);
    procedure SetDepthBits(const AValue: Cardinal);
    procedure SetStencilBits(const AValue: Cardinal);
    procedure SetAUXBuffers(const AValue: Cardinal);
    procedure SetSharedControl(const AValue: TSimpleGLContextCustom);
    function IsOpenGLRenderAllowed: boolean;
  protected
    FClearColor:    TColor;                                             // Цвет фона
    FClearColorGL:  TValue4f;                                           // Цвет фона в удобном для некоторых случаев виде

    class procedure WSRegisterClass; override;

    procedure Loaded; override;                                         // Процедура запускается, когда все свойства установились в те значения, которые указанные в инспекторе объектов
    procedure SetClearColor(AValue: TColor); virtual;                   // Процедура обслуживает свойство ClearColor

    procedure InitGLContext;    virtual;                                 // в этой процедуре можно выполнять код, кторый требует, уже созданного контекста OpenGL
    procedure DeinitGLContext;  virtual;                                 // В этой процедуре удаляются объекты OpenGL, до удаления контекста (реализуется наследниками объекта)
    procedure DefaultPaint;     virtual;                                 // Эта процедура запускается, когда не назначен обработчик для события OnPaint

    procedure WMShowWindow(var Msg: TLMShowWindow) message LM_SHOWWINDOW;
    procedure WMPaint(var Message: TLMPaint); message LM_PAINT;
    procedure WMSize(var Message: TLMSize); message LM_SIZE;
    procedure WMBeforeDestroyHandle(var Message: TLMessage); message LM_USER;
    procedure CMDoubleBufferedChanged(var Message: TLMessage); message CM_DOUBLEBUFFEREDCHANGED;

    procedure OpenGLAttributesChanged;

    property OnAfterGLInit:TNotifyEvent read FOnAfterGLInit write FOnAfterGLInit;           // Событие наступает, после инициализации OpenGL
    property OnBeforeGLDeinit:TNotifyEvent read FOnBeforeGLDeinit write FOnBeforeGLDeinit;  // Событие наступает перед уничтожением контекста OpenGL
    property UseDefaultPaint:Boolean read FUseDefaultPaint write FUseDefaultPaint;          // Признак изпользования процедуры отрисовки по умолчанию в том случае, если нет обработчика OnPaint
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure Paint; virtual;
    procedure RealizeBounds; override;
    procedure DoOnPaint; virtual;
    procedure SwapBuffers; virtual;
    function MakeCurrent(SaveOldToStack: Boolean = False): Boolean; virtual;
    procedure ClearScreen;                                                                  // Очистка цветового буфера OpenGL (самое простое стирание экрана)
    function ReleaseContext: boolean; virtual;
    function RestoreOldOpenGLControl: boolean;
    function SharingControlCount: integer;
    procedure Invalidate; override;
    procedure EraseBackground(DC: HDC); override;
    function RGBAToColor(r,g,b,a:Byte):TColor;          // Формирование значения типа TColor через компоненты цвета

    procedure RectToGL(P:TPoint; var Gx,Gy:Double);     // Пересчёт точки из координат элемента управления ( поверхности на которой рисуется ось ) в координаты системы координат OpenGL
    function RectToGL(P:TPoint):TValue2f;               // -------- // ----------
    function RectToGL(x,y:Integer):TValue2f;            // -------- // ----------

    function GLToRect(Gx,Gy:Double):TPoint;             // Пересчёт точки из координат координатной системы OpenGL в координаты элемента управления
    function GLToRect(glpos:TValue2f):TPoint;           // -------- // -----------

    property SharingControls[Index: integer]: TSimpleGLContextCustom read GetSharingControls;
    property GLContextInit:Boolean read FGLContextInit;
    property ClearColor:TColor read FClearColor write SetClearColor;                // Свойство цвета фона
    property OnMakeCurrent: TOpenGlCtrlMakeCurrentEvent read FOnMakeCurrent write FOnMakeCurrent;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property SharedControl: TSimpleGLContextCustom read FSharedOpenGLControl write SetSharedControl;
    property DoubleBuffered stored True default True;
    property ParentDoubleBuffered default False;
    property DebugContext: boolean read FDebugContext write SetDebugContext default false; // create context with debugging enabled. Requires OpenGLMajorVersion!
    property RGBA: boolean read FRGBA write SetRGBA default true;
    {$IFDEF HasRGBBits}
    property RedBits: Cardinal read FRedBits write SetRedBits default 8;
    property GreenBits: Cardinal read FGreenBits write SetGreenBits default 8;
    property BlueBits: Cardinal read FBlueBits write SetBlueBits default 8;
    {$ENDIF}
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

    property AlphaBits: Cardinal read FAlphaBits write SetAlphaBits default 0;
    property DepthBits: Cardinal read FDepthBits write SetDepthBits default DefaultDepthBits;
    property StencilBits: Cardinal read FStencilBits write SetStencilBits default 0;
    property AUXBuffers: Cardinal read FAUXBuffers write SetAUXBuffers default 0;
    property Options: TOpenGLControlOptions read FOptions write SetOptions;
  end;

  { TSimpleGLContext }

  TSimpleGLContext = class(TSimpleGLContextCustom)
  published
    property Align;
    property Anchors;
    property BorderSpacing;
    property ClearColor;
    property Enabled;
    {$IFDEF HasRGBBits}
    property RedBits;
    property GreenBits;
    property BlueBits;
    {$ENDIF}
    property OpenGLMajorVersion;
    property OpenGLMinorVersion;
    property ParentShowHint;
    property MultiSampling;
    property AlphaBits;
    property DepthBits;
    property StencilBits;
    property AUXBuffers;
    property UseDefaultPaint;

    property OnAfterGLInit;                     // Событие наступает, после инициализации OpenGL
    property OnBeforeGLDeinit;                  // Событие наступает перед уничтожением контекста OpenGL
    property OnChangeBounds;
    property OnClick;
    property OnConstrainedResize;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMakeCurrent;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnPaint;
    property OnResize;
    property OnShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
  end;

  { TWSOpenGLControl }

  TWSOpenGLControl = class(TWSWinControl)
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): HWND; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
    class function GetDoubleBuffered(const AWinControl: TWinControl): Boolean; override;
  end;



procedure Register;


implementation

var
  OpenGLControlStack: TList = nil;

procedure Register;
begin
  RegisterComponents('OtherComponents',[TSimpleGLContext]);
end;

{ TSimpleGLContextCustom }

function TSimpleGLContextCustom.GetSharingControls(Index: integer): TSimpleGLContextCustom;
begin
  Result:=TSimpleGLContextCustom(FSharingOpenGlControls[Index]);
end;

procedure TSimpleGLContextCustom.SetDebugContext(AValue: boolean);
begin
  if FDebugContext=AValue then Exit;
  FDebugContext:=AValue;
  OpenGLAttributesChanged;
end;

procedure TSimpleGLContextCustom.CMDoubleBufferedChanged(var Message: TLMessage);
begin
  inherited;
  OpenGLAttributesChanged;
end;

procedure TSimpleGLContextCustom.WMBeforeDestroyHandle(var Message: TLMessage);
begin
  if csDesigning in ComponentState then begin
  end else begin
    if not LOpenGLMakeCurrent(Handle) then Exit;
    if Assigned(FOnBeforeGLDeinit) then FOnBeforeGLDeinit(Self);
    DeinitGLContext;
    LOpenGLReleaseContext(0);
  end;
end;

procedure TSimpleGLContextCustom.SetOpenGLMajorVersion(AValue: Cardinal);
begin
  if fOpenGLMajorVersion=AValue then Exit;
  fOpenGLMajorVersion:=AValue;
end;

procedure TSimpleGLContextCustom.SetOpenGLMinorVersion(AValue: Cardinal);
begin
  if fOpenGLMinorVersion=AValue then Exit;
  fOpenGLMinorVersion:=AValue;
end;

procedure TSimpleGLContextCustom.SetOptions(AValue: TOpenGLControlOptions);
var
  RemovedRenderAtDesignTime: boolean;
begin
  if FOptions=AValue then Exit;

  RemovedRenderAtDesignTime:=
         (ocoRenderAtDesignTime in FOptions) and
    (not (ocoRenderAtDesignTime in AValue));

  FOptions:=AValue;

  { if you remove the flag ocoRenderAtDesignTime at design-time,
    we need to destroy the handle. The call to OpenGLAttributesChanged
    would not do this, so do it explicitly by calling ReCreateWnd
    (ReCreateWnd will destroy handle, and not create new one,
    since IsOpenGLRenderAllowed = false). }
  if (csDesigning in ComponentState) and
     RemovedRenderAtDesignTime and
     HandleAllocated then
    ReCreateWnd(Self);

  OpenGLAttributesChanged();
end;

procedure TSimpleGLContextCustom.SetRGBA(const AValue: boolean);
begin
  if FRGBA=AValue then exit;
  FRGBA:=AValue;
  OpenGLAttributesChanged;
end;

{$IFDEF HasRGBBits}
procedure TSimpleGLContextCustom.SetRedBits(const AValue: Cardinal);
begin
  if FRedBits=AValue then exit;
  FRedBits:=AValue;
  OpenGLAttributesChanged;
end;

procedure TSimpleGLContextCustom.SetGreenBits(const AValue: Cardinal);
begin
  if FGreenBits=AValue then exit;
  FGreenBits:=AValue;
  OpenGLAttributesChanged;
end;

procedure TSimpleGLContextCustom.SetBlueBits(const AValue: Cardinal);
begin
  if FBlueBits=AValue then exit;
  FBlueBits:=AValue;
  OpenGLAttributesChanged;
end;
{$ENDIF}

procedure TSimpleGLContextCustom.SetMultiSampling(const AMultiSampling: Cardinal);
begin
  if FMultiSampling=AMultiSampling then exit;
  FMultiSampling:=AMultiSampling;
  OpenGLAttributesChanged;
end;

procedure TSimpleGLContextCustom.SetAlphaBits(const AValue: Cardinal);
begin
  if FAlphaBits=AValue then exit;
  FAlphaBits:=AValue;
  OpenGLAttributesChanged;
end;

procedure TSimpleGLContextCustom.SetDepthBits(const AValue: Cardinal);
begin
  if FDepthBits=AValue then exit;
  FDepthBits:=AValue;
  OpenGLAttributesChanged;
end;

procedure TSimpleGLContextCustom.SetStencilBits(const AValue: Cardinal);
begin
  if FStencilBits=AValue then exit;
  FStencilBits:=AValue;
  OpenGLAttributesChanged;
end;

procedure TSimpleGLContextCustom.SetAUXBuffers(const AValue: Cardinal);
begin
  if FAUXBuffers=AValue then exit;
  FAUXBuffers:=AValue;
  OpenGLAttributesChanged;
end;

procedure TSimpleGLContextCustom.SetSharedControl(const AValue: TSimpleGLContextCustom);
begin
  if FSharedOpenGLControl=AValue then exit;
  if AValue=Self then
    Raise Exception.Create('A control can not be shared by itself.');
  // unshare old
  if (AValue<>nil) and (AValue.SharedControl<>nil) then
    Raise Exception.Create('Target control is sharing too. A sharing control can not be shared.');
  if FSharedOpenGLControl<>nil then
    FSharedOpenGLControl.FSharingOpenGlControls.Remove(Self);
  // share new
  if (AValue<>nil) and (csDestroying in AValue.ComponentState) then
    FSharedOpenGLControl:=nil
  else begin
    FSharedOpenGLControl:=AValue;
    if (FSharedOpenGLControl<>nil) then begin
      if FSharedOpenGLControl.FSharingOpenGlControls=nil then
        FSharedOpenGLControl.FSharingOpenGlControls:=TList.Create;
      FSharedOpenGLControl.FSharingOpenGlControls.Add(Self);
    end;
  end;
  // recreate handle if needed
  if HandleAllocated and IsOpenGLRenderAllowed then
    ReCreateWnd(Self);
end;

{ OpenGL rendering allowed, because not in design-mode or because we
  should render even in design-mode. }
function TSimpleGLContextCustom.IsOpenGLRenderAllowed: boolean;
begin
  Result := (not (csDesigning in ComponentState)) or
    (ocoRenderAtDesignTime in Options);
end;

procedure TSimpleGLContextCustom.SetClearColor(AValue: TColor);
var
  pv4b:PValue4b;
begin
//  WriteLn();

  if AValue=FClearColor then Exit;                          // Если устанавливается тот же самый цвет, то делать нечего
  FClearColor:=AValue;

  pv4b:=@AValue;
  FClearColorGL.v1:=pv4b^.a/255.0;
  FClearColorGL.v2:=pv4b^.b/255.0;
  FClearColorGL.v3:=pv4b^.c/255.0;
  FClearColorGL.v4:=pv4b^.d/255.0;

  if csLoading in ComponentState then Exit;               // Если выполняется загрузка компонента, то делать тут нечего (на этом этапе формируются свойства из секции published)
  if csDesigning in ComponentState then Exit;             // Если компонент в состоянии дизайна формы, то ничего не делаем
  if not LOpenGLMakeCurrent(Handle) then Exit;            // Если не удалось сделать контекст текущим, то входим

  glClearColor(FClearColorGL.v1, FClearColorGL.v2, FClearColorGL.v3, FClearColorGL.v4);   // Установка цвета закраски экрана
end;

class procedure TSimpleGLContextCustom.WSRegisterClass;
const
  Registered : Boolean = False;
begin
  if Registered then Exit;
  inherited WSRegisterClass;
  RegisterWSComponent(TSimpleGLContextCustom, TWSOpenGLControl);
  Registered := True;
end;

procedure TSimpleGLContextCustom.Loaded;
begin
  inherited Loaded;

  // Альфа компонент цвета задаётся равным 255, т.к. в интерфейс выбора цвета в инспекторе объектов не позволяет выбрать альфа канал, а здась цвет может быть только после него
  SetClearColor(FClearColor or $FF000000);                // Установка цвета, которым будет стираться экран
end;

procedure TSimpleGLContextCustom.InitGLContext;
begin
  // Актуальная реализация будет у потомков
end;

procedure TSimpleGLContextCustom.DeinitGLContext;
begin
 // Актуальная реализация будет у потомков
end;

procedure TSimpleGLContextCustom.DefaultPaint;
begin
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

procedure TSimpleGLContextCustom.WMShowWindow(var Msg: TLMShowWindow);
begin
  if (csDesigning in ComponentState) then Exit;
  if not LOpenGLMakeCurrent(Handle) then Exit;

  if not FGLContextInit then begin                          // Если не было пользовательской инициализации, то
    InitGLContext;                                          // запуск инициализации для потомков
    FGLContextInit:=True;
    glClearColor(FClearColorGL.v1, FClearColorGL.v2, FClearColorGL.v3, FClearColorGL.v4);   // Установка цвета закраски экрана
    if Assigned(FOnAfterGLInit) then FOnAfterGLInit(Self);  // Событие, которое будет возникать после инициализации
  end;
end;

procedure TSimpleGLContextCustom.WMPaint(var Message: TLMPaint);
begin
  Include(FControlState, csCustomPaint);
  inherited WMPaint(Message);
  //debugln('TCustomGTKGLAreaControl.WMPaint A ',dbgsName(Self),' ',dbgsName(FCanvas));
  if (not IsOpenGLRenderAllowed) and (FCanvas<>nil) then begin
    with FCanvas do begin
      if Message.DC <> 0 then Handle := Message.DC;

      Brush.Color:=FClearColor;
      Pen.Color:=clRed;
      Rectangle(0,0,Self.Width,Self.Height);
      MoveTo(0,0);
      LineTo(Self.Width,Self.Height);
      MoveTo(0,Self.Height);
      LineTo(Self.Width,0);

      if Message.DC <> 0 then Handle := 0;
    end;
  end else begin
    Paint;
  end;
  Exclude(FControlState, csCustomPaint);
end;

procedure TSimpleGLContextCustom.WMSize(var Message: TLMSize);
begin
  if (Message.SizeType and Size_SourceIsInterface)>0 then DoOnResize;
end;

procedure TSimpleGLContextCustom.OpenGLAttributesChanged;
begin
  if HandleAllocated and ( ([csLoading,csDestroying]*ComponentState=[]) and IsOpenGLRenderAllowed ) then RecreateWnd(Self);
end;

procedure TSimpleGLContextCustom.EraseBackground(DC: HDC);
begin
  if DC=0 then ;
  // everything is painted, so erasing the background is not needed
end;

function TSimpleGLContextCustom.RGBAToColor(r, g, b, a: Byte): TColor;
begin
  Result:=Value4b(r,g,b,a).UInt;
end;

procedure TSimpleGLContextCustom.RectToGL(P: TPoint; var Gx, Gy: Double);
begin
	Gx:=2*(P.X-Width div 2)/Width;
	Gy:=2*(Height div 2-P.Y)/Height;
end;

function TSimpleGLContextCustom.RectToGL(P: TPoint): TValue2f;
begin
	Result.X:=2*(P.X-Width div 2)/Width;
	Result.Y:=2*(Height div 2-P.Y)/Height;
end;

function TSimpleGLContextCustom.RectToGL(x, y: Integer): TValue2f;
begin
	Result.X:=2*(x-Width div 2)/Width;
	Result.Y:=2*(Height div 2-y)/Height;
end;

function TSimpleGLContextCustom.GLToRect(Gx, Gy: Double): TPoint;
begin
	Result.X:=Trunc(Width div 2*(Gx+1));
	Result.Y:=Trunc(Height div 2*(1-Gy));
end;

function TSimpleGLContextCustom.GLToRect(glpos: TValue2f): TPoint;
begin
	Result.X:=Trunc(Width div 2*(glpos.X+1));
	Result.Y:=Trunc(Height div 2*(1-glpos.Y));
end;

constructor TSimpleGLContextCustom.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FGLContextInit:=False;            // Контекст ещё не инициализирован
  FUseDefaultPaint:=True;           // Признак необходимости запуска отрисовки по умолчанию
  FClearColor:=0;                   // Инициализация цвета закраски экрана (в процессе загрузки формы цвет будет задан тем значением, которое указано в инспекторе объектов)

  ParentDoubleBuffered:=False;
  FDoubleBuffered:=true;
  FRGBA:=true;
  {$IFDEF HasRGBBits}
  FRedBits:=8;
  FGreenBits:=8;
  FBlueBits:=8;
  {$ENDIF}
  fOpenGLMajorVersion:=0;
  fOpenGLMinorVersion:=0;
  FMultiSampling:=1;
  FDepthBits:=DefaultDepthBits;
  ControlStyle:=ControlStyle-[csSetCaption];
  if not IsOpenGLRenderAllowed then begin
    FCanvas := TControlCanvas.Create;
    TControlCanvas(FCanvas).Control := Self;
  end else
    FCompStyle:=csNonLCL;
  SetInitialBounds(0, 0, 160, 90);
end;

destructor TSimpleGLContextCustom.Destroy;
begin
  if FSharingOpenGlControls<>nil then begin
    while SharingControlCount>0 do
      SharingControls[SharingControlCount-1].SharedControl:=nil;
    FreeAndNil(FSharingOpenGlControls);
  end;
  SharedControl:=nil;
  if OpenGLControlStack<>nil then begin
    OpenGLControlStack.Remove(Self);
    if OpenGLControlStack.Count=0 then
      FreeAndNil(OpenGLControlStack);
  end;
  FCanvas.Free;
  FCanvas:=nil;
  inherited Destroy;
end;

procedure TSimpleGLContextCustom.Paint;
begin
  if IsVisible and HandleAllocated then DoOnPaint;
end;

procedure TSimpleGLContextCustom.RealizeBounds;
begin
  if IsVisible and HandleAllocated and IsOpenGLRenderAllowed and ([csDestroying]*ComponentState=[]) then begin
    if MakeCurrent then glViewport(0,0,Width,Height);
  end;
  inherited RealizeBounds;
end;

procedure TSimpleGLContextCustom.DoOnPaint;
begin
  if Assigned(OnPaint) then begin
    if not MakeCurrent then exit;
    OnPaint(Self);
  end else begin
    if not MakeCurrent then exit;
    if FUseDefaultPaint then DefaultPaint;
  end;
end;

procedure TSimpleGLContextCustom.SwapBuffers;
begin
  LOpenGLSwapBuffers(Handle);
end;

function TSimpleGLContextCustom.MakeCurrent(SaveOldToStack: Boolean): Boolean;
var
  Allowed: Boolean;
begin
  if not IsOpenGLRenderAllowed then Exit(False);
  if Assigned(FOnMakeCurrent) then begin
    Allowed:=True;
    OnMakeCurrent(Self, Allowed);
    if not Allowed then begin
      Result:=False;
      Exit;
    end;
  end;
  // make current
  Result:=LOpenGLMakeCurrent(Handle);
  if Result and SaveOldToStack then begin
    // on success push on stack
    if OpenGLControlStack=nil then
      OpenGLControlStack:=TList.Create;
    OpenGLControlStack.Add(Self);
  end;
end;

procedure TSimpleGLContextCustom.ClearScreen;     // Очистка цветового буфера OpenGL (самое простое стирание экрана)
begin
  glClear(GL_COLOR_BUFFER_BIT);
end;

function TSimpleGLContextCustom.ReleaseContext: boolean;
begin
  Result:=false;
  if not HandleAllocated then exit;
  Result:=LOpenGLReleaseContext(Handle);
end;

function TSimpleGLContextCustom.RestoreOldOpenGLControl: boolean;
var
  RestoredControl: TSimpleGLContextCustom;
begin
  Result:=false;
  // check if the current context is on stack
  if (OpenGLControlStack=nil) or (OpenGLControlStack.Count=0) then exit;
  // pop
  OpenGLControlStack.Delete(OpenGLControlStack.Count-1);
  // make old control the current control
  if OpenGLControlStack.Count>0 then begin
    RestoredControl:=
      TSimpleGLContextCustom(OpenGLControlStack[OpenGLControlStack.Count-1]);
    if (not LOpenGLMakeCurrent(RestoredControl.Handle)) then
      exit;
  end else begin
    FreeAndNil(OpenGLControlStack);
  end;
  Result:=true;
end;

function TSimpleGLContextCustom.SharingControlCount: integer;
begin
  if FSharingOpenGlControls=nil then
    Result:=0
  else
    Result:=FSharingOpenGlControls.Count;
end;

procedure TSimpleGLContextCustom.Invalidate;
begin
  if csCustomPaint in FControlState then exit;
  inherited Invalidate;
end;

{ TWSOpenGLControl }

class function TWSOpenGLControl.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  OpenGlControl: TSimpleGLContextCustom;
  AttrControl: TSimpleGLContextCustom;
begin
  OpenGlControl:=AWinControl as TSimpleGLContextCustom;
  if not OpenGlControl.IsOpenGLRenderAllowed then
  begin
    // do not use "inherited CreateHandle", because the LCL changes the hierarchy at run time
    Result:=TWSWinControlClass(ClassParent).CreateHandle(AWinControl,AParams);
  end
  else
  begin
    if OpenGlControl.SharedControl<>nil then
      AttrControl:=OpenGlControl.SharedControl
    else
      AttrControl:=OpenGlControl;

    InitOpenGL();       // Динамическая загрузка функций OpenGL из библиотеки

    Result:=LOpenGLCreateContext(OpenGlControl,WSPrivate,
                                 OpenGlControl.SharedControl,
                                 AttrControl.DoubleBuffered,
                                 {$IFDEF HasMacRetinaMode}
                                 ocoMacRetinaMode in OpenGlControl.Options,
                                 {$ENDIF}
                                 {$IFDEF HasRGBA}
                                 AttrControl.RGBA,
                                 {$ENDIF}
                                 {$IFDEF HasDebugContext}
                                 AttrControl.DebugContext,
                                 {$ENDIF}
                                 {$IFDEF HasRGBBits}
                                 AttrControl.RedBits,
                                 AttrControl.GreenBits,
                                 AttrControl.BlueBits,
                                 {$ENDIF}
                                 {$IFDEF UsesModernGL}
                                 AttrControl.OpenGLMajorVersion,
                                 AttrControl.OpenGLMinorVersion,
                                 {$ENDIF}
                                 AttrControl.MultiSampling,
                                 AttrControl.AlphaBits,
                                 AttrControl.DepthBits,
                                 AttrControl.StencilBits,
                                 AttrControl.AUXBuffers,
                                 AParams);
    ReadExtensions;     // Динамическая загрузка расширенных функций OpenGL из библиотеки
  end;
end;

class procedure TWSOpenGLControl.DestroyHandle(const AWinControl: TWinControl);
begin
  SendMessage(AWinControl.Handle, LM_USER, 0, 0);

  LOpenGLDestroyContextInfo(AWinControl);
  // do not use "inherited DestroyHandle", because the LCL changes the hierarchy at run time
  TWSWinControlClass(ClassParent).DestroyHandle(AWinControl);
end;

class function TWSOpenGLControl.GetDoubleBuffered(const AWinControl: TWinControl): Boolean;
begin
  Result := False;
  if AWinControl=nil then ;
end;
{~bk
initialization
  RegisterWSComponent(TSimpleGLContextCustom,TWSOpenGLControl);
}

end.
