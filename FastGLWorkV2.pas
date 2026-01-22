unit FastGLWorkV2;

// Это модуль для создания и отображения объектов при помощи OpenGL
// Компонент, который тут реализован предполагает, что контекст OpenGL уже создан и выбран активным
// В этом компоненте можно установить и управлять камерой, а так же управлять камерой с помощью мыши
// Можно хранить список шейдерных программ, а так же хранить список объектов, которым можно отрисовывать

{$mode objfpc}{$H+}

interface

uses
  Classes
  , SysUtils
  , ArrayOfTypes
  , Graph3D
  , MatrixWork
  , dglOpenGL
  , GLShaders
  , GLVBOWork
  ;

type
	PMaterial=^TMaterial;
	TMaterial=packed Record
		Ambient:TValue4f;
		Diffuse:TValue4f;
		Specular:TValue4f;
		Emission:TValue4f;
		Shininess:Single;
	end;

  TSpotLightPos=Record
    Pos:TValue4f;
    Direction:TValue3f;
  end;

	PLightInfo=^TLightInfo;
	TLightInfo=record
		Ray:TSpotLightPos;
		Ambient:TValue4f;
		Diffuse:TValue4f;
		Specular:TValue4f;
    Exponent:Single;
    CuttOff:Single;
    CAttenuation:Single;
    LAttenuation:Single;
    QAttenuation:Single;
	end;


  { TFastGLWorkV2 }

  TFastGLWorkV2 = class(TComponent)
  private
    FObjectList:TGLVBOList;           // Поле для хранения объектов, которые будут отрисовываться с помощью OpenGL
    FShaderProgList:TGLShadersList;   // Поле для хранения объектов-шейдерных программ
    FLightPos:array [0..7] of TSpotLightPos;
    FCamera:TCoordinateSystem;        // Координатная система, которая определяет положение и ориентацию камеры
    FSavedCamera:TCoordinateSystem;   // Объект, для хранения копии камеры (изпользуется при управлении камерой)
    FCameraChangeState:Boolean;       // Признак того, что началось изменение камеры
    FMouseX:Integer;                  // Поля для сохранения координат мыши для управления поворотом камеры
    FMouseY:Integer;
    FProjectionType:Integer;          // Тип проекции запоминается в процедуре SetMatProject и соответственно хрант тип проекции, которая была установлена
    FOrthoKof:Double;                 // Коэффициент для ортогональной проекции
    FWidth:Integer;                   // Ширина и высота, которые были переданы в функцию SetProjectMatByWH
    FHeight:Integer;
    FNearPlane:Single;                // Ближняя и дальняя плоскости отсечения
    FFarPlane:Single;
    FSpeed:Single;                    // Некая абстрактная скорость, которая зависит от размеров сцены и настраивается конкретно для каждой сцены

    procedure SaveCameraState; // Сохранение состояния камеры
    procedure LoadCamereState; // Загрузска состояния камеры
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ClearScene; // Очистка буфера кадра и буфера глубины
    procedure UpdateCameraView; // Процедура обновляет видовую матрицу
    procedure SetMatProject(LeftClip, RightClip, DownClip, UpClip, NearPlane, FarPlane: Single; pt:Integer=0);  // Процедура задаёт матрицу проекции
    procedure SetProjectMatByWH(w,h:Integer; kof,nearp,farp:Single; pt:Integer=0); // Формирование матрици проекции, изпользуя ширину и высоту области вывода OpenGL
    procedure KeyDown(Sender:TObject; var Key:Word; Shift: TShiftState); // Процедура вызывается для движения камеры с помощью клавиатуры
    procedure RotMoveCamera(Shift: TShiftState; x,y:Integer); // Поворот камеры (процедура вызывается в обработчике движения мыши)
    procedure RotMoveStop; // Эту процедуру необходимо вызывать обязательно, когда управление камерой с помощью мыши закончено
    procedure ZoomByWheel(wd:Integer; Shift: TShiftState); // Наезд камерой с изпользованием колеса мыши

    property ShaderProgs:TGLShadersList read FShaderProgList;
    property Objects:TGLVBOList read FObjectList;
    property Camera:TCoordinateSystem read FCamera;
    property Speed:Single read FSpeed write FSpeed;
  end;

procedure register;

implementation

procedure register;
begin
  RegisterComponents('OtherComponents',[TFastGLWorkV2]);
end;

{ TFastGLWorkV2 }

constructor TFastGLWorkV2.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FShaderProgList:=TGLShadersList.Create;
  FObjectList:=TGLVBOList.Create;
  FCamera:=TCoordinateSystem.Create;
  FSavedCamera:=TCoordinateSystem.Create;

  FMouseX:=-1; // Поля для сохранения координат мыши для управления поворотом камеры
  FMouseY:=-1;
  FCameraChangeState:=False;
  FOrthoKof:=1;
  FSpeed:=1;
end;

destructor TFastGLWorkV2.Destroy;
begin
  FObjectList.Free;
  FShaderProgList.Free;
  FCamera.Free;
  FSavedCamera.Free;

  inherited Destroy;
end;

procedure TFastGLWorkV2.ClearScene;
begin
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
end;

procedure TFastGLWorkV2.UpdateCameraView;
var
  mat:TValue16f;
begin
  // Видовая матрица по сути задаёт положение наблюдателя и направление взгляда и вся 3Д сцена пересчитывается в соответствии с этими параметрами
  // видовая матрица формируется таким образом, что ось X камеры является направлением просмотра
  // ось Y камеры является верхом, а ось Z камеры смотрит направо
  glMatrixMode(GL_MODELVIEW);
  mat:=LookAtMatrix(FCamera.P0, Add_Vector(FCamera.P0, FCamera.Px), FCamera.Py);
  glLoadMatrixf(PSingle(@mat)); // Загрузка новой видовой матрицы
end;

procedure TFastGLWorkV2.SetMatProject(LeftClip, RightClip, DownClip, UpClip, NearPlane, FarPlane: Single; pt: Integer);
begin
  // pt - тип проекции 0-перспективная проекция, 1-ортогональная проекция
  // Эта матрица задаёт правила проецирования 3Д сцены на двумерную плоскость экрана
  FProjectionType:=pt; // Информация о типе проекции изпользуется при движении камеры
  glMatrixMode(GL_PROJECTION);
	glLoadIdentity;
  if pt=0 then
    glFrustum(LeftClip, RightClip, DownClip, UpClip, NearPlane, FarPlane)
  else
    glOrtho(LeftClip, RightClip, DownClip, UpClip, NearPlane, FarPlane);

  // Сохраняем ближнюю и дальнюю плоскости отсечения (они будут изпользоваться в процедуре ZoomByWheel)
  FNearPlane:=NearPlane;
  FFarPlane:=FarPlane;
end;

procedure TFastGLWorkV2.SetProjectMatByWH(w, h: Integer; kof, nearp, farp: Single; pt: Integer);
begin
  // kof - чем меньше это значение тем менее перспективная проекция получается
  // для ортогональной проекции kof фактически означает масштабирование

  FWidth:=w;
  FHeight:=h;

  if pt=1 then FOrthoKof:=kof;

  SetMatProject(-w*kof, w*kof, -h*kof, h*kof, nearp, farp, pt);
end;

procedure TFastGLWorkV2.KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  step:Single;
begin
  if ssShift in Shift then
    step:=10
  else
    step:=0.5;

  case Key of
    87: // W - движение вперёд
        begin
          FCamera.Move(step, Value3f(Camera.Px.X, 0, FCamera.Px.Z));
          Key:=0;
        end;
    83: // S - движение назад
        begin
          FCamera.Move(-step, Value3f(Camera.Px.X, 0, FCamera.Px.Z));
          Key:=0;
        end;
    65: // A - движение влево
        begin
          FCamera.Move(-step, FCamera.Pz);
          Key:=0;
        end;
    68: // D - движение вправо
        begin
          FCamera.Move(step, FCamera.Pz);
          Key:=0;
        end;
    81: // Q - поворот налево
        begin
          FCamera.RotateAxis(-step*KRAD, Value3f(0,1,0));
          Key:=0;
        end;
    69: // E - поворот направо
        begin
          FCamera.RotateAxis(step*KRAD, Value3f(0,1,0));
          Key:=0;
        end;
    84: // T - поворот наверх
        begin
          FCamera.RotateAxis(-step*KRAD, FCamera.Pz);
          Key:=0;
        end;
    71: // G - поворот вниз
        begin
          FCamera.RotateAxis(step*KRAD, FCamera.Pz);
          Key:=0;
        end;
    82: // R - движение вверх
        begin
          FCamera.Move(step, Value3f(0,1,0));
          Key:=0;
        end;
    70: // F - движение вниз
        begin
          FCamera.Move(-step, Value3f(0,1,0));
          Key:=0;
        end;
  end;

  UpdateCameraView;
end;

procedure TFastGLWorkV2.RotMoveCamera(Shift: TShiftState; x, y: Integer);
var
  dx,dy,s:Single;
begin
  if [ssLeft, ssMiddle, ssRight]*Shift=[] then Exit;

  if not FCameraChangeState then // Если камера ещё не двигалась, то
    begin
      SaveCameraState; // Сохраняем состояние камеры
      FMouseX:=x;
      FMouseY:=y;
      FCameraChangeState:=True; // Выставляем признак того, что началось движение камеры
      Exit;
    end;

  LoadCamereState; // Загрузка того состяния при котором началось вращение камеры (чтобы камера всегда поворачивалась с того положения с которого НАЧАЛСЯ поворот)

  if ssShift in Shift then
    s:=1
  else
    s:=0.2;

  if ssCtrl in Shift then
    s:=0.01;

  dx:=(FMouseX-X)*s;
  dy:=(FMouseY-Y)*s;

  if ssLeft in Shift then begin // Если зажата левая клавиша мыши
    // то вращение выполняется вокруг своих собственных осей
    FCamera.RotateAxis(dx*KRAD, OneYVector3f);
    FCamera.RotateAxis(dy*KRAD, FCamera.Pz);
  end;

  if ssRight in Shift then begin // Если зажата правая клавиша мыши
    // то вращение выполняется вокруг начала координат
    FCamera.RotateSystem(dx*KRAD, OneYVector3f, ZeroVector3f);
    FCamera.RotateSystem(dy*KRAD, FCamera.Pz, ZeroVector3f);
  end;

  dx:=dx*FSpeed;
  dy:=dy*FSpeed;

  if ssMiddle in Shift then // Если зажата средняя клавиша мыши, то выполняется сдвиг
    begin
      if FProjectionType=1 then
        begin
          FCamera.Move(dx*FOrthoKof, FCamera.Pz);
          FCamera.Move(-dy*FOrthoKof, FCamera.Py);
        end
      else
        begin
          FCamera.Move(dx, FCamera.Pz);
          FCamera.Move(-dy, FCamera.Py);
        end
    end;

  UpdateCameraView;
end;

procedure TFastGLWorkV2.RotMoveStop; // Эту процедуру необходимо вызывать обязательно, когда управление камерой с помощью мыши закончено
begin // Остановка процесса вращения камеры
  FCameraChangeState:=False;
end;

procedure TFastGLWorkV2.ZoomByWheel(wd: Integer; Shift: TShiftState);
var
  step:Single;
begin
  if ssShift in Shift then
    step:=1
  else
    step:=0.1;

  if FProjectionType=0 then // Если проекция перспективная, то наезд выполняется через движение камеры
    begin
      if wd<0 then
        FCamera.Move(step, FCamera.Px)
      else
        FCamera.Move(-step, FCamera.Px);

      UpdateCameraView;
    end
  else  // Для ортогональной проекции наезд камеры это на самом деле масштабирование, поэтому приходится обновлять матрицу проекции
    begin
      if wd<0 then
        FOrthoKof:=FOrthoKof*0.95
      else
        FOrthoKof:=FOrthoKof*1.05;

      SetProjectMatByWH(FWidth, FHeight, FOrthoKof, FNearPlane, FFarPlane, 1);
    end;
end;

procedure TFastGLWorkV2.SaveCameraState; // Сохранение текущего состяния камеры
begin
  FSavedCamera.SystemPointAndNormals(FCamera.P0, FCamera.Px, FCamera.Py, FCamera.Pz);
end;

procedure TFastGLWorkV2.LoadCamereState; // загрузка сохранённого ранее состояния камеры
begin
  FCamera.SystemPointAndNormals(FSavedCamera.P0, FSavedCamera.Px, FSavedCamera.Py, FSavedCamera.Pz);
end;



end.
