unit FastGLWork;

// Это модуль для создания и отображения объектов при помощи OpenGL
// Компонент, который тут реализован предполагает, что контекст OpenGL уже создан и выбран активным

{$mode objfpc}{$H+}

interface

uses
  Classes
  , SysUtils
  , dglOpenGL
  , ArrayOfTypes
  , GLObjects
  , Graph3D
  , MatrixWork
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


  { TFastGLWork }

  TFastGLWork = class(TComponent)
  private
    FObjectList:TGLSimpleObjectList; // Поле для хранения объектов, которые будут отрисовываться с помощью OpenGL
    FCurentTexture:Integer;  // Поле определяет индекс текстуры, которая является текущей
    FLightPos:array [0..7] of TSpotLightPos;
    FCamera:TCoordinateSystem; // Координатная ситема, которая определяет положение и ориентацию камеры

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ClearScene; // Очистка буфера кадра и буфера глубины
    procedure ShowObject(Index:Integer; TypeGeometric:Cardinal;  VecCount:Integer=-1);

    procedure UpdateCameraView; // Процедура задаёт видовую матрицу
    procedure SetMatProject(LeftClip, RightClip, DownClip, UpClip, NearPlane, FarPlane: Single; pt:Integer=0);  // Процедура задаёт матрицу проекции

    property Objects:TGLSimpleObjectList read FObjectList;
    property Camera:TCoordinateSystem read FCamera;
  published

  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('OtherComponents',[TFastGLWork]);
end;

{ TFastGLWork }

constructor TFastGLWork.Create(AOwner: TComponent);
begin
  inherited;
  FObjectList:=TGLSimpleObjectList.Create;
  FCamera:=TCoordinateSystem.Create;
end;

destructor TFastGLWork.Destroy;
begin
  FObjectList.Free;
end;

procedure TFastGLWork.ClearScene;
begin
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
end;

procedure TFastGLWork.ShowObject(Index: Integer; TypeGeometric: Cardinal; VecCount:Integer=-1);
var
	WorkObject:TGLSimpleObject;
  vc:Integer;
begin
  // VecCount - Количество отображаемых вершин

  if (Index<0)or(Index>=FObjectList.Count) then
		Exit;

	WorkObject:=FObjectList[Index];
  if not WorkObject.Visible then // Если запрещено отображать объект, то выходим из процедуры
    Exit;
  if WorkObject.VectorCount=0 then
		Exit;

  if VecCount=-1 then
    vc:=WorkObject.VectorCount
  else
    vc:=VecCount;
  if vc>WorkObject.VectorCount then
    vc:=WorkObject.VectorCount;

	case 	WorkObject.UsesTypeVector of
		tv2D:   			begin
									  glVertexPointer(2, GL_FLOAT, SizeOf(TValue2f), WorkObject.MassPointer);
								  end;
		tv2D_Col:			begin
									  glVertexPointer(2, GL_FLOAT, SizeOf(TValue2f4b), WorkObject.MassPointer);
									  glColorPointer(4, GL_UNSIGNED_BYTE, SizeOf(TValue2f4b), WorkObject.MassPointer+8);
									  glEnableClientState(GL_COLOR_ARRAY);
								  end;
		tv2D_Tex:			begin
										glVertexPointer(2, GL_FLOAT, SizeOf(TValue4f), WorkObject.MassPointer);
										glTexCoordPointer(2, GL_FLOAT, SizeOf(TValue4f), WorkObject.MassPointer+8);
										glEnableClientState(GL_TEXTURE_COORD_ARRAY);
  									glEnable(GL_TEXTURE_2D);
										glBindTexture(GL_TEXTURE_2D, FCurentTexture);
									end;
    tv3D:         begin
                    glVertexPointer(3, GL_FLOAT, SizeOf(TValue3f), WorkObject.MassPointer);
                  end;
    tv3D_Col:			begin
										glVertexPointer(3, GL_FLOAT, SizeOf(TValue3f4b), WorkObject.MassPointer);
										glColorPointer(4, GL_UNSIGNED_BYTE, SizeOf(TValue3f4b), WorkObject.MassPointer+12);
										glEnableClientState(GL_COLOR_ARRAY);
									end;
		tv3D_Mat:			begin
										glVertexPointer(3, GL_FLOAT, SizeOf(TValue6f), WorkObject.MassPointer);
										glNormalPointer(GL_FLOAT, SizeOf(TValue6f), WorkObject.MassPointer+12);
										glEnableClientState(GL_NORMAL_ARRAY);
									end;
		tv3D_Tex:			begin
										glVertexPointer(3, GL_FLOAT, SizeOf(TValue5f), WorkObject.MassPointer);
										glTexCoordPointer(2, GL_FLOAT, SizeOf(TValue5f), WorkObject.MassPointer+12);
										glEnableClientState(GL_TEXTURE_COORD_ARRAY);
										glEnable(GL_TEXTURE_2D);
										glBindTexture(GL_TEXTURE_2D, FCurentTexture);
									end;
		tv3D_Mat_Tex:	begin
										glVertexPointer(3, GL_FLOAT, SizeOf(TValue8f), WorkObject.MassPointer);
										glNormalPointer(GL_FLOAT, SizeOf(TValue8f), WorkObject.MassPointer+12);
										glTexCoordPointer(2, GL_FLOAT, SizeOf(TValue8f), WorkObject.MassPointer+24);
										glEnableClientState(GL_TEXTURE_COORD_ARRAY);
										glEnableClientState(GL_NORMAL_ARRAY);
										glEnable(GL_TEXTURE_2D);
										glBindTexture(GL_TEXTURE_2D, FCurentTexture);
									end;
	end;

	glEnableClientState(GL_VERTEX_ARRAY);

	if WorkObject.UseObjectMatrix then
		begin
			glPushMatrix;
			glMultMatrixf(@WorkObject.CurentMatrix);
			glDrawArrays(TypeGeometric, 0, vc);
			glPopMatrix;
		end
	else
		glDrawArrays(TypeGeometric, 0, vc);

	glDisableClientState(GL_VERTEX_ARRAY);

	case 	WorkObject.UsesTypeVector of
		tv2D_Col, tv3D_Col: glDisableClientState(GL_COLOR_ARRAY);

		tv2D_Tex, tv3D_Tex: begin
												  glDisableClientState(GL_TEXTURE_COORD_ARRAY);
												  glDisable(GL_TEXTURE_2D);
											  end;

    tv3D_Mat:			      glDisableClientState(GL_NORMAL_ARRAY);

		tv3D_Mat_Tex:	      begin
													glDisableClientState(GL_NORMAL_ARRAY);
													glDisableClientState(GL_TEXTURE_COORD_ARRAY);
													glDisable(GL_TEXTURE_2D);
												end;
	end;
end;

procedure TFastGLWork.UpdateCameraView;
var
  i:Integer;
  mat:TValue16f;
begin
  // Видовая матрица по сути задаёт положение наблюдателя и направление взгляда и вся 3Д сцена пересчитывается в соответствии с этими параметрами
  // видовая матрица формируется таким образом, что ось X камеры является направлением просмотра
  // ось Y камеры является верхом, а ось Z камеры смотрит направо
  glMatrixMode(GL_MODELVIEW);
  mat:=LookAtMatrix(FCamera.P0, Add_Vector(FCamera.P0, FCamera.Px), FCamera.Py);
  glLoadMatrixf(PSingle(@mat));
  for i:=0 to 7 do
    begin
    	glLightfv(GL_LIGHT0+i, GL_POSITION, @FLightPos[i].Pos);
    	glLightfv(GL_LIGHT0+i, GL_SPOT_DIRECTION, @FLightPos[i].Direction);
    end;
end;

procedure TFastGLWork.SetMatProject(LeftClip, RightClip, DownClip, UpClip, NearPlane, FarPlane: Single; pt: Integer);
begin
  // pt - тип проекции 0-перспективная проекция, 1-ортогональная проекция
  // Эта матрица задаёт правила проецирования 3Д сцены на двумерную плоскость экрана
  glMatrixMode(GL_PROJECTION);
	glLoadIdentity;
  if pt=0 then
    glFrustum(LeftClip, RightClip, DownClip, UpClip, NearPlane, FarPlane)
  else
    glOrtho(LeftClip, RightClip, DownClip, UpClip, NearPlane, FarPlane)
end;


end.
