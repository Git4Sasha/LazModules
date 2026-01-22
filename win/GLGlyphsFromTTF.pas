unit GLGlyphsFromTTF;

{$mode objfpc}{$H+}

interface

uses
    Classes
  , SysUtils
  , GL
  , GLU
  , ArrayOfTypes
  , TrueTypeReader
  ;

type
  { TGLTTFFont }

  { TGLTTFGlyphs }

  TGLTTFGlyphs=class
    private
      FGlyphsListBase:Integer; // Начало списка для контуров
      FGlyphsInit:Boolean;
      FGlyphsCount:Integer; // Количество контуров в шрифте
      FGlyphsRect:TArrayOfValue4d;
      function GetGlyphRect(Index: Integer): TValue4d;
    public
      constructor Create;
      destructor Destroy; override;

      procedure LoadGlyphs(filename:string; quality:Integer=2; xscale:Single=1.0; ffill:Boolean=True); // Процедура загружает контуры из файла
      procedure GlyphOutGL(gx, gy, sx, sy: Single; code:Integer); // Вывод контура в координатах OpenGL

      property GlyphsInit:Boolean read FGlyphsInit;
      property GlyphsCount:Integer read FGlyphsCount;
      property GlyphRect[Index:Integer]:TValue4d read GetGlyphRect;
  end;

implementation

const
  ZERO_DOUBLE_RECT:TValue4d=(xmin:0; xmax:0; ymin:0; ymax:0);

{ TGLTTFGlyphs }

procedure TessError(errn: GLenum); stdcall; // Эта функция вызывается в процессе триангуляции с помощью Glu только тогда, когда возникает ошибка триангуляции
var
  str:PChar;
begin
  str:=gluErrorString(errn);
end;

procedure TessCombineUserData(vrtx:PValue3d; vrtx_data:Pointer; weight:Pointer; out dataout:PDouble; usrData:Pointer); stdcall;
var
  i:Integer;
  mass:TArrayOfValue3d;
begin
  mass:=TArrayOfValue3d(usrData^);
  i:=Length(mass);
  SetLength(mass, i+1);
  mass[i]:=vrtx^;
  dataout:=@mass[i];
end;

function TGLTTFGlyphs.GetGlyphRect(Index: Integer): TValue4d;
begin
  if (Index<0)or(Index>=FGlyphsCount) then
    Result:=ZERO_DOUBLE_RECT
  else
    Result:=FGlyphsRect[Index];
end;

constructor TGLTTFGlyphs.Create;
begin
  FGlyphsListBase:=-1; // Начало списка для контуров
  FGlyphsInit:=False;
  FGlyphsCount:=-1; // Количество контуров в шрифте
end;

destructor TGLTTFGlyphs.Destroy;
begin
  if FGlyphsInit then
    begin
      glDeleteLists(FGlyphsListBase, FGlyphsCount); // Удаляем списки для начертания шрифта
      SetLength(FGlyphsRect, 0);
    end;
  inherited Destroy;
end;

procedure TGLTTFGlyphs.LoadGlyphs(filename: string; quality: Integer; xscale: Single; ffill: Boolean);
var
  glf,glfq:TGlyph;
  i:Integer;
  ic,icp:Integer;
  gluTess:PGLUtesselator; // Объект для выполнения триангуляции
  CrossVertex:TArrayOfValue3d; // Массив для точек, который получаются при пересечении
  ttfreader:TTrueTypeReader;
begin
  // Если параметр ffil=True, то контуры шрифтов будут заполнены, иначе будут только контуры шрифтов

  if not FileExists(filename) then
    raise Exception.Create('Не найден файл шрифтов');
  SetLength(FGlyphsRect, 0);
  ttfreader:=TTrueTypeReader.Create;
  ttfreader.OpenFont(filename);

  if ffill then // если необходимо заполнять шрифты, то выполняем необходимые действия для запуска триангуляции с помощью библиотеки GLU
    begin
      gluTess:=gluNewTess(); // Создаём объект для триангуляции

      gluTessCallback(gluTess, GLU_TESS_COMBINE_DATA, _GLUfuncptr(@TessCombineUserData));
      gluTessCallback(gluTess, GLU_TESS_VERTEX, _GLUfuncptr(glVertex3dv));
      gluTessCallback(gluTess, GLU_TESS_BEGIN, _GLUfuncptr(glBegin));
      gluTessCallback(gluTess, GLU_TESS_END, _GLUfuncptr(glEnd));
      gluTessCallback(gluTess, GLU_TESS_ERROR, _GLUfuncptr(@TessError));

      gluTessProperty(gluTess, GLU_TESS_WINDING_RULE, GLU_TESS_WINDING_NONZERO);
      gluTessProperty(gluTess, GLU_TESS_BOUNDARY_ONLY, GL_FALSE);
    end;

  if FGlyphsInit then // Если инициализация уже была, то
    glDeleteLists(FGlyphsListBase, FGlyphsCount); // удаляем эти списки
  FGlyphsCount:=ttfreader.NumGlyphs; // Кол-во контуров в файле шрифте
  FGlyphsListBase:=glGenLists(FGlyphsCount); // Создаём нужное количество списков
  SetLength(FGlyphsRect, FGlyphsCount);
  for i:=0 to FGlyphsCount-1 do
    begin
      ttfreader.ReadGlyph(i, @glf, False); // Чтение контура
      ttfreader.UpQuality(@glf, @glfq, quality); // повышение качества контура

      glNewList(FGlyphsListBase+i, GL_COMPILE); // Объявляем начало нового списка

      if ffill then
        begin
          gluTessBeginPolygon(gluTess, @CrossVertex); // Начинаем новый полигон

            for ic:=0 to glfq.ccnt-1 do // Цикл по контурам
              begin
                gluTessBeginContour(gluTess); // Начинаем новый контур
                for icp:=0 to  glfq.circs[ic].pcnt-1 do
                  gluTessVertex(gluTess, @glfq.circs[ic].pts[icp], @glfq.circs[ic].pts[icp]);
                gluTessEndContour(gluTess);
              end;

          gluTessEndPolygon(gluTess);

          SetLength(CrossVertex, 0); // Освобождение памяти от точек пересечения
        end
      else
        for ic:=0 to glfq.ccnt-1 do // Цикл по контурам
          begin
            glBegin(GL_LINE_LOOP); // Начинаем новый контур
            for icp:=0 to  glfq.circs[ic].pcnt-1 do
              glVertex2dv(@glfq.circs[ic].pts[icp]);
            glEnd;
          end;

      glTranslated(glfq.Rect.xmax*xscale, 0, 0); // После отрисовки символа выполняется сдвиг, чтобы следующий символ рисовался уже на новом месте

    glEndList;

    FGlyphsRect[i]:=glfq.Rect;

    ttfreader.FreeGlyph(@glf);
    ttfreader.FreeGlyph(@glfq);
  end;
  if ffill then gluDeleteTess(gluTess); // удаляем объект триангуляции, если он использовался

  ttfreader.Free;

  FGlyphsInit:=True; // контуры были загружены
end;

procedure TGLTTFGlyphs.GlyphOutGL(gx, gy, sx, sy: Single; code: Integer);
begin
  if not FGlyphsInit then // Если контуры не были проинициализированы (загружены), то
    Exit; // выходим

  glPushMatrix; // Сохраняем текущие матрицы преобразования
  glTranslatef(gx,gy,0); // Установка матрицы сдвига
  glScalef(sx, sy, 1);   // Установка матрицы масштабирования
  glCallList(FGlyphsListBase + code);
  glPopMatrix;
end;

end.

