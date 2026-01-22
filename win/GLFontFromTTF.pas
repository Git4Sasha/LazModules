unit GLFontFromTTF;

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

  TGLTTFFont=class
    private
      FFontListBase:Integer; // Начало списка для шрифта
      FFontInit:Boolean;
      FStrForOut:PByte; // Это указатель на строку, которая отображается на экране, но эта строка переформатированна на однобайтовй размер для символа
      FStrLen:Cardinal; // Для хранения длины строки
      FCharCount:Integer; // Количество символов
      FCharRect:TArrayOfValue4d;
      function GetCharRect(Index: Integer): TValue4d;
    public
      constructor Create;
      destructor Destroy; override;

      procedure LoadFont(filename:string; quality:Integer=2; xscale:Single=1.0; ffill:Boolean=True); // Процедура загружает шрифт из файла (quality - Качество отрисовки шрифтов (значения по умолчанию вполне хватает для нормального вида шрифтов))
      procedure UTF8ToOneByteCode(str: string); // Перевод строки из UTF8 в однобайтовый формат с 256-ю символами
      procedure TextOutGL(gx, gy, sx, sy: Single; s:string); overload; // Вывод текста в координатах OpenGL
      procedure TextOutGL(gx, gy, gz, ang, rx,ry,rz, sx, sy, sz: Single; s:string); overload; // Вывод текста в координатах OpenGL
      function TextWidth(str:string):Double; // Функция возвращает ширину текста

      property FontInit:Boolean read FFontInit;
      property CharCount:Integer read FCharCount;
      property CharRect[Index:Integer]:TValue4d read GetCharRect;
  end;

implementation

const
  ZERO_DOUBLE_RECT:TValue4d=(xmin:0; xmax:0; ymin:0; ymax:0);

{ TGLTTFFont }

procedure TessError(errn: GLenum); stdcall; // Эта функция вызывается в процессе триангуляции с помощью Glu только тогда, когда возникает ошибка триангуляции
var
  str:PChar;
begin
  str:=gluErrorString(errn);
//  Format('err=%d  str=%s',[errn, str]));
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

function TGLTTFFont.GetCharRect(Index: Integer): TValue4d;
begin
  if (Index<0)or(Index>=FCharCount) then
    GetCharRect:=ZERO_DOUBLE_RECT
  else
    GetCharRect:=FCharRect[Index];
end;

constructor TGLTTFFont.Create;
begin
  FFontListBase:=-1;
  FFontInit:=False;
  FStrLen:=1024; // Предельный размер строки
  FStrForOut:=AllocMem(FStrLen); // выделение памяти под строку
end;

destructor TGLTTFFont.Destroy;
begin
  if FFontInit then
    begin
      glDeleteLists(FFontListBase, 256); // Удаляем списки для начертания шрифта
      SetLength(FCharRect, 0);
    end;
  Freemem(FStrForOut);
  inherited Destroy;
end;

procedure TGLTTFFont.LoadFont(filename: string; quality: Integer; xscale: Single; ffill: Boolean);
var
  glf,glfq:TGlyph;
  i,j:Integer;
  ic,icp:Integer;
  chrw:Word;
  gluTess:PGLUtesselator; // Объект для выполнения триангуляции
  CrossVertex:TArrayOfValue3d; // Массив для точек, который получаются при пересечении
  ttfreader:TTrueTypeReader;
begin
  // Если параметр ffil=True, то контуры шрифтов будут заполнены, иначе будут только контуры шрифтов

  if not FileExists(filename) then
    raise Exception.Create('Не найден файл шрифтов');
  SetLength(FCharRect, 0);
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

  if FFontListBase<>0 then // Если списки есть, то
    glDeleteLists(FFontListBase, 256); // удаляем эти списки
  FFontListBase:=glGenLists(256); // Создаём 256 списков

  FCharCount:=0;
  for i:=0 to 15 do
    for j:=0 to 15 do
      begin
        if FCharCount<128 then
          chrw:=FCharCount
        else
          chrw:=1024+FCharCount-128;

        ttfreader.ReadGlyph(chrw, @glf); // Чтение символа
        ttfreader.UpQuality(@glf, @glfq, quality); // повышение качества символа

        glNewList(FFontListBase+FCharCount, GL_COMPILE); // Объявляем начало нового списка

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

        SetLength(FCharRect, FCharCount+1);
        FCharRect[FCharCount]:=glfq.Rect;

        ttfreader.FreeGlyph(@glf);
        ttfreader.FreeGlyph(@glfq);

        Inc(FCharCount);
      end;
  if ffill then gluDeleteTess(gluTess); // удаляем объект триангуляции, если он использовался

  ttfreader.Free;

  FFontInit:=True; // Шрифт был загружен
end;

procedure TGLTTFFont.UTF8ToOneByteCode(str: string); // Перевод строки из UTF8 в однобайтовый формат с 256-ю символами
var
  pb,bw:PByte;
  ffb:Integer;
begin
  pb:=PByte(@str[1]); // Получаем указатель на 1-й символ, переданной строки
  bw:=FStrForOut; // Выходная строка
  FStrLen:=0; // Длина выводимой строки будет определена в цикле
  while pb^<>0 do begin // пока нет 0-го кода (нулём заканчивается строка) выполняем нижеследующий код
    if pb^ and 128=0 then
      bw^:=pb^
    else begin
      ffb:=(pb^ and 31) shl 6;
      Inc(pb);
      ffb:=ffb+(pb^ and 63)-1024+128;
      bw^:=ffb;
    end;
    Inc(bw);
    Inc(pb);
    Inc(FStrLen);
  end;
end;

procedure TGLTTFFont.TextOutGL(gx, gy, sx, sy: Single; s: string); // Вывод текста в координатах OpenGL (на плоскости)
begin
  if not FFontInit then // Если шрифт не был проинициализирован (загружен), то
    Exit; // выходим
  if s='' then Exit; // Если строка пустая, то и отображать нечего
  UTF8ToOneByteCode(s); // Перекодировка строки к однобайтовому виду
  glPushMatrix; // Сохраняем текущие матрицы преобразования
  glListBase(FFontListBase);
  glTranslatef(gx,gy,0); // Установка матрицы сдвига
  glScalef(sx, sy, 1);   // Установка матрицы масштабирования
  glCallLists(FStrLen, GL_UNSIGNED_BYTE, FStrForOut);
  glPopMatrix;
end;

procedure TGLTTFFont.TextOutGL(gx, gy, gz, ang, rx, ry, rz, sx, sy, sz: Single; s: string); // Вывод текста в координатах OpenGL (в 3-х координатах)
begin
  if not FFontInit then // Если шрифт не был проинициализирован (загружен), то
    Exit; // выходим
  if s='' then Exit; // Если строка пустая, то и отображать нечего
  UTF8ToOneByteCode(s); // Перекодировка строки к однобайтовому виду
  glPushMatrix; // Сохраняем текущие матрицы преобразования
  glListBase(FFontListBase);
  glTranslatef(gx,gy,gz); // Установка матрицы сдвига
  glRotatef(ang, rx, ry, rz); // Угол поворота задаётся в градусах
  glScalef(sx, sy, sz);   // Установка матрицы масштабирования
  glCallLists(FStrLen, GL_UNSIGNED_BYTE, FStrForOut);
  glPopMatrix;
end;

function TGLTTFFont.TextWidth(str: string): Double; // Расчёт ширины строки
var
  i:Integer;
begin
  TextWidth:=0;
  UTF8ToOneByteCode(str); // Перекодировка строки к однобайтовому виду
  for i:=0 to FStrLen-1 do
    with FCharRect[FStrForOut[i]] do
      TextWidth:=TextWidth+(xmax-xmin);
end;

end.

