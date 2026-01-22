unit ShareMemAsFile;

{$mode objfpc}{$H+}

// Некоторые нюансы работы класса TShareMemAsFile:
// - процедура Dastroy не вызывает удаление общего ресурса, чтобы он не изчез из системы
// - для удаления существует специальная процедура DeleteShareMem, которая удаяет общий объект только текущей программы и для всех, новых программ
// - удалённый общий объект будет работать для всех программ, которые ещё не закрылись или не вызвали процедуру DeleteShareMem
// - подробнее см. процедуру DeleteShareMem в тексте модуля

interface

uses
  Classes
  , SysUtils
  , BaseUnix
  ;

type

  { TShareMemAsFile }

  TShareMemAsFile=class
  private
    FShareMemCreated:Boolean;
    FName:string;
    FSize:Cardinal;
    FAddr:Pointer;
    FId:LongInt;

  public
    destructor Destroy; override;

    procedure CreateShareMem(name:string; size:Cardinal);           // Создание общей памяти
    procedure CloseShareMem;                                        // Закрытие общей памяти
    procedure DeleteShareMem;                                       // Удаление общей памяти

    property Addr:Pointer read FAddr;
    property Size:Cardinal read FSize;
    property Name:string read FName;
  end;

implementation

const
  rtlib = 'rt';
  clib  = 'libc';

{$linklib rt}
{$linklib libc}

function shm_open(__name:Pchar; __oflag:longint; __mode:mode_t):longint;cdecl;external rtlib name 'shm_open';
function shm_unlink(__name:Pchar):longint;cdecl;external rtlib name 'shm_unlink';

function ftruncate(__fd:longint; __length:longint):longint;cdecl;external clib name 'ftruncate';
function mmap(__addr:pointer; __len:size_t; __prot:longint; __flags:longint; __fd:longint; __offset: longint):pointer;cdecl;external clib name 'mmap';
function munmap(__addr:pointer; __len:size_t):longint;cdecl;external clib name 'munmap';


{ TShareMemAsFile }

destructor TShareMemAsFile.Destroy;
begin
  CloseShareMem;
  inherited Destroy;
end;

// Создание объекта ошбщей памяти, если он был ранее создан другой программой, то объект общей памяти будет просто открыт.
// Первая программа, которая создаёт объект общей памяти получает общую память заполненую нулями
procedure TShareMemAsFile.CreateShareMem(name: string; size: Cardinal);
begin
  FSize:=size;
  FName:=name;

  Fid:=shm_open(PChar(FName), O_CREAT or O_RDWR, S_IRUSR or S_IWUSR);
  ftruncate(Fid, size);
  FAddr:=mmap(nil, FSize, PROT_READ or PROT_WRITE, MAP_SHARED, Fid, 0);
  fpclose(Fid);               // закрываем дискриптор

  FShareMemCreated:=True;         // Общий ресурс создан
end;

// Закрытие общей памяти, при этом не происходит её удаления, объект останется даже в том случае, елси программу закроют.
// После закрытия объекта, его можно снова создать процедурой CreateShareMem, данные при этом не потеряются
procedure TShareMemAsFile.CloseShareMem;
begin
  if FShareMemCreated then begin
    munmap(FAddr, FSize);
    FShareMemCreated:=False;    // Общая память закрыта

    FAddr:=nil;
    FSize:=0;
    FName:='';
  end;
end;

// Удаление общей памяти, после этого действия ни эта ни вновь запущенные программы не будут иметь возможность получить данные из общей памяти
// Те программы, которые не выполнили shm_unlink смогут ещё использовать общую память при этом, если другая программа создаст объект общей памяти
// с таким же и менем, что и был удалён, то это будет уже новый блок (при создании он будет заполнен нулями), к которому оставшиеся программы отношения
// иметь не будут. Получается, что один раз вызванная процедура DeleteShareMem удаляет общую память для всех вновь запускаемых программ, поэтому
// эту процедуру должна запускать только одна программа, которая закрывается последней (следить за этим нужно самому)
// Никаких встроеных иструментов для определения того, есть общая память или нет, нету. Более того если хоть одна программа вызовет DeleteShareMem, а потом
// любая другая вновь запущенная создаст (откроет) общую память с тем же именем, то часть программ будет работать с одной общей памятью, а часть с другой,
// не смотря на то, что имя общего ресурса одно.
procedure TShareMemAsFile.DeleteShareMem;
begin
  if FShareMemCreated then begin
    shm_unlink(PChar(FName));
    munmap(FAddr, FSize);

    FShareMemCreated:=False;        // Общая память не создана

    FAddr:=nil;
    FSize:=0;
    FName:='';
  end;
end;

end.

