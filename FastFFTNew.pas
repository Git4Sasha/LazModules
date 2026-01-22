unit FastFFT;

{$mode objfpc}{$H+}

interface

// Это модуль-интерфейс для библиотеки fftw3. Эта библиотека выполняет быстрое преобразование фурье изпользуя аппаратные возможности процессора

uses
  dynlibs, sysutils, ArrayOfTypes;

type
  Tfftw_dir=(fftw_forward=-1,fftw_backward=1);
  Tfftw_flag=(fftw_measure,            {generated optimized algorithm}
             fftw_destroy_input,      {default}
             fftw_unaligned,          {data is unaligned}
             fftw_conserve_memory,    {needs no explanation}
             fftw_exhaustive,         {search optimal algorithm}
             fftw_preserve_input,     {don't overwrite input}
             fftw_patient,            {generate highly optimized alg.}
             fftw_estimate);          {don't optimize, just use an alg.}
  Tfftw_flagset=set of Tfftw_flag;


  { TFastFFT }

  TFastFFT=class
  private
    FPlan:Pointer;
    FMassIn:TArrayOfValue2f;
    FMassOut:TArrayOfValue2f;
    FNfft:Integer;

    procedure DeletePlan;
  public
    constructor Create;
    destructor Destroy; override;

    procedure CreateSupportMass(nbpf:Integer; dir:Tfftw_dir; flag:Tfftw_flagset=[fftw_measure]; twomass:Boolean=False);
    procedure CalcFFT;

    property MassIn:TArrayOfValue2f read FMassIn;
    property MassOut:TArrayOfValue2f read FMassOut;
    property Nfft:Integer read FNfft;
  end;


function GetTwoPowerHigth(val:Integer):Integer; // Получение числа, являющего степенью 2-ки ближайшего с большей стороны
function GetTwoPowerLow(val:Integer):Integer;   // Получение числа, являющего степенью 2-ки ближайшего с меньшей стороны

implementation

{$IFDEF Unix}
  const
    fftwlib = 'libfftw3f.so.3';
{$ELSE}
  const
    fftwlib = 'libfftw3f.dll';
{$ENDIF}

type
  Tfftw_plan_dft_1d=function(n:cardinal;i,o:PValue2f; sign:Tfftw_dir;flags:Tfftw_flagset):Pointer;  cdecl;  // Перед тем как выполнять разсчёт БПФа необходимо создать "план"
  Tfftw_execute=procedure(plan:Pointer);      cdecl;   // Если "план" создан, то можно выполнять операцию БПФ
  Tfftw_destroy_plan=procedure(plan:Pointer); cdecl;   // По окончании работы с "планом" его необходимо удалить

var
  libhandle: TLibHandle=NilHandle;
  fftw_plan_dft_1d:Tfftw_plan_dft_1d;
  fftw_execute:Tfftw_execute;
  fftw_destroy_plan:Tfftw_destroy_plan;

function GetTwoPowerHigth(val: Integer): Integer; // Получение числа, являющего степенью 2-ки ближайшего с большей стороны
begin
  Result:=1;
  while (Result<val) do Result:=Result shl 1;
end;

function GetTwoPowerLow(val: Integer): Integer; // Получение числа, являющего степенью 2-ки ближайшего с меньшей стороны
begin
  Result:=1;
  while (Result<val) do Result:=Result shl 1;
  if Result<>val then Result:=Result shr 1;
end;

{ TFastFFT }

procedure TFastFFT.DeletePlan;
begin
  if FPlan<>nil then begin
    fftw_destroy_plan(FPlan);
    FPlan:=nil;
  end;
end;

constructor TFastFFT.Create;
begin
  if libhandle<>NilHandle then Exit; // Если библиотека уже была инициализированна, то выходим ничего не делая

  libhandle:=LoadLibrary(PChar(fftwlib));

  if libhandle=NilHandle then
    Exception.Create(Format('Библиотека fftw3f не загружается Ошибка:  %s', [dynlibs.GetLoadErrorStr]));

  fftw_plan_dft_1d:=Tfftw_plan_dft_1d(GetProcAddress(libhandle, 'fftwf_plan_dft_1d'));
  fftw_execute:=Tfftw_execute(GetProcAddress(libhandle, 'fftwf_execute'));
  fftw_destroy_plan:=Tfftw_destroy_plan(GetProcAddress(libhandle, 'fftwf_destroy_plan'));
end;

destructor TFastFFT.Destroy;
begin
  inherited;
  DeletePlan;
end;

procedure TFastFFT.CreateSupportMass(nbpf: Integer; dir: Tfftw_dir; flag: Tfftw_flagset; twomass: Boolean);
begin
  // nfft - База БПФ
  // dir - Направление БПФ (fftw_forward - Прямой БПФ; fftw_backward - Обратный БПФ)
  // flag - Этот параметр определяет способ формирования плана, по которому выполняется быстрое преобразование фурье (см. Tfftw_flag)
  // twomass - Этот параметр определяет будет изпользоваться 2 массива при преобразовании (входной и выходной) или будет изпользоваться одни массив (входной он же выходной)

  DeletePlan; // Сначала удаляется текущий план (если его не было, то ничего не произойдёт)

  FNfft:=nbpf;
  SetLength(FMassIn, FNfft);  // Выделение памяти для массива входных данных

  if twomass then begin       // Если изпользуются 2 массива, то данные нужно записывать в FMassIn, а считывать из FMassOut
    SetLength(FMassOut, FNfft);
    FPlan:=fftw_plan_dft_1d(FNfft, @FMassIn[0], @FMassOut[0], dir, flag);
  end else begin              // Если изпользуется 1 массив, то данные нужно записывать в FMassIn и считывать из FMassIn
    FPlan:=fftw_plan_dft_1d(FNfft, @FMassIn[0], @FMassIn[0], dir, flag);
  end;
end;

procedure TFastFFT.CalcFFT;
begin
  fftw_execute(FPlan);
end;

finalization
  if libhandle=NilHandle then Exit;
  FreeLibrary(libhandle);
  libhandle:=NilHandle;


end.

