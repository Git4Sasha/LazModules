unit FastBPF;

interface

uses
  Complex, ArrayOfTypes;

type
  TFastBPF=class
    private
      FTabSinCos:TArrayOfValue2f; // Массив синусов и косинусов
      FChangeIndex:TArrayOfInteger; // Масиив хранящий номера элементов массива данных для перестановки
      FNfft:Integer; // База БПФ
      FINfft:Single; // Коэффициент равный 1/FNfft, изпользуется в операции обратного преобразования Фурье
      FPow:Integer; // Степень двойки, при заданной базе БПФ
      FCntChngM:Integer; // Количество элементов массива FChangeIndex

      function MirrorBits(a, base:Integer):Integer; // Расчёт номера ячейки, которая соответствует перевёрнутым битам в номере ячейке
    public
      destructor Destroy; override;
      procedure CreateSupportMass(Nfft:Integer);          // Функция формирует вспомогательные массивы
      procedure BPF(Data: PValue2f);                      // Прямое преобразование Фурье
      procedure IBPF(Data: PValue2f);                     // Обратное преобразование Фурье

      property Nfft:Integer read FNfft;                   // База БПФ
  end;



implementation


{ TFastBPF }

procedure TFastBPF.BPF(Data: PValue2f);
var
	lp,lp2,lp3:Integer;
	i,j,k,ks,l:Integer;
	V,V1:TValue2f;
  m1,m2:PValue2f;
  admchn:^Integer;
begin
  admchn:=@FChangeIndex[0];
	for i:=0 to FCntChngM-1 do
		begin
			j:=admchn^;
      Inc(admchn);
      l:=admchn^;
      Inc(admchn);

      m1:=Data;
      m2:=m1;
      Inc(m1,j);
      Inc(m2,l);

			V:=m1^;
      m1^:=m2^;
      m2^:=v;
		end;

	for i:=1 to FPow do // Цикл по степени базы БПФ
		begin
      lp:=1 shl i;
      lp2:=FNfft shr i;
      lp3:=lp shr 1;
      ks:=FNfft div lp3;
      l:=0;
			for j:=0 to lp3-1 do
				begin
          V:=FTabSinCos[l];
          l:=l+ks;

          m1:=@Data[j];
          m2:=@Data[j+lp3];
          k:=lp2;
          repeat
              v1.Re:=m2^.Re*v.Re - m2^.Im*v.Im;
              v1.Im:=m2^.Re*v.Im + m2^.Im*v.Re;

              m2^.Re:=m1^.Re - V1.Re;
              m2^.Im:=m1^.Im - V1.Im;

              m1^.Re:=m1^.Re + V1.Re;
              m1^.Im:=m1^.Im + V1.Im;

              Inc(m1, lp);
              Inc(m2, lp);

              Dec(k);
          until k=0;
				end;
		end;
end;


procedure TFastBPF.CreateSupportMass(Nfft: Integer);
var
  i,j:Integer;
  al:Single;
begin
  FNfft:=Nfft;
  FINfft:=1/Nfft;

  // Формирование таблицы синусов и косинусов
  SetLength(FTabSinCos, Nfft);
  for i:=0 to Nfft-1 do
    begin
      al:=-pi*i*FINfft;
      FTabSinCos[i].Re:=Cos(al);
      FTabSinCos[i].Im:=Sin(al);
    end;

	FPow:=0;
	repeat
		Inc(FPow);
	until Nfft shr FPow=1;

  // Формирование таблицы для перестановки данных
  SetLength(FChangeIndex, Nfft*2);
  FCntChngM:=0;
	for i:=1 to Nfft-1 do
		begin
      j:=MirrorBits(i, FPow);
      if i>=j then
        continue;
      FChangeIndex[FCntChngM*2]:=i;
      FChangeIndex[FCntChngM*2+1]:=j;
      Inc(FCntChngM);
		end;
  SetLength(FChangeIndex, FCntChngM*2);
end;

destructor TFastBPF.Destroy;
begin
  FTabSinCos:=nil;
  FChangeIndex:=nil;
  inherited;
end;

procedure TFastBPF.IBPF(Data: PValue2f);
var
	lp,lp2,lp3:Integer;
	i,j,k,ks,l:Integer;
	V,V1:TValue2f;
  m1,m2:PValue2f;
  admchn:^Integer;
begin
  m1:=Data;
	for i:=0 to FNfft-1 do begin
		m1^.Re:=m1^.Re*FINfft;
		m1^.Im:=m1^.Im*FINfft;
    Inc(m1);
	end;

  admchn:=@FChangeIndex[0];
	for i:=0 to FCntChngM-1 do begin
		j:=admchn^;
    Inc(admchn);
    l:=admchn^;
    Inc(admchn);

    m1:=Data;
    m2:=m1;
    Inc(m1,j);
    Inc(m2,l);

		V:=m1^;
    m1^:=m2^;
    m2^:=v;
	end;

	for i:=1 to FPow do begin // Цикл по степени базы БПФ
		lp:=1 shl i;
		lp2:=(FNfft div lp);
		lp3:=lp div 2;
    ks:=FNfft div lp3;
    l:=0;
		for j:=0 to lp3-1 do begin
      V:=FTabSinCos[l];
      l:=l+ks;

      m1:=@Data[j];
      m2:=@Data[j+lp3];
      k:=lp2;
      repeat
        v1.Re:=m2^.Re*v.Re + m2^.Im*v.Im;
        v1.Im:=m2^.Im*v.Re - m2^.Re*v.Im;

        m2^.Re:=m1^.Re - V1.Re;
        m2^.Im:=m1^.Im - V1.Im;

        m1^.Re:=m1^.Re + V1.Re;
        m1^.Im:=m1^.Im + V1.Im;

        Inc(m1, lp);
        Inc(m2, lp);

        Dec(k);
      until k=0;
		end;
	end;
end;

function TFastBPF.MirrorBits(a, Base: Integer): Integer; // Расчёт номера ячейки, которая соответствует перевёрнутым битам в номере ячейке
begin  // a - число, биты которого необходимо перевернуть; Base - Степень двойки, для которой выполняется переворот
  Result:=0;
  Dec(base);
  repeat
    Result:=Result or (a and 1);
    Result:=Result shl 1;
    a:=a shr 1;
    Dec(base);
  until base=0;
  Result:=Result or (a and 1);
end;

end.
