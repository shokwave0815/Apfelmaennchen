unit ULogicalCPUCount;

// http://wiki.lazarus.freepascal.org/Example_of_multi-threaded_application:_array_of_threads#1._Detect_number_of_cores_available.

{ Copyright (c) 2018 by Ugochukwu Mmaduekwe
  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.
}

interface

{$IFDEF FPC}
  {$IF DEFINED(WINDOWS)}
    uses Windows;
  {$ELSEIF DEFINED(LINUX) }
    {$linklib c}
    uses ctypes;
  {$ELSEIF DEFINED(DARWIN) OR DEFINED(FREEBSD)}
    uses SysUtils, SysCtl, Unix;
  {$ENDIF}
{$ENDIF FPC}

//returns number of cores: a computer with two hyperthreaded cores will report 4
function GetLogicalCPUCount: int32;

implementation

{$IFDEF FPC}
  {$IF DEFINED(LINUX)}
    const _SC_NPROCESSORS_ONLN = 83;
    function sysconf(i: cint): clong; cdecl; external Name 'sysconf';
  {$ENDIF LINUX}
{$ENDIF FPC}

function GetLogicalCPUCount: int32;
{$IFDEF FPC}
  {$IF DEFINED(WINDOWS)}
    var
      LIdx: Int32;
      LProcessAffinityMask, LSystemAffinityMask: DWORD_PTR;
      LMask: DWORD;
      LSystemInfo: SYSTEM_INFO;
  {$ELSEIF DEFINED(DARWIN) OR DEFINED(FREEBSD)}
    var
      Len: size_t;
  {$ENDIF}
{$ENDIF FPC}
begin
{$IFNDEF FPC}  // For Delphi
  Result := System.CPUCount;
{$ELSE}  // For FreePascal
  {$IF DEFINED(WINDOWS)}
    //returns total number of processors available to system including logical hyperthreaded processors
    if GetProcessAffinityMask(GetCurrentProcess, LProcessAffinityMask, LSystemAffinityMask) then
    begin
      Result := 0;
      for LIdx := 0 to 31 do
      begin
        LMask := DWORD(1) shl LIdx;
        if (LProcessAffinityMask and LMask) <> 0 then
        begin
          System.Inc(Result);
        end;
      end;
    end else
    begin  // can't get the affinity mask so we just report the total number of processors
      GetSystemInfo(LSystemInfo);
      Result := LSystemInfo.dwNumberOfProcessors;
    end;
  {$ELSEIF DEFINED(LINUX)}
    Result := sysconf(_SC_NPROCESSORS_ONLN);
  {$ELSEIF DEFINED(DARWIN) OR DEFINED(FREEBSD)}
    Len := SizeOf(Result);
    fpSysCtlbyname(PChar('hw.logicalcpu'), @Result, @Len, nil, 0);
  {$ELSE}
    Result := 1;  // Fallback for other platforms
  {$ENDIF}
{$ENDIF FPC}
  if Result < 1 then
    Result := 1;
end;

end.

