program Timetable;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, datetimectrls, UMainForm, UDBConnection, UDirectoryForm, UMetadata,
  USQL, UFilter, UNotification, UEditCard, UTimeTable, UConflicts, UConverter;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TDBConnection, DBConnection);
  Application.Run;
end.

