program Timetable;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, UMainForm, UDBConnection, UDirectoryForm, UMetadata, USQL, UFilter,
  UNotification, UEditCard, UTimeTable
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TDBConnection, DBConnection);
  //Application.CreateForm(TDirectoryForm, DirectoryForm);
  //Application.CreateForm(TEditCard, EditCard);
  //Application.CreateForm(TTimetableForm, TimetableForm);
  Application.Run;
end.

