program epanet;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, FrameViewer09, tachartlazaruspkg, lazcontrols, tachartbgra, main,
  restclient, about, config, epanet2, epanetmsx, ShpAPI, proj, utils, mapthemes,
  themelegend, themepalette, results, simulator, chartoptions, configeditor,
  controlseditor, curveeditor, demandseditor, editor, labeleditor,
  patterneditor, qualeditor, ruleseditor, sourceeditor, titleeditor, validator,
  inifile, project, projectbuilder, projectsetup, projectmapdata, projectframe,
  projectloader, projectsummary, projectviewer, properties, mapcoords, webmap,
  mapframe, maphiliter, maplabel, mapoptions, maprenderer, webmapfinder, map,
  filemenu, welcome, reportframe, statusrpt, sysflowrpt, timeseriesrpt,
  pumpingrpt, networkrpt, mapgeoref, calibrationrpt, energycalc, energyrpt,
  dxfimporter, dxfloader, dxfviewer, shpimporter, shploader, shpviewer,
  maplocater, mapquery, webmapserver, rangerpt, menuframe, basemapmenu,
  mapexporter, projtransform, curveselector, patternselector, curveviewer,
  tseriesselector, hydprofilerpt, msxfileprocs, groupeditor, profileselector,
  controledit, ruleedit, statusframe, themerange;

{$R *.res}

begin
  //SetHeapTraceOutput('Trace.log');
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Title:='EPANET';
  Application.Initialize;
  Application.CreateForm(TmainForm, mainForm);
  Application.CreateForm(TFileMenuForm, FileMenuForm);
  Application.Run;
end.

