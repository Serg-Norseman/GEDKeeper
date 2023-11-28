
# Solutions

## Disabled and removed features

1. Plugins -> Monolith
2. Themes
3. LayoutWindows
4. Functions:
  - File\Close - canceled
  - Edit\(quick) Search - popup panel of basewin below infopanel
  - Reports and Plugins - integrate to monolith application
  - View error log - canceled
  - Send error log - ?
5. Scripts
6. No extended notes

## Windows and dialogs

1. LanguageSelectDlg (locales) - canceled, OptionsDlg only
2. ProgressDlg - canceled, only popup progressbar at top of pages
3. ScriptEditWin - canceled
4. NoteEditDlgEx - canceled
5. LanguageEditDlg (from FilePropertiesDlg) - canceled


# Experimental

1. https://www.nuget.org/packages/C1.Xamarin.Forms.Grid
2. https://www.nuget.org/packages/Syncfusion.Xamarin.SfDataGrid

## Canceled

1. DLToolkit.Forms.Controls and Xamvvm.Forms - obsolete, on Android - ugly

## Checked packages

Xamarin.Forms 5.0.0.2291-2545
Xamarin.Essentials 1.7.0-3 -> TargetFrameworkVersion=v10.0, android:targetSdkVersion="29"
Xamarin.Forms.InputKit 3.7.2, 4.1.6
Xamarin.Forms.DataGrid 4.8.0
  - 4.8.0 - selecting items does not work correctly after scrolling
  - >= 5.0.0.2515 - dont works
Xamarin.CommunityToolkit 2.0.6

# Dev Requirements

## Checked configuration (Android emulator)

Project: Android 10.0 Q (API 29), MinAndVer 5.0 (API 21), MaxAndVer 8.1 (API 27)

Emulator: sc query intelhaxm

Android Device Image: Tablet M 10.1in (Android 9.0 - API 28), 1 gb
  Start application: 512 mb - 122 s; 1gb - 8-15 s; 2 gb - 7-8 s
  Load gedcom (370 kb): 512 mb - 74 s; 1gb - 2 s; 2 gb - <1 s

## Checked configurations (Android real devices)

ASUS Nexus 7, Android 6.0.1 (API 23), 2015
Samsung Galaxy A32, Android 13 (API 33), 2022

# Incomprehensible problems

- How to control scrolling and size of SkCanvasView in ScrollView (Charts); centering in container?
- DataGrid unused space background color black not turning off.
