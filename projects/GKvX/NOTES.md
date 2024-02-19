
# Solutions

## Disabled and removed features

1. Plugins -> Monolith
2. Themes
3. LayoutWindows
4. Functions:
  - File\Close - canceled
  - Edit\(quick) Search - popup panel of basewin below infopanel
  - Reports and Plugins - integrate to monolith application
  - View error log - ?
  - Send error log - canceled
5. Scripts
6. No extended notes
7. FindAndReplace - excluded because it is unsafe if the found/modified record cannot be seen

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

## Supported screen densities

ScrDens  |   DPI   | Factor | Base | Actual | Support |
ldpi     |   120   |  0.75X |  24  |   18   |    +    |
mdpi     |   160   |   1X   |  24  |   24   |    +    |
hdpi     |   240   |  1.5X  |  24  |   36   |    +    |
x-hdpi   |   320   |   2X   |  24  |   48   |    +    |
xx-hdpi  |   480   |   3X   |  24  |   72   |    +    |
xxx-hdpi |   640   |   4X   |  24  |   96   |         |

* Base size of GK toolbar icons = 20px. For `ldpi` and `mdpi` - good size.

## Checked configurations (Emulator devices)

Phone H-DPI 4in             |  4.0"   (480x800) | +++++++++++++ | Android 5.1   (API 22) |
Tablet M-DPI 10.1in         | 10.1"  (1280x800) | 149 PPI |   l | Android 9.0   (API 28) |
Phone M-DPI 5.4in           |  5.4"   (480x854) | 181 PPI |   m | Android 9.0   (API 28) |
Phone H-DPI 4in             |  4.0"   (480x800) | 233 PPI |   h | Android 9.0   (API 28) |
Phone Xh-DPI 4.7in          |  4.7"  (720x1280) | 312 PPI |  xh | Android 9.0   (API 28) |
Phone Xxh-DPI 4.7in         |  4.7" (1080x2636) | 606 PPI | xxh | Android 9.0   (API 28) |

## Checked configurations (Android real devices)

ASUS Nexus 7                |  7.0" (1920x1200) | 323 PPI |  xh | Android 6.0.1 (API 23) | 2015
Samsung Galaxy J1           |  4.5"   (480x800) | 207 PPI |   m | Android 5.1.1 (API 22) | 2016
Samsung Galaxy S7           |  5.1" (1440x2560) | 576 PPI | xxh | Android 8.0   (API 26) | 2016
Xiaomi Mi 5X                |  5.5" (1080x1920) | 401 PPI |  xh | Android 8.1   (API 27) | 2017
LG Q6a                      |  5.5" (1080x2160) | 439 PPI |  xh | Android 8.1   (API 27) | 2017
Samsung Galaxy Note 10 Lite |  6.7" (1080x2400) | 393 PPI |  xh | Android 13    (API 33) | 2020
ASUS Zenfone 8              |  5.9" (1080x2400) | 446 PPI |  xh | Android 12    (API 31) | 2021
Samsung Galaxy A32          |  6.6" (1080x2340) | 390 PPI |  xh | Android 13    (API 33) | 2022
Google Pixel 6a             |  6.1" (1080x2400) | 431 PPI |  xh | Android 14    (API 34) | 2022
Xiaomi 13T                  |  6.7" (2712x1220) | 446 ppi |  xh | Android 14    (API 34) | 2023

# Incomprehensible problems

- How to control scrolling and size of SkCanvasView in ScrollView (Charts); centering in container?
- DataGrid unused space background color black not turning off.
- When switching between tabs multiple times, nested tabs disappear (OptionsDlg).
