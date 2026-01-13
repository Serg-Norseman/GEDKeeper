/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Threading.Tasks;
using GDModel;
using GKCore.Design;
using GKCore.Filters;
using GKCore.Lists;
using GKCore.Locales;
using GKCore.Options;
using GKCore.Plugins;

namespace GKCore
{
    public enum Feature
    {
        GridCellFormat,
        InternetProxy,
        MediaPlayer,
        RecentFilesLoad,
        Themes,
        OverwritePrompt,
        EmbeddedLocales,
        Mobile,             // -> SingleBase UI, AppData/Local instead of AppData/Roaming, no Plugins
        PrintPreview,
        Graphics,
    }


    public interface IHost
    {
        void Activate();
        IForm GetActiveForm();
        IBaseWindow GetCurrentFile(bool extMode = false);
        IWorkWindow GetWorkWindow();
        void CloseDependentWindows(IWindow owner);

        string GetAppDataPath();
        string GetUserFilesPath(string filePath);

        Task<IBaseWindow> CreateBase(string fileName);
        Task LoadBase(IBaseWindow baseWin, string fileName);
        IBaseWindow FindBase(string fileName);
        void BaseChanged(IBaseWindow baseWin);
        void BaseClosed(IBaseWindow baseWin);
        void BaseRenamed(IBaseWindow baseWin, string oldName, string newName);
        void BaseSaved(IBaseWindow baseWin, string fileName);
        void NotifyRecord(IBaseWindow baseWin, object record, RecordAction action);
        void NotifyFilter(IBaseWindow baseWin, GDMRecordType recType, IListSource listSource, ListFilter filter);
        void SelectedIndexChanged(IBaseWindow baseWin);
        void TabChanged(IBaseWindow baseWin);

        void ApplyOptions();

        bool IsWidgetActive(IWidgetPlugin widget);
        void WidgetShow(IWidgetPlugin widget);
        void WidgetClose(IWidgetPlugin widget);

        void EnableWindow(IWidgetForm form, bool value);
        MRUFile GetMRUFile(IBaseWindow baseWin);
        Task<bool> ShowModalAsync(ICommonDialog dialog, IView owner, bool keepModeless = false);
        void ShowWindow(IWindow window);

        ILangMan CreateLangMan(object sender);
        void UpdateControls(bool forceDeactivate, bool blockDependent = false);
        void ShowHelpTopic(string topic);

        string SelectFolder(string folderPath);

        bool HasFeatureSupport(Feature feature);
    }
}
