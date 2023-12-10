/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
 *
 *  This file is part of "GEDKeeper".
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

using System.Threading.Tasks;
using GDModel;
using GKCore.Design;
using GKCore.Options;
using GKCore.Types;

namespace GKCore.Interfaces
{
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
        void NotifyFilter(IBaseWindow baseWin, GDMRecordType recType, IListSource listSource, IListFilter filter);
        void SelectedIndexChanged(IBaseWindow baseWin);
        void TabChanged(IBaseWindow baseWin);

        void ApplyOptions();

        bool IsWidgetActive(IWidget widget);
        void WidgetShow(IWidget widget);
        void WidgetClose(IWidget widget);

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
