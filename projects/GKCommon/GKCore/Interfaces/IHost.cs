/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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

using GKCore.Types;

namespace GKCore.Interfaces
{
    public interface IMainWindow : IWindow
    {
        void Restore();
        void UpdateControls(bool forceDeactivate);
    }

    public interface IHost
    {
        IBaseWindow GetCurrentFile(bool extMode = false);
        IWorkWindow GetWorkWindow();

        string GetUserFilesPath(string filePath);
        IBaseWindow CreateBase(string fileName);
        void LoadBase(IBaseWindow baseWin, string fileName);
        IBaseWindow FindBase(string fileName);
        void BaseChanged(IBaseWindow baseWin);
        void BaseClosed(IBaseWindow baseWin);
        void BaseRenamed(IBaseWindow baseWin, string oldName, string newName);
        void NotifyRecord(IBaseWindow baseWin, object record, RecordAction action);

        void ApplyOptions();
        string GetAppDataPath();

        bool IsWidgetActive(IWidget widget);
        void WidgetShow(IWidget widget);
        void WidgetClose(IWidget widget);

        void ShowWindow(IWindow window);

        ILangMan CreateLangMan(object sender);
        void UpdateControls(bool forceDeactivate);
        void ShowHelpTopic(string topic);
        void EnableWindow(IWidgetForm form, bool value);

        bool ShowModalX(ICommonDialog form, bool keepModeless);
    }
}
