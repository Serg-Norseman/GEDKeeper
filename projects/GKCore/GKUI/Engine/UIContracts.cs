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

using System;
using System.Drawing;
using System.Reflection;
using System.Windows.Forms;

using GKCommon;
using GKCommon.Controls;
using GKCommon.GEDCOM;

namespace GKUI.Engine
{
    /// <summary>
    /// The interface of the class for working with WinForms dialogs.
    /// </summary>
    public interface IStdDialogs
    {
        Font SelectFont(Font font);
        string GetOpenFile(string title, string context, string filter,
                           int filterIndex, string defaultExt);
        string GetSaveFile(string filter);
        string GetSaveFile(string title, string context, string filter, int filterIndex, string defaultExt,
                           string suggestedFileName, bool overwritePrompt = true);

        void ShowMessage(string msg);
        void ShowError(string msg);
        bool ShowQuestionYN(string msg);
        void ShowWarning(string msg);

        bool GetInput(string prompt, ref string value);
        bool GetPassword(string prompt, ref string value);
    }


    /// <summary>
    /// 
    /// </summary>
    public interface IUIHelper
    {
        // TODO: necessary to exclude these references to WinForms controls!
        void SelectComboItem(ComboBox comboBox, object tag, bool allowDefault);
        void SelectComboItem(ListBox listBox, object tag, bool allowDefault);

        void NormalizeFormRect(ref ExtRect winRect);
        ExtRect GetFormRect(Form form);
        void RestoreFormRect(Form form, ExtRect rt, FormWindowState winState);
        void CenterFormByParent(Form form, IntPtr parent);
        GKListView CreateRecordsView(Control parent, GEDCOMTree tree, GEDCOMRecordType recType);
    }


    /// <summary>
    /// 
    /// </summary>
    public interface IBaseController
    {
    }


    /// <summary>
    /// 
    /// </summary>
    public interface IPathReplacer
    {
        void Load(string fileName);
        bool TryReplacePath(string path, out string newPath);
    }


    /// <summary>
    /// 
    /// </summary>
    public interface IUtilities
    {
        Assembly GetExecutingAssembly();
        Version GetAppVersion();
    }
}
