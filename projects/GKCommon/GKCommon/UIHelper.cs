/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
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
using System.Windows.Forms;

namespace GKCommon
{
    /// <summary>
    /// Description of UIHelper.
    /// </summary>
    public class UIHelper
    {
        public UIHelper()
        {
        }

        public static string GetOpenFile(string title, string context, string filter, int filterIndex, string defaultExt)
        {
            using (OpenFileDialog ofd = CreateOpenFileDialog(title, context, filter, filterIndex, defaultExt, false)) {
                if (ofd.ShowDialog() == DialogResult.OK) {
                    return ofd.FileName;
                } else {
                    return string.Empty;
                }
            }
        }

        public static OpenFileDialog CreateOpenFileDialog(string title, string context, string filter, int filterIndex, string defaultExt,
                                                          bool multiSelect)
        {
            OpenFileDialog ofd = new OpenFileDialog();
            
            if (!string.IsNullOrEmpty(title))
                ofd.Title = title;

            if (!string.IsNullOrEmpty(context))
                ofd.InitialDirectory = context;

            if (!string.IsNullOrEmpty(filter))
            {
                ofd.Filter = filter;

                if (filterIndex > 0) ofd.FilterIndex = filterIndex;
            }

            if (!string.IsNullOrEmpty(defaultExt))
                ofd.DefaultExt = defaultExt;

            ofd.Multiselect = multiSelect;

            return ofd;
        }

        public static string GetSaveFile(string title, string context, string filter, int filterIndex, string defaultExt,
                                         string suggestedFileName, bool overwritePrompt = true)
        {
            using (SaveFileDialog sfd = CreateSaveFileDialog(title, context, filter, filterIndex, defaultExt, suggestedFileName)) {
                if (sfd.ShowDialog() == DialogResult.OK) {
                    return sfd.FileName;
                } else {
                    return string.Empty;
                }
            }
        }

        public static SaveFileDialog CreateSaveFileDialog(string title, string context, string filter, int filterIndex, string defaultExt,
                                                          string suggestedFileName)
        {
            SaveFileDialog sfd = new SaveFileDialog();

            if (!string.IsNullOrEmpty(title))
                sfd.Title = title;

            if (!string.IsNullOrEmpty(context))
                sfd.InitialDirectory = context;

            if (!string.IsNullOrEmpty(filter))
            {
                sfd.Filter = filter;

                if (filterIndex > 0) sfd.FilterIndex = filterIndex;
            }

            if (!string.IsNullOrEmpty(defaultExt))
                sfd.DefaultExt = defaultExt;

            if (!string.IsNullOrEmpty(suggestedFileName))
                sfd.FileName = suggestedFileName;

            return sfd;
        }
    }
}
