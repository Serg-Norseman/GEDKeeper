/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Text;
using BSLib;

namespace GKUI.Components
{
    public static class UIHelper
    {

        public static string[] Convert(string text)
        {
            var strList = new StringList(text);
            return strList.ToArray();
        }

        public static string Convert(string[] lines)
        {
            StringBuilder strBuilder = new StringBuilder();
            foreach (var line in lines) {
                if (strBuilder.Length > 0) {
                    strBuilder.Append(Environment.NewLine);
                }
                strBuilder.Append(line);
            }
            return strBuilder.ToString();
        }
    }
}
