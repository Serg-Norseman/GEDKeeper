/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Text.RegularExpressions;
using BSLib;
using GKCore.Design.Controls;
using Terminal.Gui.Views;

namespace GKUI.Components
{
    /// <summary>
    /// 
    /// </summary>
    public class HyperView : TextView, IHyperView
    {
        private readonly StringList fLines;

        public new StringList Lines
        {
            get { return fLines; }
        }

        public HyperView()
        {
            //base.ReadOnly = true;
            base.WordWrap = true;

            fLines = new StringList();
            fLines.OnChange += LinesChange;
        }

        private void LinesChange(object sender)
        {
            var text = Regex.Replace(fLines.Text, @"\[.*?\]", string.Empty);
            base.Text = text;
        }

        public void Activate()
        {
        }
    }
}
