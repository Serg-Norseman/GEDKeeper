/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2018-2023 by Sergey V. Zhdanovskih.
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

using BSLib;
using GKCore.Design.Controls;
using Xamarin.Forms;

namespace GKUI.Components
{
    public delegate void LinkEventHandler(object sender, string linkName);

    /// <summary>
    ///
    /// </summary>
    public partial class HyperView : ContentView, IHyperView
    {
        private readonly StringList fLines;

        private bool fWordWrap;

        public event LinkEventHandler OnLink;

        public bool Enabled
        {
            get { return base.IsEnabled; }
            set { base.IsEnabled = value; }
        }

        public StringList Lines
        {
            get { return fLines; }
        }

        public bool WordWrap
        {
            get { return fWordWrap; }
            set { fWordWrap = value; }
        }


        public HyperView()
        {
            InitializeComponent();

            fLines = new StringList();
            fLines.OnChange += LinesChanged;
            fWordWrap = true;
        }

        public void Activate()
        {
            try {
                Focus();
            } catch {
                // why is an exception thrown here?
            }
        }

        private void LinesChanged(object sender)
        {
            //UpdateScrollPosition(0, 0);
            ArrangeText();
        }

        private void ArrangeText()
        {
            hvContent.Text = fLines.Text;
        }
    }
}
