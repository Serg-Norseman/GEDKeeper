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

using System;
using System.Collections.Generic;
using GKCore.Design.Controls;
using SkiaSharp.Views.Forms;
using Xamarin.Forms;

namespace GKUI.Components
{
    public class GKPortrait : ContentView, IPortraitControl
    {
        private readonly List<Button> fBtnsList;

        public bool Enabled { get; set; }
        int IPortraitControl.Height { get; set; }
        int IPortraitControl.Width { get; set; }


        public SKImageImageSource Image
        {
            get { return fImageBox.Image; }
            set {
                if (fImageBox.Image != null) {
                    //fImageBox.Source.Dispose();
                }
                fImageBox.Image = value;
            }
        }


        public GKPortrait()
        {
            InitializeComponent();

            fBtnsList = new List<Button>();
        }

        public void Activate()
        {
        }

        public void AddButton(Button b)
        {
            fBtnsList.Add(b);
            RedrawButtons();
        }

        private void RedrawButtons()
        {
        }

        #region Design

        private ImageBox fImageBox;

        private void InitializeComponent()
        {
            fImageBox = new ImageBox();
            fImageBox.AllowZoom = false;
            fImageBox.AutoPan = false;
            fImageBox.SelectionMode = ImageBoxSelectionMode.None;
            fImageBox.SizeToFit = true;
            Content = fImageBox;
        }

        #endregion
    }
}
