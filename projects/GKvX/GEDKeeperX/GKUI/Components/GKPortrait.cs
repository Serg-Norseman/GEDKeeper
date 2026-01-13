/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Collections.Generic;
using GKCore.Design.Controls;
using SkiaSharp.Views.Forms;
using Xamarin.Forms;

namespace GKUI.Components
{
    public class GKPortrait : ContentView, IPortraitControl
    {
        private readonly List<Button> fBtnsList;

        public bool Enabled
        {
            get { return base.IsEnabled; }
            set { base.IsEnabled = value; }
        }

        public bool Visible
        {
            get { return base.IsVisible; }
            set { base.IsVisible = value; }
        }


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
