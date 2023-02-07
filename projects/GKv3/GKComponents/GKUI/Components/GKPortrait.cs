/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2017-2021 by Sergey V. Zhdanovskih, Igor Tyulyakov.
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
using Eto.Drawing;
using Eto.Forms;
using GKCore.Design.Controls;

namespace GKUI.Components
{
    using EFPictureBox = Eto.Forms.ImageView;

    /// <summary>
    /// Image with the pop-up panel.
    /// </summary>
    public class GKPortrait : Panel, IPortraitControl
    {
        private readonly List<Button> fBtnsList;
        private int fPixelSpeed;


        public Image Image
        {
            get { return fImageBox.Image; }
            set { fImageBox.Image = value; }
        }


        public GKPortrait()
        {
            InitializeComponent();

            fBtnsList = new List<Button>();

            //fImageBox.SizeMode = PictureBoxSizeMode.CenterImage;
            fImageBox.Cursor = Cursors.Arrow;

            fPixelSpeed = 5;

            //fSlidePanel.Height = 36;
            //fSlidePanel.Cursor = Cursors.Arrow;
            //fSlidePanel.Top = Height;
            //fLayout.Move(fSlidePanel, 0, Height);

            //fTimer = AppHost.Instance.CreateTimer(100.0f, MoveSlidePanel);
            //fTimer.Stop();
        }

        public void Activate()
        {
            Focus();
        }

        public void AddButton(Button b)
        {
            fBtnsList.Add(b);
            RedrawButtons();
        }

        private void RedrawButtons()
        {
            /*int lenwagon = 0;

            fSlidePanel.Controls.Clear();

            for (int i = 0, c = fBtnsList.Count; i < c; i++) {
                lenwagon += (i > 0) ? (8 + fBtnsList[i].Width) : fBtnsList[i].Width;
            }

            int center = lenwagon / 2;
            int startPosition = fSlidePanel.Width / 2 - center;

            for (int i = 0, c = fBtnsList.Count; i < c; i++) {
                int heightCenter = fSlidePanel.Height / 2;
                int btnCenter = fBtnsList[i].Height / 2;

                fBtnsList[i].Location = new Point(startPosition, heightCenter - btnCenter);
                fSlidePanel.Controls.Add(fBtnsList[i]);
                startPosition += fBtnsList[i].Width + 8;
            }*/
        }

        private void MoveSlidePanel(object sender, EventArgs e)
        {
            /*if (fSlidePanel.Top <= Height - fSlidePanel.Height) {
                fTimer.Stop();
            } else {
                fSlidePanel.Top -= (fSlidePanel.Top - 5 > Height - fSlidePanel.Height) ? fPixelSpeed : fSlidePanel.Top - (Height - fSlidePanel.Height);
            }*/
        }

        private void CheckCursorPosition(object sender, EventArgs e)
        {
            /*Point p = PointToClient(Cursor.Position);
            bool buf = (p.X <= 1 || p.Y <= 1 || p.X >= fImageBox.Width || p.Y >= fImageBox.Height - 1);
            if (!buf) {
                fTimer.Start();
                fTimer.Interval = 1;
            } else {
                fSlidePanel.Top = Height;
                fTimer.Stop();
            }*/
        }

        protected override void OnSizeChanged(EventArgs e)
        {
            base.OnSizeChanged(e);

            //fSlidePanel.Width = Width;
            //CheckCursorPosition(this, e);
        }

        #region Design

        private EFPictureBox fImageBox;
        //private PixelLayout fLayout;
        //private Panel fSlidePanel;
        //private ITimer fTimer;

        private void InitializeComponent()
        {
            SuspendLayout();
            //fLayout = new PixelLayout();

            fImageBox = new EFPictureBox();
            //fImageBox.MouseLeave += CheckCursorPosition;
            //fImageBox.MouseMove += CheckCursorPosition;

            //fSlidePanel = new Panel();
            //fSlidePanel.BackgroundColor = SystemColors.ButtonShadow;
            //fSlidePanel.Location = new Point(0, 152);
            //fSlidePanel.MouseLeave += CheckCursorPosition;
            //fSlidePanel.MouseMove += CheckCursorPosition;

            //fLayout.Add(fImageBox, 0, 0);
            //fLayout.Add(fSlidePanel, 0, Height);
            Content = fImageBox; //fLayout;
            ResumeLayout();
        }

        #endregion
    }
}
