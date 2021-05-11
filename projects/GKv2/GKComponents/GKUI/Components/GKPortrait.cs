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
using System.Drawing;
using System.Windows.Forms;
using GKCore.MVP.Controls;

namespace GKUI.Components
{
    /// <summary>
    /// Image with the pop-up panel.
    /// </summary>
    public class GKPortrait : UserControl, IPortraitControl
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

            fImageBox.SizeMode = PictureBoxSizeMode.CenterImage;
            fImageBox.Cursor = Cursors.Arrow;

            fPixelSpeed = 5;

            fSlidePanel.Height = 36;
            fSlidePanel.Cursor = Cursors.Arrow;
            fSlidePanel.Top = Height;

            fTimer.Stop();
        }

        public void Activate()
        {
            Select();
        }

        public void AddButton(Button b)
        {
            fBtnsList.Add(b);
            RedrawButtons();
        }

        private void RedrawButtons()
        {
            int lenwagon = 0;

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
            }
        }

        private void MoveSlidePanel(object sender, EventArgs e)
        {
            if (fSlidePanel.Top <= Height - fSlidePanel.Height) {
                fTimer.Stop();
            } else {
                fSlidePanel.Top -= (fSlidePanel.Top - 5 > Height - fSlidePanel.Height) ? fPixelSpeed : fSlidePanel.Top - (Height - fSlidePanel.Height);
            }
        }

        private void CheckCursorPosition(object sender, EventArgs e)
        {
            Point p = PointToClient(Cursor.Position);
            bool buf = (p.X <= 1 || p.Y <= 1 || p.X >= fImageBox.Width || p.Y >= fImageBox.Height - 1);
            if (!buf) {
                fTimer.Start();
                fTimer.Interval = 1;
            } else {
                fSlidePanel.Top = Height;
                fTimer.Stop();
            }
        }

        protected override void OnResize(EventArgs e)
        {
            base.OnResize(e);

            fSlidePanel.Width = Width;
            CheckCursorPosition(this, e);
        }

        #region Design

        private System.ComponentModel.IContainer components = null;
        private PictureBox fImageBox;
        private Panel fSlidePanel;
        private Timer fTimer;

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                if (components != null) {
                    components.Dispose();
                }
            }
            base.Dispose(disposing);
        }

        private void InitializeComponent()
        {
            components = new System.ComponentModel.Container();
            fTimer = new Timer(components);
            fTimer.Tick += MoveSlidePanel;

            SuspendLayout();

            fImageBox = new PictureBox();
            fImageBox.BackgroundImageLayout = ImageLayout.Center;
            fImageBox.Dock = DockStyle.Fill;
            fImageBox.Location = new Point(0, 0);
            fImageBox.Size = new Size(178, 188);
            fImageBox.MouseLeave += CheckCursorPosition;
            fImageBox.MouseHover += CheckCursorPosition;

            fSlidePanel = new Panel();
            fSlidePanel.BackColor = SystemColors.ButtonShadow;
            fSlidePanel.Location = new Point(0, 152);
            fSlidePanel.Size = new Size(178, 36);
            fSlidePanel.MouseLeave += CheckCursorPosition;
            fSlidePanel.MouseHover += CheckCursorPosition;

            Controls.Add(fSlidePanel);
            Controls.Add(fImageBox);
            Name = "GKPortrait";
            Size = new Size(178, 188);
            ResumeLayout(false);
        }

        #endregion
    }
}
